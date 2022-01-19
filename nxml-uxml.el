;;; nxml-uxml.el --- MicroXML support for nXML -*- lexical-binding: t -*-
;;
;;; Author: Daphne Preston-Kendal
;;; URL: https://gitlab.com/dpk/nxml-uxml
;;; Keywords: languages, XML, MicroXML
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; [[https://dvcs.w3.org/hg/microxml/raw-file/tip/spec/microxml.html][MicroXML]] is a delightful tiny subset of XML, removing everything
;; that makes XML a nightmare to deal with in practice. nxml-uxml-mode
;; is a minor mode for Emacs that slightly modifies the XML parser of
;; nxml-mode to forbid most (though, at present, not quite all)
;; constructs which are allowed in XML 1.0 but disallowed in MicroXML.
;;
;;; Code:

;;;###autoload
(define-minor-mode nxml-uxml-mode
  "Modifies the nXML mode parser to reject constructs that are
not allowed in MicroXML."
  ;; TODO: Check the RNC file to make sure it defines a
  ;; MicroXML-compatible schema
  ;;
  ;; TODO: Disallow non-characters and control characters
  ;;
  ;; TODO: Disallow encodings other than UTF-8

  :lighter "µXML"
  (when (and nxml-uxml-mode (not (eq major-mode 'nxml-mode)))
    (nxml-uxml-mode -1)
    (error "nxml-uxml-mode can only be used inside nXML"))
  ;; I haven't found a better way than this to force nXML to recheck
  ;; the whole document for errors:
  (rng-after-change-function (point-min) (point-max) (point-max)))

(defvar-local nxml-uxml-allow-newlines-in-attributes nil
  "Whether to allow newlines in attribute values in MicroXML. The
default is nil out of an abundance of caution, even though
they're technically allowed in MicroXML, because conformant XML
processing tools will treat them differently to conformant
MicroXML parsers.")

(defun uxmltok-disallow-in-prolog (prolog)
  "Disallow the XML prolog."
  (when nxml-uxml-mode
    (with-demoted-errors
        (dolist (item prolog)
          (let ((type (aref item 0))
                (start (aref item 1))
                (end (aref item 2)))
            (cond ((eq type 'xml-declaration)
                   (xmltok-add-error "XML declaration not allowed in MicroXML"
                                     start end))
                  ((eq type 'markup-declaration-open)
                   (if (string= (buffer-substring start end) "<!DOCTYPE")
                       (xmltok-add-error "DOCTYPE declaration not allowed in MicroXML"
                                         start end)
                     (xmltok-add-error "Declaration not allowed in MicroXML"
                                       start end)))
                  ((eq type 'processing-instruction-left)
                   (xmltok-add-error "Processing instruction not allowed in MicroXML"
                                     start end)))))))
  prolog)
(advice-add 'xmltok-forward-prolog :filter-return #'uxmltok-disallow-in-prolog)

(defun uxmltok-disallow-gts (start end)
  "Disallow the raw character > between start and end."
  (when (and start end)
    (save-excursion
      (goto-char start)
      (while (re-search-forward ">" end t)
        (xmltok-add-error "`>' that is not markup must be entered as `&gt;'"
                          (1- (point)) (point))))))

(defun uxmltok-disallow-newlines (start end)
  "Disallow newline characters between start and end."
  (when (and start end)
    (save-excursion
      (goto-char start)
      (end-of-line)
      (while (<= (point) end)
        (xmltok-add-error "Newlines in attribute values are inadvisable in MicroXML" (point) (1+ (point)))
        (forward-line)
        (end-of-line)))))

(defun uxmltok-disallow-in-content (token)
  "Disallow things that aren't allowed in MicroXML in the content
of documents."
  (when nxml-uxml-mode
    (with-demoted-errors
        (cond ((eq xmltok-type 'start-tag)
               (if xmltok-name-colon
                   (xmltok-add-error "Colons are not allowed in element names in MicroXML"
                                     xmltok-start xmltok-name-end))
               (dolist (attr (append xmltok-attributes xmltok-namespace-attributes))
                 (cond ((aref attr 1)
                        (xmltok-add-error "Colons are not allowed in attribute names in MicroXML"
                                          (aref attr 0) (aref attr 2)))
                       ((string= (buffer-substring (aref attr 0) (aref attr 2))
                                 "xmlns")
                        (xmltok-add-error "Namespaces are not allowed in MicroXML")))
                 (uxmltok-disallow-gts (aref attr 3) (aref attr 4))
                 (if (not nxml-uxml-allow-newlines-in-attributes)
                     (uxmltok-disallow-newlines (aref attr 3) (aref attr 4)))))
              ((eq xmltok-type 'data)
               (uxmltok-disallow-gts xmltok-start (point)))
              ((eq xmltok-type 'cdata-section)
               (xmltok-add-error "CDATA sections are not allowed in MicroXML"
                                 xmltok-start))
              ((eq xmltok-type 'char-ref)
               (let ((end (point)))
                 (save-excursion
                   (goto-char xmltok-start)
                   (cond ((not (looking-at "&#x"))
                          (xmltok-add-error "Only hexadecimal character references are allowed in MicroXML" xmltok-start end))
                         ((looking-at "&#x0*[dD];")
                          (xmltok-add-error "Character reference to `&#xD;' not allowed in MicroXML" xmltok-start end))))))
              ((eq xmltok-type 'entity-ref)
               ;; in practice this should be caught by the fact doctypes are disallowed, but let's be extra careful
               (let ((end (point)))
                 (save-excursion
                   (goto-char xmltok-start)
                   (if (not (looking-at "&\\(amp\\|lt\\|gt\\|quot\\|apos\\);"))
                       (xmltok-add-error "Entity reference not allowed in MicroXML" xmltok-start end))))))))
  token)
(advice-add 'xmltok-forward :filter-return #'uxmltok-disallow-in-content)
;; (advice-remove 'xmltok-foward #'uxmltok-disallow-in-content)

(provide 'nxml-uxml)

;;; nxml-uxml.el ends here
