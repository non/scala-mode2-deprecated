;; scala2 - a more usable scala-mode
;;
;; by Erik Osheim
;;
;; available under the MIT license

(defvar scala2-hook nil)

(defvar scala2-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for scala2")

(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala2))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala2))

(defvar scala2-font-lock-keywords
  `(
    ; annotation, e.g. "@specialized"
    (,(rx "@" (in "a-zA-Z_") (0+ (in "a-zA-Z0-9_")))
     . font-lock-preprocessor-face)

    ; class/trait/object names, e.g. "object Foo"
    (,(rx symbol-start
          (group (or "class" "trait" "object"))
          (1+ space)
          (group (and (in "a-zA-Z_") (0+ (in "a-zA-Z0-9_")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ; def names, e.g. "def bar"
    (,(rx symbol-start
          (group "def")
          (1+ space)
          (group (and (not (in ":([ "))
                      (0+ (not (in "0-9:([ "))))))
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))

    ; val/var names, e.g. "val xyz"
    (,(rx symbol-start
          (group (or "val" "var"))
          (1+ space)
          (group (and (not (in ":([ "))
                      (0+ (not (in "0-9:([ "))))))
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face))

    ; symbols, e.g. "'lisp"
    (,(rx "'"
          (in "a-zA-Z_")
          (0+ (in "a-zA-Z0-9_")))
     . font-lock-constant-face)

    ; types, e.g. "Qux"
    (,(rx ":"
          (0+ space)
          (group (and (in "a-zA-Z_") (0+ (not (in "()[]{}; \t"))))))
     (1 font-lock-type-face))
    (,(rx ":"
          (1+ space)
          (group (and ":" (0+ (not (in "()[]{}; \t"))))))
     (1 font-lock-type-face))
    (,(rx (group "extends")
          (1+ space)
          (group (1+ (not (in "()[]{}; \t")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))
    (,(rx (group "with")
          (1+ space)
          (group (1+ (not (in "()[]{}; \t")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))
    (,(rx (group "new")
          (1+ space)
          (group (1+ (not (in "()[]{}; \t")))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))
    (,(rx symbol-start
          (in "A-Z")
          (0+ (in "a-zA-Z0-9_")))
     . font-lock-type-face)

    ; keywords, e.g. "this"
    (,(rx symbol-start
          (or "yield" "with" "while" "var" "val" "until" "type" "true" "try"
              "trait" "throw" "to" "this" "super" "sealed" "return" "protected"
              "private" "package" "override" "object" "null" "new" "match"
              "macro" "lazy" "import" "implicit" "if" "forSome" "for" "finally"
              "final" "false" "extends" "else" "do" "def" "class" "catch"
              "case" "abstract")
          symbol-end)
     . font-lock-keyword-face)
    )
  )

(defun scala2-quote-syntax2 ()
  ;; Handle tripple quotes by using generic string delimiters
  ;; leave the first pair of quotes alone and the last pair of 
  ;; quotes alone and mark the middle with generic string delim
  ;; so """ hello """ is parsed something like:
  ;; <open str><close str><open gen delim> hello <close gen delim><open str><close str>
  (save-excursion
    (let ((syntax (save-match-data (syntax-ppss))))
      (if (not (nth 3 syntax))
          (put-text-property (match-beginning 3) (match-end 3) 
                             'syntax-table (string-to-syntax "|"))
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "|"))))))
        
(defvar scala2-syntax-propertize-function
  (syntax-propertize-rules 
   ("\\(\"\\)\\(\"\\)\\(\"\\)"
            (3 (ignore (scala2-quote-syntax2))))))

(defvar scala2-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\} "){" st)
    
    (modify-syntax-entry ?\_ "_" st)
    (modify-syntax-entry ?\. "." st)
       
    (modify-syntax-entry ?/ ". 124nb" st)
    (modify-syntax-entry ?* ". 23n" st)
    (modify-syntax-entry ?\n "> bn" st)
    (modify-syntax-entry ?\r "> bn" st)
    st))

(defun scala2 ()
  "Major mode for scala2"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table scala2-syntax-table)
  (use-local-map scala2-map)

  (set (make-local-variable 'font-lock-defaults) '(scala2-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       scala2-syntax-propertize-function)

  ;;(set (make-local-variable 'indent-line-function) 'scala2-indent-line) 

  (setq major-mode 'scala2
    mode-name "Scala2"
    comment-start "// "
    comment-end ""
  )
  (run-hooks 'scala2-hook))

(provide 'scala2)
