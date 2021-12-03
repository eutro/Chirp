;; -*- lexical-binding: t; -*-

(require 'seq)

(defgroup chirp nil
  "Major mode for the Chirp programming language."
  :prefix "chirp-"
  :group 'languages)

(defvar chirp-indent-depth 2
  "The depth of each level of indentation in Chirp.")

;;;###autoload
(define-derived-mode chirp-mode prog-mode "Chirp"
  "Major mode for Chirp."
  (setq font-lock-defaults '(chirp-mode-font-lock-keywords))
  (setq-local comment-start "//")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-multi-line t)
  (setq-local indent-line-function #'chirp-indent-line)
  (setq-local electric-indent-chars '(?= ?: ?\{ ?\( ?\) ?\} ?\n))
  (local-set-key (kbd "C-c \\") #'chirp-insert-lambda)
  (set-syntax-table chirp-mode-syntax-table))

(defconst chirp-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol syntax, any ASCII character
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    (modify-syntax-entry ?\_ "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Comments
    (modify-syntax-entry ?\/ ". 124b" table)
    (modify-syntax-entry ?\# ". 1b" table)
    (modify-syntax-entry ?\! ". 2b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\* ". 23" table)

    ;; Punctuation
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\% "." table)
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\x5e "." table) ;; ^
    (modify-syntax-entry ?\? "." table)
    (modify-syntax-entry ?\: "." table)
    (modify-syntax-entry ?\~ "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\$ "." table)

    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    table))

(defun chirp-region-tokens-to-regex ()
  "Take the active region and convert it to a regular expression."
  (interactive)
  (let* ((bounds (car (region-bounds)))
         (lo (car bounds))
         (hi (cdr bounds))
         (text (buffer-substring-no-properties lo hi)))
    (insert (prin1-to-string (chirp-token-strings-to-regex text)))))

(defun chirp-token-strings-to-regex (text)
  "Convert TEXT from tokens.txt to a regular expression."
  (with-temp-buffer
    (save-excursion
      (insert text))
    (let (exprs
          raw-exprs)
      (while (re-search-forward
              "\\(.+\\) ::= \\(/\\(.+\\)/\\|'\\(.+\\)'\\)"
              nil t)
        (let ((raw (match-string 4)))
          (if raw
              (push raw raw-exprs)
            (let ((re (match-string 3)))
              (push (chirp--convert-regexp re) exprs)))))
      (list raw-exprs exprs))))

(defun chirp--convert-regexp (re)
  "Format RE from tokens.txt for use with Emacs' regexp."
  (seq-reduce
   (lambda (re rep)
     (replace-regexp-in-string (car rep) (cdr rep) re))
   '(("(" . "\\\\(?:") (")" . "\\\\)")
     ("\\t" . "\t") ("\\v" . "\v") ("\\f" . "\f")
     ("\\r" . "\r") ("\\n" . "\r"))
   re))

(defconst chirp--id-regexp "[a-zA-Z_][a-zA-Z_0-9]*")
(defconst chirp--ignore-regexp
  (concat
   "\\(?:"
   (string-join
    '("[ \t\v\f\r]+"
      "/\\*\\(?:[^*]|\\*[^/]\\)*\\*/"
      "//[^\n]*"
      "[ \t\v\f\r]*\n")
    "\\|")
   "\\)"))
(defconst chirp--maybe-ignore-regexp
  (concat chirp--ignore-regexp "?"))
(defconst chirp--generics-regexp
  "\\(?:<.+?>\\)")

(defconst chirp-mode-font-lock-keywords
  `((,(concat
       "\\b"
       (regexp-opt '("foreign" "fn" "in" "let" "else" "if" "defn"))
       "\\b")
     (0 font-lock-keyword-face))
    ("\\\\\\|λ" (0 font-lock-keyword-face))
    (,(concat
       "\\(" chirp--id-regexp "\\)"
       chirp--maybe-ignore-regexp
       chirp--generics-regexp "?"
       chirp--maybe-ignore-regexp
       "([^)]*)"
       ".*?=")
     (1 font-lock-function-name-face))
    (,(concat
       "\\(" chirp--id-regexp "\\)"
       ".*?=")
     (1 font-lock-variable-name-face))
    (,(concat "\\b\\(?:de\\)?fn" chirp--ignore-regexp "\\(" chirp--id-regexp "\\)")
     (1 font-lock-function-name-face))
    (,(concat
       "\\bin"
       chirp--ignore-regexp
       "\\(" chirp--id-regexp "\\)")
     (1 font-lock-function-name-face))
    (,(concat "\\b" (regexp-opt '("false" "true")) "\\b")
     (0 font-lock-constant-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.crp\\'" . chirp-mode))

(defun chirp--in-string-p ()
  "Check if we are currently in a string."
  (nth 3 (save-excursion (syntax-ppss (point)))))

;;;###autoload
(defun chirp-insert-lambda ()
  "Insert a λ character."
  (interactive)
  (insert "λ"))

(defun chirp-predict-indent ()
  (save-excursion
    (let* ((last-line-indent (save-excursion (forward-line -1) (current-indentation)))
           (offset (- (point) (save-excursion (back-to-indentation) (point)))))
      (back-to-indentation)
      (let* ((should-indent
              (or
               (looking-at-p "[=+/*-]")
               (save-excursion
                 (unless (progn (beginning-of-line) (bobp))
                   (forward-line -1)
                   (end-of-line)
                   (looking-back "[=:({]\\|\\blet")))))
             (let-alignment
              (save-excursion
                (unless (progn (beginning-of-line) (bobp))
                  (forward-line -1)
                  (back-to-indentation)
                  (and (looking-at-p "let ")
                       (not (re-search-forward "\\bin\\b" (save-excursion (end-of-line) (point)) t))))))
             (at-in (looking-at-p "in\\b"))
             (should-unindent (looking-at-p "[)}]")))
        (+ last-line-indent
           (if should-indent chirp-indent-depth 0)
           (if let-alignment 4 0)
           (if at-in (- chirp-indent-depth 4) 0)
           (if should-unindent (- chirp-indent-depth) 0))))))

(defun chirp-indent-line ()
  "Indent the current line as Chirp."
  (interactive)
  (if (chirp--in-string-p) 'noindent
    (let* ((cyclep (memq last-command '(indent-for-tab-command chirp-indent-line)))
           (expected-indent (chirp-predict-indent))
           (offset (- (point) (save-excursion (back-to-indentation) (point))))
           (at-viable
            (memq
             (current-indentation)
             (list
              expected-indent
              (- expected-indent chirp-indent-depth)
              (- expected-indent (* 2 chirp-indent-depth))
              (+ expected-indent chirp-indent-depth)))))
      (cond
       ((and (not cyclep)
             (eq 0 (current-indentation))
             (not (eq expected-indent 0)))
        (indent-line-to expected-indent))
       ((and at-viable (not cyclep)) nil)
       ((not cyclep) (indent-line-to expected-indent))
       (t (chirp-cycle-indent)))
      (back-to-indentation)
      (forward-char (max offset 0)))))

(defun chirp--cycle-indent-internal (expected-indent)
  (let* ((current-indent (current-indentation))
         (diff (- current-indent expected-indent))
         (target-indent
          (cond
           ((eq diff (- chirp-indent-depth)) expected-indent)
           ((eq diff 0) (+ expected-indent chirp-indent-depth))
           ((eq diff chirp-indent-depth) (+ expected-indent (* 2 chirp-indent-depth)))
           ((eq diff (* 2 chirp-indent-depth)) 0)
           ((eq current-indent 0) (- expected-indent chirp-indent-depth))
           (t expected-indent))))
    (indent-line-to target-indent)))

(defun chirp-cycle-indent ()
  (interactive)
  (chirp--cycle-indent-internal (chirp-predict-indent)))

(provide 'chirp-mode)
;;; chirp-mode.el ends here
