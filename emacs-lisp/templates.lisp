fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

latex-mode

(begin "\\begin{" (s env) "}" > n> r> "\\end{" (s env) "}")
(frac "\\frac{" p "}{" p "}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(let "(let (" p ")" n> r> ")")
(star "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive)" n> r> ")")

text-mode

(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))

rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

java-mode

(class "public class " (p (file-name-base (or (buffer-file-name) (buffer-name)))) " {" n> r> n "}")

c-mode :condition (re-search-backward "^\\w*$" (line-beginning-position) 'noerror)

(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")

org-mode

(pnl "p.n.l.")

(title "#+title: " p n "#+author: Daniel Mendler" n "#+language: en" n n)
(quote "#+begin_quote" n> r> n> "#+end_quote")
(example "#+begin_example" n> r> n> "#+end_example")
(center "#+begin_center" n> r> n> "#+end_center")
(comment "#+begin_comment" n> r> n> "#+end_comment")
(verse "#+begin_verse" n> r> n> "#+end_verse")
(src "#+begin_src " p n> r> n> "#+end_src")
(elisp "#+begin_src emacs-lisp" n> r> n "#+end_src"
       :post (progn (tempel-done) (org-edit-src-code)))

haskell-mode

(case
    "case " (p "x") " of " n>
    (p "Data") " -> " (p "undefined") n>
    (p "Data") " -> " (p "undefined"))
({-} "{- " p " -}")
(=> (p "Class") " " (p "m") " => ")
(idata "data " (p "Type" ndata) " = " (s ndata) " " (p "Int") n>
       "deriving (" (p "Show, Eq") ")")
(newtype "newtype " (p "Type" ndata) " = " (s ndata) " " (p "Int") n>
         "deriving (" (p "Show, Eq") ")")
(data "data " (p "Type" ndata) " = " (s ndata)  n>
      "{ " (p "field") " :: " (p "Type") n>
      ", " (p "field") " :: " (p "Type") (p (ignore (setq tempel-retpoint (point)) ) retpoint t) n>
      "}" n>
      "deriving (" (p "Show, Eq") ")")
(fn (p "f" fname) " :: " (p "a") " -> " (p "b") n
    (s fname) " " (p "x") " = " (p "undefined"))
(fnc (p "f" fname) " :: " (p "a") " -> " (p "b") n
     (s fname) " " (p "pattern") " = " (p "undefined") n
     (s fname) " " (p "pattern") " = " (p "undefined"))
; guarded fn
(<- (p "x") " <- " (p "undefined"))
