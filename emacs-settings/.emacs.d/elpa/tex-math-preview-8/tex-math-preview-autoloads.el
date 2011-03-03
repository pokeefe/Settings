;;; tex-math-preview-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tex-math-preview tex-math-preview) "tex-math-preview"
;;;;;;  "tex-math-preview.el" (19819 61105))
;;; Generated autoloads from tex-math-preview.el

(let ((loads (get 'tex-math-preview 'custom-loads))) (if (member '"tex-math-preview" loads) nil (put 'tex-math-preview 'custom-loads (cons '"tex-math-preview" loads))))

(autoload 'tex-math-preview "tex-math-preview" "\
Preview a TeX mathematical expression at or surrounding point.
The expressions recognised are

    $...$ or $$...$$              TeX
    \\(...\\) or \\=\\[...\\]            LaTeX
    \\begin{math}...\\end{math}     LaTeX
    \\begin{displaymath}...        LaTeX
    @math{...}                    Texinfo (plain TeX)
    <math>...</math>              Wikipedia (plain TeX)
    <alt role=\"tex\">...</alt>     DBTexMath

$...$ forms are plain tex by default, or latex if the buffer is
in `latex-mode'.  DBTexMath is plain tex by default, or latex if
it contains \\(...\\) or \\=\\[...\\].  For latex any
\\usepackage{} directives are included, for possible math
extensions.  `tex-math-preview-function' controls the viewing
method.

\"$\" is both a start and end for TeX forms, making it slightly
ambiguous.  `tex-math-preview' assumes point is inside the
expression, so when just after a \"$\" then that's the start, or
when just before then that's the end.  If point is in between two
\"$$\" then that's considered a start.

For more on the respective formats see

    URL `http://www.latex-project.org/'
    Info node `(texinfo)math'
    URL `http://meta.wikimedia.org/wiki/Help:Displaying_a_formula'
    URL `http://ricardo.ecn.wfu.edu/~cottrell/dbtexmath/'

The tex-math-preview.el home page is
URL `http://user42.tuxfamily.org/tex-math-preview/index.html'

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("tex-math-preview-pkg.el") (19819 61105
;;;;;;  393353))

;;;***

(provide 'tex-math-preview-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tex-math-preview-autoloads.el ends here
