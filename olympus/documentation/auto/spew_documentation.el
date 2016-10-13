(TeX-add-style-hook
 "spew_documentation"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("biblatex" "backend=biber")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "hyperref"
    "graphicx"
    "inputenc"
    "verbatim"
    "dirtree"
    "algorithm2e"
    "amsmath"
    "amsfonts"
    "authblk"
    "centernot"
    "biblatex")
   (LaTeX-add-labels
    "alg:spew"
    "l3"
    "alg::schools"
    "alg::workplaces")
   (LaTeX-add-bibliographies
    "spew_refs"))
 :latex)

