templater
=========

The aim of this very small R library
is to enable a workflow where a `.Rmd` template
is written with externally specified inputs, 
and processed many times, with different inputs, to create different reports.

There is a need for this because of several outstanding issues having to do with cache and figure handling of `knitr`:

- `rmarkdown::render` [confounds](https://github.com/rstudio/rmarkdown/issues/499) different parallel instances compiling the same .Rmd file, with no workaround.  
    (This results in **wrong reports**, silently.)
- `knitr::knit` similarly by default can't deal with [chunks that have the same name](https://github.com/yihui/knitr/issues/875) in multiple documents.
    (Hope you weren't re-using code?)


Furthermore, the template modifies a few non-default behaviours of `pandoc`
in ways that I find more useful:

- The file `macros.tex`, if present, will automatically be used for [LaTeX macro definitions](https://github.com/jgm/pandoc/issues/2382).

- MathJax, which turns LaTeX into math in pandoc's output, is *not* included in pandoc's output, [even with `--self-contained`](https://github.com/jgm/pandoc/issues/682),
    because it is very big. But, what if you want to view your files without being connected to the internet?
    This produces a file that will use a local copy of MathJax, if present, and fall back on the CDN (i.e., the internet).

