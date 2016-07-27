templater
=========

The aim of this very small R library (ok, it's got one function)
is to enable a workflow where a `.Rmd` template
is written with externally specified inputs, 
and processed many times, with different inputs, to create different reports.

## How?

There are two basic use cases, that differ in whether you write your template 
with file locations relative to where your template is,
or where the output file will be.
Both of the following examples are in the [test/](test/) directory.

### Use case (1): relative to template

Suppose we want to compile a template programatically many times,
while varying a parameter that is pre-specified.
For instance, if `example_cor.Rmd` contains:
````
A correlation with `r n` points:
```{r}
x <- rnorm(n)
y <- x/2+rnorm(n)
plot( x, y )
abline(coef(lm(y~x)))
```
````
To do this, `n` must be specified beforehand;
we could produce ten such output files like so:
```
for (n in 10*(1:10)) {
    render_template("example_cor.Rmd", output=paste0("cor_",n,".html"))
}
```
This example isn't using the fact that compilation occurs in the directory of the template;
but we could add external resources to that location, for instance.

### Use case (2): relative to output

Suppose we have subdirectories `a/` and `b/`, with files named `x` and `y` in each, and these files have numbers in them;
we want to display those numbers in each subdirectory.
Therefore, we want the template to look for the *same* files relative to its *output* location;
this is achieved with the option `change.rootdir=TRUE`.
Here is an example template, `example_chdir.Rmd`:
````
```{r}
x <- scan('x')
y <- scan('y')
```
In this directory, $x=`r x`$ and $y=`r y`$, so $x+y=`r x+y`$.
````
This would be rendered as:
```
render_template("example_chdir.Rmd",output="a/sum.html",change.rootdir=TRUE)
render_template("example_chdir.Rmd",output="b/sum.html",change.rootdir=TRUE)
```


## Why?

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


## Notes

- When debugging it's useful to evaluate the code in your Rmarkdown file in the global environment, so you can play around with the result.
    To do this, just pass in the argument `envir=globalenv()`.

- If you don't have `pandoc` available where you're running this, you can create a "dummy" pandoc (an executable that does nothing),
    make sure to pass in `clean=FALSE`, then copy the markdown file and the `figure/` directory somewhere else later
    to run pandoc on (just call `render_template()` on the `.md` file).
