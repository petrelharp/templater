
#' Safely Compile a R/markdown Template
#'
#' Calls "knit()" on the template given,
#' directing output to the given output name,
#' and doing this in a way that doesn't cause collisions 
#' between parallel instances of this script.
#'
#' @param template Name of .Rmd file.
#' @param output Name of output .md or .html file.
#' @param html Run pandoc to compile html from the .md?
#' @param md.file Name of the .md output file.
#' @param resource.dir Directory that files referenced in pandoc options are in.
#' @param macros Name of a file with LaTeX macros.
#' @param opts.knit List of additional options for \code{knitr::opts_knit$set()}.
#' @param change.rootdir Whether to evalute the template in the output directory (rather than the template directory).
#' @param verbose Whether to print commands sufficient to recreate what is done.
#' @export
#' @return The name of the output file.
#' Note that by default, both knitr and pandoc look for figures relative to the *current directory*,
#' not the directory that the markdown file lives in.  
#' This makes subsequent parsing of the markdown file difficult.
#' To avoid this, here we switch to the directory of the markdown file before running either.
#'
#' If the name of the output is 'outdir/out.html', then the 'cache' and 'figure' directories
#' for knitr will be 'outdir/cache/out/' and 'outdir/figure/out', respectively.
#' This ensures that separate output files have distinct caches and figures.
#'
#' This executes in the output directory (precisely, the directory where \code{md.file} lives),
#' which is not necessarily in the same directory as the template.
render_template <- function ( template,
                           output=gsub("[.](Rmd|md)$",".html",template),
                           html=grepl("html$",output),
                           md.file=gsub("[.](html|md)$",".md",output),
                           resource.dir=system.file(package="templater"),
                           macros="macros.tex",
                           opts.knit=NULL,
                           change.rootdir=FALSE,
                           verbose=TRUE
                       ) {
    # if output is a directory, we won't be able to overwrite it
    if (dir.exists(output)) { stop(paste("Can't write to output file", output, "since it's actually a directory.")) }
    if (template==output) { stop("Specify an output file that is different than the template.") }
    thisdir <- getwd()
    .fullpath <- function (x) { ifelse( is_absolute_path(x), x, file.path(normalizePath("."),x) ) }
    template.loc <- .fullpath(template)
    output.loc <- .fullpath(output)
    resource.dir.loc <- .fullpath(resource.dir)
    macros.loc <- .fullpath(macros)
    md.dir <- dirname(md.file)
    dir.create(dirname(md.file),showWarnings=FALSE,recursive=TRUE)
    # change directory knitr looks for files in (by default is relative to where the template is)
    #   this is executed *after* we've setwd()'d, so it's "."
    if (change.rootdir) { opts.knit$root.dir <- "." }
    # outbase is everything but the last suffix in md.file
    outbase <- gsub("[.][^.]*$","",basename(md.file))
    # change directory so that paths are correct relative to where the markdown file is
    if (verbose) {
        cat("## run_template:\n")
        cat(paste("setwd('",md.dir,"')\n",sep=''))
        cat(paste("knitr::opts_chunk$set( fig.path=file.path('figure','",outbase,"',''), 
                  cache.path=file.path('cache','",outbase,"','') )\n",sep=''))
        if (length(opts.knit)>0) cat(paste("knitr::opts_knit$set(", paste(names(opts.knit),paste0('"',opts.knit,'"'),sep="="), ")\n"))
        cat(paste("knitr::knit('",template.loc,"',output='",basename(md.file),"')\n",sep=''))
    }
    setwd(md.dir)
    on.exit(setwd(thisdir),add=TRUE)
	knitr::opts_chunk$set( fig.path=file.path("figure",outbase,""),
                           cache.path=file.path("cache",outbase,"") )
    if (length(opts.knit)>0) { do.call( knitr::opts_knit$set, opts.knit ) }
    knitr::knit(template.loc,output=basename(md.file))
    if (html) {
        dir.create(dirname(output.loc),showWarnings=FALSE,recursive=TRUE)
        if (verbose) cat("Using pandoc to write html output to", output.loc, "\n")
        if (verbose) cat("pandoc", c( basename(md.file), .pandoc.opts(resource.dir.loc,macros=macros.loc), paste("--output", output.loc) ),"\n" )
        system2( "pandoc", args=c( basename(md.file), .pandoc.opts(resource.dir.loc,macros=macros.loc), paste("--output", output.loc) ) )
    }
    return(output.loc)
}


.pandoc.opts <-  function (resource.dir, 
                           .local.mathjax = "/usr/share/javascript/mathjax/MathJax.js",
                           .pandoc.template = system.file("rmarkdown-template.html",package="templater"),
                           macros = "macros.tex" ) {
        .mathjax <- if (file.exists(.local.mathjax)) { .local.mathjax } else { "https://cdn.mathjax.org/mathjax/latest/MathJax.js" }
        opts <- c("--to html", 
                   "--from markdown",
                   "--self-contained", 
                   "--standalone", 
                   "--section-divs", 
                   paste("--template", .pandoc.template),
                   "--variable 'theme:bootstrap'", 
                   # paste("--include-in-header ", file.path(resource.dir,"header-scripts.html")), 
                   "--mathjax", 
                   paste("--variable 'mathjax-url:",.mathjax,"?config=TeX-AMS-MML_HTMLorMML'",sep=''), 
                   paste("--variable 'libraries-url:",resource.dir,"'",sep=''), 
                   "--no-highlight", 
                   paste("--variable highlightjs=",file.path(resource.dir,"highlight"),sep=''), 
                   paste("--include-in-header ", file.path(resource.dir,"mathjax-config.js"))
               )
        if (file.exists(macros)) {
            temp.macros <- tempfile()
            cat("\\[", file=temp.macros)
            file.append(temp.macros,macros)
            cat("\\]", file=temp.macros, append=TRUE)
            opts <- c( opts, 
                   paste("--include-in-header ", temp.macros) )
        }
        return(opts)
}

# pass in opts.knit=list( animation.fun=.hook_ffmpeg_html )
# to output mp4 instead of webm
.hook_ffmpeg_html <- function (x, options) { knitr:::hook_ffmpeg(x, options, ".mp4") }


#' Write Out a Table to knitr's Cache Directory
#'
#' Writes out the given table to a text file to the currently used cache directory,
#' and returns the path to the file written.
#'
#' @param x Data to write out.
#' @param file File name to write to.
#' @param ... Other parameters passed to write.csv().
#' @export
#' @return The name of the output file.
write_table_cache <- function (x,
                               file=paste(knitr::opts_current$get("label"),".csv",sep=''),
                               ...) {
    outfile <- file.path( knitr::opts_current$get("cache.path"), file )
    write.csv( x, file=file, ... )
    return( outfile )
}

#' Check if Path is Absolute
#'
#' Absolute paths start with either ~ or / (linux) or X:/ or X:\\ (windows).
#'
#' @param x The paths to be checked
#'
#' @return A logical vector.
is_absolute_path <- function (x) {
    prefixes <- lapply( strsplit(x,"[/\\]"), "[", 1 )
    return( grepl( "^.:$", prefixes ) | (prefixes == "") | (substr(prefixes,1,1)=="~") )
}
