
#' Compile a R/markdown Template
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
#' @param mathjax.loc Location to look for a local copy of MathJax (harmless if not present).
#' @param verbose Whether to print commands sufficient to recreate what is done.
#' @param clean Whether to delete intermediate files (the .md file, if the output is html).
#' @param width The width of the container (in valid html code).
#' @param pandoc.exec Name of the pandoc executable.
#' @param ... Additional parameters passed to \code{knitr::knit()}.
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
#' 
#' The html document produced will use the copy of MathJax at \code{mathjax.loc} if it is not NULL and that file is present,
#' but will fall back on the CDN \code{https://cdn.mathjax.org/mathjax/latest/MathJax.js} if it is not.
#' 
#' If you want the code in the .Rmd file to be evaluated in the current environment,
#' as is the default if you call \code{knit("myfile.Rmd")}, then pass the option
#' \code{envir=environment()} (or, \code{envir=globalenv()} if you want the global environment).
render_template <- function ( template,
                           output=gsub("[.](Rmd|md)$",".html",template),
                           html=grepl("html$",output),
                           md.file=gsub("[.](html|md)$",".md",output),
                           resource.dir=system.file(package="templater"),
                           macros="macros.tex",
                           opts.knit=NULL,
                           change.rootdir=FALSE,
                           mathjax.loc="/usr/share/javascript/mathjax/MathJax.js",
                           verbose=TRUE,
                           clean=FALSE,
                           width="940px",
                           pandoc.exec="pandoc",
                           ...
                       ) {
    if (nchar(Sys.which(pandoc.exec))==0) {
        stop("It looks like pandoc is not installed or not available -- see http://pandoc.org/installing.html for installation.")
    }
    pandoc.only <- (missing(template) && ! missing(md.file))
    if (pandoc.only) { output <- gsub("[.](Rmd|md)$", ".html", md.file) }
    if (dir.exists(output)) { stop(paste("Can't write to output file", output, "since it's actually a directory.")) }
    thisdir <- getwd()
    .fullpath <- function (x) { ifelse( is_absolute_path(x), x, file.path(normalizePath("."),x) ) }
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
    if (!pandoc.only) {
        template.loc <- .fullpath(template)
        if (template==output) { stop("Specify an output file that is different than the template.") }
        if (verbose) {
            cat("## run_template:\n")
            cat(paste("setwd('",md.dir,"')\n",sep=''))
            cat(paste("knitr::opts_chunk$set( fig.path=file.path('figure','",outbase,"',''), 
                      cache.path=file.path('cache','",outbase,"','') )\n",sep=''))
            if (length(opts.knit)>0) cat(paste("knitr::opts_knit$set(", paste(names(opts.knit),paste0('"',opts.knit,'"'),sep="="), ")\n"))
            ## without dots:
            # cat(paste("knitr::knit('",template.loc,"',output='",basename(md.file),"')\n",sep=''))
            ## attempt 1:
            # could do this with deparse(call(...)) but don't see how to put the 'knitr::' into the call...
            # knit.call <- do.call( call, c( list( "knit", template.loc, output=basename(md.file) ), lapply(as.list(substitute(list(...)))[-1L],deparse) ) )
            # cat( paste0("knitr::", paste(deparse(knit.call,width.cutoff=500L),collapse=" ")), "\n" )
            ## attempt 2:
            dotargs <- as.list(substitute(list(...)))[-1L]
            dotarg.string <- paste( names(dotargs), sapply( dotargs, deparse ), sep="=", collapse=", " )
            knit.call <- call( "knit", template.loc, output=basename(md.file) )
            cat( gsub( ") *$", paste(",",dotarg.string,")"), paste( deparse(knit.call,width.cutoff=500L), collapse=" ") ), "\n")
        }
        # change directory so that paths are correct relative to where the markdown file is
        setwd(md.dir)
        on.exit(setwd(thisdir),add=TRUE)
        knitr::opts_chunk$set( fig.path=file.path("figure",outbase,""),
                               cache.path=file.path("cache",outbase,"") )
        if (length(opts.knit)>0) { do.call( knitr::opts_knit$set, opts.knit ) }
        knitr::knit(template.loc,output=basename(md.file),...)
    }
    if (html) {
        dir.create(dirname(output.loc),showWarnings=FALSE,recursive=TRUE)
        if (verbose) cat("Using pandoc to write html output to", output.loc, "\n")
        if (verbose) cat(pandoc.exec, c( basename(md.file), .pandoc.opts(resource.dir.loc,macros=macros.loc,.local.mathjax=mathjax.loc,width=width), paste("--output", output.loc) ),"\n" )
        system2( pandoc.exec, args=c( basename(md.file), .pandoc.opts(resource.dir.loc,macros=macros.loc,.local.mathjax=mathjax.loc,width=width), paste("--output", output.loc) ) )
        if (clean) {
            if (verbose) cat(sprintf("unlink(%s)",md.file),"\n")
            unlink(md.file)
        }
    }
    return(output.loc)
}


.pandoc.opts <-  function (resource.dir, 
                           .local.mathjax = "/usr/share/javascript/mathjax/MathJax.js",
                           .pandoc.template = system.file("rmarkdown-template.html",package="templater"),
                           macros = "macros.tex",
                           width="940px" ) {
    # .mathjax <- if (file.exists(.local.mathjax)) { .local.mathjax } else { "https://cdn.mathjax.org/mathjax/latest/MathJax.js" }
    # the template will deal even if this file doesn't exist
    .mathjax <- if ( (!is.null(.local.mathjax)) && is.character(.local.mathjax) ) { .local.mathjax } else { "https://cdn.mathjax.org/mathjax/latest/MathJax.js" }
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
               paste("--variable 'width:",width,"'",sep=''), 
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
