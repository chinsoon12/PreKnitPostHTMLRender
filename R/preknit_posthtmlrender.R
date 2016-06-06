##########################################################################
#' Eval a line of code and capture output
#'
#' Run a line of code and capture output into a character string
#'
#' @param oneline - code line
#'
#' @param envir - environment to evaluate the code in
#'
#' @importFrom utils capture.output
#'
run_code <- function(oneline, envir) {
    ans <- capture.output(eval(parse(text=oneline), envir=envir))
    gsub("^\\[.*\\] ", "", ans)
} #run_code



##########################################################################
#' Eval a character array of code
#'
#' Evaluate an array of code line by line and capture output
#'
#' @param srcCharVec - code character array
#'
#' @param ... - arguments to be passed on to other functions
#'
eval_src <- function(srcCharVec, ...) {
    unlist(lapply(srcCharVec,
        function(x) {
            if (trimws(x) != '') {
                return(run_code(x, ...))
            }
        }))
} #eval_src



##########################################################################
#' Pre-knitting Processing
#'
#' Evaluates code chunks which are marked as between adjacent lines of @@@s.
#' Replace these code chunks with evaluated output captured using capture.output function.
#'
#' @param infile - input file for be processed
#'
#' @param outfile - output file name
#'
#' @importFrom tools file_path_sans_ext
#'
pre_knit_proc <- function(infile, outfile=NULL) {
    symbol <- '@@@'

    if (is.null(outfile)) {
        outfile <- tempfile(tools::file_path_sans_ext(infile), fileext='.Rmd')
    }

    #read in code file
    incode <- readLines(infile)

    #chunk start indices
    indices <- which(grepl(symbol, incode))

    if (length(indices) > 0) {
        #chunk/code starting line positions
        startIdx <- indices[seq.int(1L, length(indices), 2L)]
        endIdx <- indices[seq.int(2L, length(indices), 2L)]

        #split into incode chunks i.e. those without and those with symbol
        idx <- rep(0, length(incode))
        posn <- sort(unique(c(1, startIdx, pmin(endIdx+1, length(incode)))))
        idx[posn] <- 1
        codeSections <- split(incode, cumsum(idx))

        #create a new environment to run chunk so as not to overwrite
        #existing variables
        temp_env_ <- new.env()

        #source each chunk of code and return the character vector
        newIncode <- do.call(c, lapply(codeSections, function(x) {
            x <- trimws(x)
            if (x[1] == symbol) {
                x <- x[x!=symbol]
                return(eval_src(x, envir=temp_env_))
            }
            return(x)
        }))

        #write new code to output file
        writeLines(newIncode, outfile)
    } else {

        #no symbol found; use original input file
        invisible(file.copy(infile, outfile))
    }
} #pre_knit_proc



##########################################################################
#' Post HTML-Rendering Processing
#'
#' Evaluates code chunks in between `%%% MY_CODE_HERE `. Replace
#' these code chunks with evaluated output captured using capture.output function.
#' Typically, we want to insert an external HTML file and we can
#' use `%%% writeLines(readLines("MY_EXTERNAL_HTML_FILE"))` within the Rmd file.
#' This function will read in the external html file and replace this `%%% ` with
#' contents in the html file.
#'
#' @param inhtml - input file for be processed
#'
#' @param outhtml - output file name
#'
#' @import XML
#'
#' @importFrom tools file_path_sans_ext
#'
post_html_render_proc <- function(inhtml, outhtml=NULL) {
    symbol <- '%%%'

    if (is.null(outhtml)) {
        outhtml <- tempfile(tools::file_path_sans_ext(inhtml), fileext='.html')
    }

    #parse input html
    indoc <- htmlParse(inhtml)

    #select code nodes
    codenodes <- getNodeSet(indoc, "//code")

    #create a new environment to run chunk so as not to overwrite
    #existing variables
    temp_env_ <- new.env()

    #evaluate and replace code
    invisible(lapply(codenodes, function(x) {
        rcodes <- xmlValue(x)
        if (grepl(paste0("^",symbol), rcodes)) {
            rcodes <- trimws(gsub(paste0("^",symbol), "", rcodes))
            htmlcode <- eval_src(rcodes, temp_env_)
            htmldoc <- htmlParse(htmlcode, asText=TRUE)
            newnode <- getNodeSet(htmldoc, "//body/*")[[1]]
            replaceNodes(x, newnode)
        }
    }))

    saveXML(indoc, file=outhtml)
} #post_html_render_proc



##########################################################################
#' First Pre-knitting Processing, then knitr::knit, then rmarkdown::render,
#' then Post HTML-rendering Processing
#'
#' 1) In pre-knitting processing, function takes in a Rmd file, evaluates code chunks
#' which are marked as between adjacent lines of @@@@@@s, and then
#' replace these code chunks with evaluated output captured using capture.output function.
#' 2) Function then calls knitr::knit followed by rmarkdown::render.
#' 3) After which, in post HTML-rendering processing, function evaluates code chunks
#' in between `%%% MY_CODE_HERE `, and then replace these code chunks with evaluated output
#' captured using capture.output function. Typically, we want to insert an external HTML file
#' and we can use `%%% writeLines(readLines("MY_EXTERNAL_HTML_FILE"))` within the Rmd file.
#' This function will read in the external html file and replace this `%%% ` with
#' contents in the html file.
#'
#' @param pRmdfile - input Rmd file
#'
#' @param outhtml - output html file
#'
#' @return The output html file will have the codes between @@@@@@
#' and/or \`\%\%\% \` (where \` is a backtick) code chunks evaluated
#'
#' @import knitr
#'
#' @import rmarkdown
#'
#' @importFrom tools file_path_sans_ext
#'
#' @export
#'
#' @examples
#' \donttest{
#' oldwd <- getwd()
#' setwd(tempdir())
#'
#' #pandoc.exe is required to run this code
#' samplermd <- tempfile('test', getwd(), '.Rmd')
#' addhtml <- 'test__test.html'
#'
#' #generate the test Rmd file
#' writeLines(c('---',
#' 'title: "Example Usage"',
#' 'output: html_document',
#' '---',
#' '',
#' 'This document is used for various similar reports.',
#' '',
#' '@@@@@@',
#' "cat(paste('#Dynamic Header1', rnorm(1)))",
#' '@@@@@@',
#' '',
#' '`%%% writeLines(readLines("test__test.html"))`',
#' '',
#' '@@@@@@',
#' 'cat(paste("##Dynamic Header2", rnorm(1)))',
#' '@@@@@@',
#' '',
#' 'Some content is invariant across different reports.'), samplermd)
#'
#' #generate test html file
#' writeLines(c('<ul>',
#' '<li>Item 1</li>',
#' '<li>Item 2</li>',
#' '<li>Item 3</li>',
#' '</ul>'), addhtml)
#'
#' #Pre-knit processing and post HTML render processing
#' preknit_knit_render_postrender(samplermd, "sample__html.html")
#'
#' #output 'sample__html.html' is in tempdir()
#'
#' setwd(oldwd)
#' }
#'
preknit_knit_render_postrender <- function(pRmdfile, outhtml=NULL) {
    bn <- tools::file_path_sans_ext(basename(pRmdfile))
    Rmdfile <- tempfile(bn, getwd(), ".Rmd")
    mdfile <- tempfile(bn, getwd(), ".md")
    htmlfile <- tempfile(bn, getwd(), ".html")
    if (is.null(outhtml)) {
        outhtml <- tempfile(bn, getwd(), ".html")
    }

    #print("Pre-knitting processing...")
    pre_knit_proc(pRmdfile, Rmdfile)

    #print("Knitting...")
    knit(Rmdfile, mdfile)

    #print("Rendering...")
    render(mdfile, output_file=htmlfile)

    #print("Post rendering processing...")
    post_html_render_proc(htmlfile, outhtml)

    unlink(Rmdfile)
    unlink(mdfile)
    unlink(htmlfile)
} #preknit_knit_render_postrender
