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
    ans <- capture.output(eval(parse(text=oneline),
        envir=envir))
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
#' Evaluate and replace code chunks/lines
#'
#' Extract code chunks for Rmd or code lines for html, evaluate each line and
#' replace original code chunks/lines with captured output in original positions
#'
#' @param infile - input Rmd or html file
#'
#' @param symbol - symbol string to identify which are code chunks/lines to processed
#'
#' @param outfile - output file name after processing. Either outfile or fileext must be provided.
#'
#' @param fileext - file extension of output file is required if outfile is not provided.
#'
#' @param encoding - file encoding is required to readLines of infile
#'
#' @import xml2
#'
#' @importFrom tools file_ext
#'
eval_replace <- function(infile, symbol, outfile=NULL, fileext=NULL, encoding="UTF-8") {
    if (is.null(outfile)) {
        if (!is.null(fileext)) {
            outfile <- tempfile(file_ext(infile), fileext=fileext)
        } else {
            stop("either outfile or fileext must be provided.")
        }
    }

    invisible(file.copy(infile, outfile))

    #read in code file
    incode <- readLines(infile, encoding=encoding)

    #chunk start indices
    indices <- which(grepl(symbol, incode))

    if (length(indices) > 0) {
        if (tolower(file_ext(infile))=="html") {
            doc <- read_html(infile)
            codesInText <- xml_text(xml_find_all(doc, "//code"))
            codesNeedSrcing <- gsub(paste(symbol,""), "",
                codesInText[grepl(symbol, codesInText)])
            reformedCode <- lapply(codesNeedSrcing, function(x) c(symbol, x))
            names(reformedCode) <- indices

            idx <- rep(0, length(incode))
            posn <- sort(unique(c(1, indices, pmin(indices+1,length(incode)))))
            idx[posn] <- posn
            lsIncode <- split(incode, posn[cumsum(idx!=0)])

            for (i in as.character(indices)) {
                lsIncode[[i]] <- reformedCode[[i]]
            }

        } else {
            #chunk/code starting line positions
            startIdx <- indices[seq.int(1L, length(indices), 2L)]
            endIdx <- indices[seq.int(2L, length(indices), 2L)]

            #split into incode chunks i.e. those without and those with symbol
            idx <- rep(0, length(incode))
            posn <- sort(unique(c(1, startIdx, pmin(endIdx+1, length(incode)))))
            idx[posn] <- 1
            lsIncode <- split(incode, cumsum(idx))
        }

        #create a new environment to run chunk so as not to overwrite
        #existing variables
        temp_env_ <- new.env()

        #source each chunk of code and return the character vector
        newIncode <- do.call(c, lapply(lsIncode, function(x) {
            if (x[1] == symbol) {
                x <- x[x!=symbol]
                return(eval_src(x, envir=temp_env_))
            }
            return(x)
        }))

        #write new code to output file
        writeLines(newIncode, outfile)
    }
} #evalReplace



##########################################################################
#' Pre-knitting Processing
#'
#' Evaluates code chunks which are marked as between adjacent lines of @@@s.
#' Replace these code chunks with evaluated output captured using capture.output function.
#'
#' @param pRmdfile - input file for be processed
#'
#' @param Rmdfile - output file name
#'
pre_knit_proc <- function(pRmdfile, Rmdfile=NULL) {
    eval_replace(pRmdfile, symbol='@@@', outfile=Rmdfile, fileext='.Rmd')
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
post_html_render_proc <- function(inhtml, outhtml=NULL) {
    eval_replace(inhtml, symbol='%%%', outfile=outhtml, fileext='.html')
} #post_html_render_proc



##########################################################################
#' First Pre-knitting Processing, then knitr::knit, then rmarkdown::render,
#' then Post HTML-rendering Processing
#'
#' In pre-knitting processing, function takes in a Rmd file, evaluates code chunks
#' which are marked as between adjacent lines of @@@s, and then
#' replace these code chunks with evaluated output captured using capture.output function.
#'
#' Function then calls knitr::knit followed by rmarkdown::render.
#'
#' After which, in post HTML-rendering processing, function evaluates code chunks
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
#' @return The output html file will have the evaluated @@@ and/or `%%% ` code chunks
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
#' samplermd <- tempfile("test", getwd(), ".Rmd")
#' addhtml <- normalizePath(tempfile("additional", getwd(), ".html"),'/',FALSE)
#' #generate the test Rmd file
#' cat(paste0('---
#' title: "Example Usage"
#' output: html_document
#' ---
#'
#' This document is used for various similar reports.
#'
#' @@@
#' cat(paste("#Dynamic Header1", rnorm(1)))
#' @@@
#'
#' `%%% writeLines(readLines("',addhtml,'"))`
#'
#' @@@
#' cat(paste("##Dynamic Header2", rnorm(1)))
#' @@@
#'
#' Some content is invariant across different reports.\n'), file=samplermd)
#'
#' #generate test html file
#' cat('<ul>
#' <li>Item 1</li>
#' <li>Item 2</li>
#' <li>Item 3</li>
#' </ul>\n', file=addhtml)
#'
#' preknit_knit_render_postrender(samplermd)
#'
preknit_knit_render_postrender <- function(pRmdfile, outhtml=NULL) {
    bn <- file_path_sans_ext(basename(pRmdfile))
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
