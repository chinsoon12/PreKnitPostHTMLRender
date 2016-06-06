# PreKnitPostHTMLRender
Dynamize headers or R code within Rmd documents to prevent proliferation of Rmd docs for similar tasks. Add in external HTML document within rmarkdown rendered HTML doc.


## Description
First pre-knitting processing, then `knitr::knit`, then `rmarkdown::render`, then post HTML-rendering processing

The current process:
<pre>    
 *.Rmd       ---------------     *.md     ---------------------  *.html
-----------> | knitr::knit | -----------> | rmarkdown::render | ----------->
             ---------------              ---------------------
</pre>

`preknit_knit_render_postrender`:
<pre>  

*.Rmd     ------------   *.Rmd     ---------------     *.md     ---------------------    *.html    --------------  *.html
--------> | pre_knit |-----------> | knitr::knit | -----------> | rmarkdown::render | -----------> | post_render| -----------> 
          ------------             ---------------              ---------------------              --------------
</pre>


1) In pre-knitting processing, function takes in a Rmd file, evaluates code chunks which are marked as between adjacent lines of `@@@`, and then replace these code chunks with evaluated output captured using `capture.output` function.

2) Function then calls `knitr::knit` followed by `rmarkdown::render`.

3) After which, in post HTML-rendering processing, function evaluates code chunks in between `` `%%% MY_CODE_HERE ` ``, and then replace these code chunks with evaluated output captured using capture.output function. Typically, we want to insert an external HTML file and we can use `` `%%% writeLines(readLines("MY_EXTERNAL_HTML_FILE"))` `` within the Rmd file. This function will read in the external html file and replace this `` `%%% ` `` with contents in the html file.

## Usage

    preknit_knit_render_postrender(pRmdfile, outhtml = NULL)

## Arguments
pRmdfile	- input Rmd file

outhtml	- output html file

## Value
The output html file will have the codes between `@@@` and/or `` `%%% ` `` code chunks evaluated.

## Examples
    #rmarkdown package and pandoc.exe is required to run this code
    
    library(PreKnitPostHTMLRender)
    
    samplermd <- tempfile('test', getwd(), '.Rmd')
    addhtml <- 'test__test.html'
    
    #generate the test Rmd file
    writeLines(c('---',
    'title: "Example Usage"',
    'output: html_document',
    '---',
    '',
    'This document is used for various similar reports.',
    '',
    '@@@',
    "cat(paste('#Dynamic Header1', rnorm(1)))",
    '@@@',
    '',
    '`%%% writeLines(readLines("test__test.html"))`',
    '',
    '@@@',
    'cat(paste("##Dynamic Header2", rnorm(1)))',
    '@@@',
    '',
    'Some content is invariant across different reports.'), samplermd)
    
    #generate test html file
    writeLines(c('<ul>',
    '<li>Item 1</li>',
    '<li>Item 2</li>',
    '<li>Item 3</li>',
    '</ul>'), addhtml)
    
    #Pre-knit processing and post HTML render processing
    preknit_knit_render_postrender(samplermd)

    #You can open 'test__test.html' in your current directory


