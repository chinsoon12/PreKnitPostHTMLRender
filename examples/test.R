testrmd <- tempfile("test", getwd(), ".Rmd")
addhtml <- normalizePath(tempfile("additional", getwd(), ".html"),'/')

#generate the test Rmd file
cat(paste0('---
title: "Example Usage"
output: html_document
---

This document is used for various similar reports.

@@@
cat(paste("#Dynamic Header1", rnorm(1)))
@@@

`%%% writeLines(readLines("',addhtml,'"))`

@@@
cat(paste("##Dynamic Header2", rnorm(1)))
@@@

Some content is invariant across different reports.'), file=testrmd)

#generate test html file
cat('<ul>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ul>', file=addhtml)

preknit_knit_render_postrender(testrmd)
