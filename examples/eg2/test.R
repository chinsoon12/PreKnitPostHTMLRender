rm(list=ls())
options(stringsAsFactors=FALSE)

devtools::install_github("chinsoon12/PreKnitPostHTMLRender")
library(PreKnitPostHTMLRender)

#http://stackoverflow.com/questions/37703326/rmarkdown-chunk-name-from-variable/37714290#37714290

NUM_CHUNKS <- 5
CHUNK_NAME <- "myChunk-"
preknit_knit_render_postrender("master.Rmd", "test__test.html")
