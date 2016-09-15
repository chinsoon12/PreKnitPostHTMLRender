#setwd("C:/src/misc")
setwd("C:\\R\\R-3.3.0\\library\\RSelenium\\bin")
rm(list=ls())
options(stringsAsFactors=FALSE)

library(RSelenium)
library(rvest)
library(stringi)

system("java -jar selenium-server-standalone.jar", wait=FALSE, invisible=FALSE)
#startServer()
remDr <- remoteDriver(browserName='chrome')
remDr$open()

remDr$navigate("https://www.sgpokemap.com/")
Sys.sleep(3)
doc <- read_html(remDr$getPageSource()[[1]])

filterBtn <- deselect <- remDr$findElement("id", "filter_link")
filterBtn$clickElement()

deselectBtn <- remDr$findElement("id", "deselect_all_btn")
deselectBtn$clickElement()

#all pokemon names
pokeDF <- data.frame(PokeCheckbox=doc %>% html_nodes(xpath="//*/label") %>% html_attr("for"),
    PokeName=doc %>% html_nodes(xpath="//*/label") %>% html_text())
pokeDF$PokeIdx <- as.numeric(stri_replace_first_fixed(pokeDF$PokeCheckbox,"checkbox_",""))

pokeBtn <- remDr$findElement("xpath", "//*[@for='checkbox_4']")
pokeBtn$clickElement()

closeBtn <- remDr$findElement("id", "close_btn")
closeBtn$clickElement()

remDr$navigate("https://www.sgpokemap.com/")
Sys.sleep(3)

icons <- remDr$findElements("class", "leaflet-marker-icon")

for (n in 1:length(icons)) {
    imgPath <- icons[[n]]$getElementAttribute("src")[[1]]
    style <- icons[[n]]$getElementAttribute("style")[[1]]

    ans <- tryCatch(icons[[n]]$clickElement(), error=function(e) NA)
    if (!is.null(ans) && is.na(ans)) next()

    doc <- read_html(remDr$getPageSource()[[1]])
    #write_xml(doc, "temp.html")
    txt <- doc %>% html_nodes(xpath="//*[@class='leaflet-popup-content']") %>% html_text()

    pokemon <- stri_split_regex(txt, "[0-9]")[[1]][1]

    minss <- stri_split_fixed(txt, " |")[[1]][1] %>%
        stri_replace_first_fixed(pokemon, "") %>%
        stri_split_fixed(":") %>% .[[1]] %>% as.numeric()

    ggmapUrl <- doc %>% html_nodes(xpath="//*[@class='leaflet-popup-content']") %>% html_nodes("a") %>% html_attr("href")

    leafcloseBtn <- remDr$findElement("class", "leaflet-popup-close-button")
    leafcloseBtn$clickElement()
    Sys.sleep(0.5)

    print(n)
    print(pokemon)
    print(style)
    print(ggmapUrl)
} #for each icons
