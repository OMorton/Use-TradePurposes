library(tidyverse)
library(rvest)
library(tm)
library(stringi)

## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"
wiki.urls <- read.csv(paste0(data.path, "Data/Wikipedia/all.birds.wiki.csv"))

## Scrape wikipedia -------------------------------------------------------------

# tidy up species common name
wiki.urls <- wiki.urls %>% 
  mutate(common.name = gsub("https://en.wikipedia.org/wiki/", "", wikipediaURL),
         common.name = gsub("_", " ", common.name),
         common.name = gsub("%27", "'", common.name),)

wiki.text.df <- data.frame()

for (i in 1:nrow(wiki.urls)) {
  cat(i, "out of 12053", "\n")
  wiki.i <- wiki.urls[i,]
  # Read the HTML content from the page
  page <- read_html(wiki.i$wikipediaURL)
  # Extract the main content area of the article
  main.content <- page %>%
  html_node("#mw-content-text .mw-parser-output")
  # Extract all paragraph text (excluding tables, infoboxes, etc.)
  text.paragraphs <- main.content %>%
   html_elements("p") %>%
    html_text(trim = TRUE)
  # Filter out empty paragraphs
  text.paragraphs <- text.paragraphs[nchar(text.paragraphs) > 0]
  # Combine all paragraphs into single text 
  article.text <- paste(text.paragraphs, collapse = "\n\n")
  wiki.i$text <- article.text
  wiki.i <-wiki.i %>% mutate(text = gsub("\n", " ", text))
  # output
  wiki.text.df <- rbind(wiki.text.df, wiki.i)
}
# no NAs
check <- wiki.text.df %>% filter(is.na(text))

write.csv(wiki.text.df, paste0(data.path, "Data/Wikipedia/all.birds.wiki.text.csv"))
