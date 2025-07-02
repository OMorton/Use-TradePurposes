library(tidyverse)
library(rvest)
library(tm)
library(stringi)

## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"
all.wiki.urls <- read.csv(paste0(data.path, "Data/Wikipedia/all.birds.wiki.csv"))
matched.wiki.urls <- read.csv(paste0(data.path, "Data/Wikipedia/IUCN.Wikipedia.taxo.match.csv"))

## Scrape wikipedia -------------------------------------------------------------


wiki.text.df <- data.frame()

for (i in 1:nrow(matched.wiki.urls)) {
  cat(i, "out of ",nrow(matched.wiki.urls), "\n")
  wiki.i <- matched.wiki.urls[i,]
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

## Text search -----------------------------------------------------------------

order.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv"))
used.sp <- order.use %>% filter(!is.na(code)) #6559 rows
used.sp %>% group_by(description) %>% tally()

# Dominant use categories are
# - Food (consumption, eaten, eating, delicacy, consumed, cooking, meat)
# - Pets (pet, cagebird, cage-bird, cage bird, collector)
# - Sport hunting (game bird, gamebird, sport shoot, sports shoot, drive)
# - jewelry, 
# - medicine
# - Wearing apparel, accessories

# generic (trade, market, sold, hunt)
