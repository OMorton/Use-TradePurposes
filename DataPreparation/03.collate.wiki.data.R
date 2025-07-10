library(tidyverse)
library(rvest)
library(tm)
library(stringi)

## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"
all.wiki.urls <- read.csv(paste0(data.path, "Data/Wikipedia/all.birds.wiki.csv"))
matched.wiki.urls <- read.csv(paste0(data.path, "Data/Wikipedia/IUCN.Wikipedia.taxo.match.csv")) %>%
  select(-X)

## Scrape wikipedia -------------------------------------------------------------

wiki.text.df <- data.frame()

for (i in 1:nrow(matched.wiki.urls)) {
  cat(i, "out of ",nrow(matched.wiki.urls), "\n")
  wiki.i <- matched.wiki.urls[i,]
  # Read the HTML content from the page
  if (!is.na(wiki.i$wikipediaURL)){
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
  wiki.i <-wiki.i %>% mutate(text = gsub("\n", " ", text))}else{
    wiki.i$text <- NA
  }
  
  # output
  wiki.text.df <- rbind(wiki.text.df, wiki.i)
}
# 48 NAs as supplied ()
check <- wiki.text.df %>% filter(is.na(text))
# tidy where species binomial names link to two wikipedia pages (4 species)
# collapse these to one string
wiki.text.df2 <- wiki.text.df %>% 
  group_by(IUCN.name, common.name,status) %>% 
  summarise(wikipediaURL = toString(wikipediaURL),
            text = toString(text))

write.csv(wiki.text.df2, paste0(data.path, "Data/Wikipedia/all.birds.wiki.text.csv"))

## Text search -----------------------------------------------------------------

order.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv"))
used.sp <- order.use %>% filter(!is.na(code)) #6559 rows
used.sp %>% group_by(description) %>% tally()


# Dominant use categories are classed as those with >50 species referenced, 
# "Other" also has more than 50 is too vague to incorporate.
# - Food (consumption, eaten, eating, delicacy, consumed, cooking, meat, bushmeat
# bush meat, culinary, cuisine, for food)
# - Pets (pet, cagebird, cage-bird, cage bird, collector, aviculture, aviculturist)
# - Sport hunting (game bird, gamebird, sport shoot, sports shoot, drive, game,
# recreational, waterfowling, fowling, open season, closed season)
# - jewelry (ornamental, decorative, jewel, craft, handicrafts, artisanal,
# decoration, earing, necklace, carving, adornment)
# - medicine (medicine, medicinal, remedy, cure, curative, therapy, therapeutic,
# healing, treat, treatment, charm)

# generic (trade, market, sold, hunt, trapped, trapping, sustainable,
# unsustainable, exploited, shot)

# get wiki data
wiki.all.text <- read.csv(paste0(data.path, "Data/Wikipedia/all.birds.wiki.text.csv"))

# define search strings
# space are included where words are commonly prefaced or suffixed by exclusionary
# terms e.g. trapped should flag, retrapped should not as it likely refers to banding.
food.keywords <- c("\\bconsumption\\b", "\\beaten\\b", "\\beating\\b", 
                   "\\bdelicacy\\b", "\\bconsumed\\b", 
                   "\\bcooking\\b", "\\bmeat\\b", "\\bbushmeat\\b", "bush meat", "\\bculinary\\b",
                   "\\bcuisine\\b")
pet.keywords <- c("\\bpet\\b","\\bpet\\.", "\\bcagebird\\b", "\\bcage-bird\\b",
                  "cage bird", "\\bcollector\\b", "\\baviculture\\b", 
                  "\\baviculturist\\b", "\\bdomesticated\\b")
sport.keywords <- c("\\bgame bird\\b", "\\bgamebird\\b", "\\bsport shoot\\b",
                    "\\bsports shoot\\b", "\\bshoot\\b", "\\bdrive\\b", "\\bshot\\b",
                    "\\bgame\\b", "\\brecreational\\b", 
                    "\\bwaterfowling\\b", "\\bfowling\\b", "\\bopen season\\b",
                    "\\bclosed season\\b")
jewelry.keywords <- c("\\bornamental\\b", "\\bdecorative\\b", "\\bjewel", 
                      "\\bhandicrafts\\b", "\\bartisanal", "\\bdecoration",
                      "\\bearing", "\\bnecklace", "\\bcarving", "\\badornment")
medicine.keywords <- c("\\bmedicine\\b", "\\bmedicinal\\b", "\\bremedy\\b", 
                       "\\bcure\\b", "\\bcurative\\b", 
                       "\\btherapy\\b", "\\btherapeutic\\b", "\\bhealing\\b", 
                       "\\bcharm\\b", "\\bto treat\\b")
generic.keywords <- c("\\btrade\\b", "\\bmarket\\b", "\\bsold\\b", "\\bhunt\\b",
                      "\\btrapped\\b", "\\btrapping\\b", "\\bsustainable\\b",
                      "\\bunsustainable\\b", "\\bexploited\\b", "\\bshot\\b", 
                      "\\bsubsistence\\b")

# collapse
food.str <- str_c(food.keywords, collapse = "|")
pet.str <- str_c(pet.keywords, collapse = "|")
sport.str <- str_c(sport.keywords, collapse = "|")
jewelry.str <- str_c(jewelry.keywords, collapse = "|")
medicine.str <- str_c(medicine.keywords, collapse = "|")
generic.str <- str_c(generic.keywords, collapse = "|")

# search
wiki.key.search <- wiki.all.text %>%
  mutate(food = str_detect(text, regex(food.str, ignore_case = TRUE)),
         pet = str_detect(text, regex(pet.str, ignore_case = TRUE)),
         sport = str_detect(text, regex(sport.str, ignore_case = TRUE)),
         jewelry = str_detect(text, regex(jewelry.str, ignore_case = TRUE)),
         medicine = str_detect(text, regex(medicine.str, ignore_case = TRUE)),
         generic = str_detect(text, regex(generic.str, ignore_case = TRUE)),
         # which words are detected
         food.matches = str_extract_all(text, regex(food.str, ignore_case = TRUE)),
         food.matches = sapply(food.matches, function(x) paste(unique(x), collapse = ", ")),
         pet.matches = str_extract_all(text, regex(pet.str, ignore_case = TRUE)),
         pet.matches = sapply(pet.matches, function(x) paste(unique(x), collapse = ", ")),
         sport.matches = str_extract_all(text, regex(sport.str, ignore_case = TRUE)),
         sport.matches = sapply(sport.matches, function(x) paste(unique(x), collapse = ", ")),
         jewelry.matches = str_extract_all(text, regex(jewelry.str, ignore_case = TRUE)),
         jewelry.matches = sapply(jewelry.matches, function(x) paste(unique(x), collapse = ", ")),
         medicine.matches = str_extract_all(text, regex(medicine.str, ignore_case = TRUE)),
         medicine.matches = sapply(medicine.matches, function(x) paste(unique(x), collapse = ", ")),
         generic.matches = str_extract_all(text, regex(generic.str, ignore_case = TRUE)),
         generic.matches = sapply(generic.matches, function(x) paste(unique(x), collapse = ", ")))

wiki.key.search<- wiki.key.search %>% 
  mutate(det = food + pet + sport + jewelry + medicine + generic)
write.csv(wiki.key.search, 
          paste0(data.path, "Data/Wikipedia/wiki.use.match.raw.csv"))

# Next step is to take the IUCN + other trade use database and match to this 
# then all species with "novel" uses reported by Wiki can be investigated.

                  
test <- data.frame(text = c("petrel", "the pet was hunted", "the petrel is a seabird"))         
test %>% mutate(pet = str_detect(text, regex(pet.str, ignore_case = TRUE)))
