
library(tidyverse)
library(caret)
source("functions.R")

data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## Read in data ----------------------------------------------------------------

# U & T data
IUCN.AVES.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.Oct25.csv")) %>% select(-X)
IUCN.MAM.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.MAMMALIA.Oct25.csv")) %>% select(-X)

# Verbose text descriptions
IUCN.AVES.text <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.verbose.txt.Oct25.csv"))
IUCN.MAM.text <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.verbose.txt.MAMMALIA.Oct25.csv"))

# BRU threat tables
IUCN.AVES.threat <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.threat.Oct25.csv")) %>% mutate(Class = "Aves")
IUCN.MAM.threat <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.threat.MAMMALIA.Oct25.csv")) %>% mutate(Class = "Mammalia")

# Species IUCN Taxonomy etc.
IUCN.AVES.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Oct25.csv"))
IUCN.MAM.taxo <-read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.MAMMALIA.Oct25.csv"))
sp.status <- rbind(IUCN.AVES.taxo, IUCN.MAM.taxo) %>% select(IUCN.name, status)

## Extract list of used species per the IUCN ------------------------------------------------------------
IUCN.use.all <- rbind(IUCN.AVES.use, IUCN.MAM.use)

IUCN.use.tidy <- IUCN.use.all %>% 
  # keep only used sp
  filter(!is.na(code)) %>%
  mutate(international = ifelse(international == TRUE, "international", NA),
         national = ifelse(national == TRUE, "national", NA),
         subsistence = ifelse(subsistence == TRUE, "subsistence", NA),
         other = ifelse(other == TRUE, "other", NA),
         code = paste0("IUCN.UT.",code),
         level = paste(international, national, subsistence, other, sep = ", "),
         level = gsub("NA, ", "", level),
         level = gsub(", NA", "", level), 
         level = gsub("NA", "unknown", level))

single.use.sp <- IUCN.use.tidy %>% group_by(IUCN.name) %>% filter(n() == 1)
multi.use.sp <- IUCN.use.tidy %>% group_by(IUCN.name) %>% filter(n() > 1)

length(unique(single.use.sp$IUCN.name)) # 4381
length(unique(multi.use.sp$IUCN.name)) # 1954
length(unique(IUCN.use.tidy$IUCN.name)) # 6335

# 6335 species with a use per the IUCN U & T 
# (Note - 264 are reported as threatened by BRU but have nothing flagged in U & T)
IUCN.use.sp <- multi.use.sp %>%
  pivot_wider(id_cols = "IUCN.name", names_from = "code",
              values_from = "level") %>%
  mutate(used.per.UT = 1)

## Extract list of threats for these species -------------------------------------------------------
IUCN.threat <- rbind(IUCN.AVES.threat, IUCN.MAM.threat)

# 2521 species
IUCN.BRU.all <- IUCN.threat %>% filter(code %in% c("5_1_1", "5_1_4"))
# 2308 species
IUCN.BRU.filt <- IUCN.BRU.all %>% 
  filter(!timing %in% c("Past, Unlikely to Return")) %>%
  # optional scoring criteria
  mutate(scope.n = case_when(scope == "Minority (<50%)" ~ 1,
                             scope == "Majority (50-90%)" ~ 2,
                             scope == "Whole (>90%)" ~ 3,
                             scope == "Unknown" ~ 2,
                             is.na(scope) ~2),
         timing.n = case_when(timing == "Ongoing" ~3,
                              timing %in% c("Future", "Past, Likely to Return") ~ 1,
                              timing == "Unknown" ~ 2),
         severity.n = case_when(severity == "Very Rapid Declines" ~ 3,
                                 severity %in% c("Rapid Declines", "Unknown") ~ 2,
                                 is.na(severity) ~ 2,
                                 severity %in% c("Slow, Significant Declines",
                                                 "Causing/Could cause fluctuations") ~1,
                                 severity %in% c("No decline", "Negligible declines") ~0),
         threat.score = scope.n + timing.n + severity.n) %>%
  rename("BRU.descr" = "description", "BRU.code" = "code")

# 1563 species (likely medium or highly threatened by use).
IUCN.BRU.Thr <- IUCN.BRU.filt %>% filter(threat.score >= 6)
# 1558 if we keep all threat scores, after removing LC and EX species (.
IUCN.BRU.Thr <- IUCN.BRU.filt %>%
  #filter(threat.score >= 6) %>%
  left_join(sp.status) %>% filter(!status %in% c("EX", "LC"))

## Pathway 1 - if a species is likely threatened by use and only has 1 use 
## documented by the IUCN then we can reasonably assume that that must logically
## be the use associated with the threat.

# 776 species are only used for one purpose
IUCN.BRU.Thr.single <- IUCN.BRU.Thr %>%
  filter(IUCN.name %in% single.use.sp$IUCN.name) %>%
  left_join(single.use.sp)

IUCN.BRU.Thr.single %>% group_by(description) %>% tally()

write.csv(IUCN.BRU.Thr.single, paste0(data.path, "Outputs/threat.dataset/IUCN.BRU.Thr.single.Oct25.csv"))

## Pathway 2 - where more than 1 use is associated with species threatened by BRU
## here it cannot be deduced from the species Threat table and Use & Trade
## taxonomy alone what explicit end use is threatening the species. 
## It could be one or multiple of the listed uses explicitly driving the threat 
## or it could simply not be possible to deduce this from the given data.

# 153 species are likely threatened by BRU but are not assessed by the IUCN U & T
# to have a known use. These species go into the pipeline to see if specific uses are mentioned
IUCN.BRU.Thr.none <- IUCN.BRU.Thr %>%
  filter(!IUCN.name %in% multi.use.sp$IUCN.name &
           !IUCN.name %in% single.use.sp$IUCN.name )

# 629 threatend species with BRU listed and have multiple U & T end uses.
IUCN.BRU.Thr.multi <- IUCN.BRU.Thr %>%
  filter(IUCN.name %in% multi.use.sp$IUCN.name)

## initial tuning set
test.samp <- IUCN.BRU.Thr.multi %>% slice_sample(n = 100) %>% select(IUCN.name, status)
#write.csv(test.samp, paste0(data.path, "Outputs/threat.dataset/IUCN.BRU.Thr.multi.testset.Oct25.csv"))

## final test set
test.samp <- IUCN.BRU.Thr.multi %>% slice_sample(n = 200) %>% select(IUCN.name, status)
#write.csv(test.samp, paste0(data.path, "Outputs/threat.dataset/IUCN.BRU.Thr.multi.final.testset.Oct25.csv"))

## Keyword/phrase generation ---------------------------------------------------

## Process
# Unlikely - any LC species with uses, 
# Unlikely - any species with uses that does not have BRU listed as a threat
# Unlikely - any species uses that are explicitly detailed as not a threat
# Insufficient information - the species is used and BRU is a threat, but that use is not mentioned anywhere
# Likely - if a species has a single use and bru is down as a threat
# Likely - use is a threat, use is mentioned in the threat rationale
# Certain - specific use is mentioned as a key threat in the threat, trend or assessment

# 1. clean up the IUCN text
stopwords <- c(
  "a", "also", "an", "and", "any", "are", "as", "at", "be", "been", "being", 
  "by", "for", "from", "has", "have", "if", "in", "is", "it", "of", "or", 
  "still", "that", "the", "there", "this", "to", "were", "where", 
  "which", "with", "would", "species", "species's")
stopwords.pattern <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\b")


## Focus on the 6 most common uses (food, pets, sport, medicine, apparel, aesthetics)
# 65 strings
food.1 <- c("exploitation(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "exploited(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "hunted(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "hunting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "trapping(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "trapped(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "shot(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "snared(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "snaring(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "harvesting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "killed(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "capture(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+food",
            "exploitation(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "exploited(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "hunted(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "hunting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "trapping(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "trapped(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "shot(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "snared(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "snaring(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "harvesting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "killed(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "capture(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+consumption",
            "exploitation(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "exploited(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "hunted(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "hunting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "trapping(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "trapped(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "shot(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "snared(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "snaring(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "harvesting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "killed(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "collection(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "capture(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+subsistence",
            "hunting food", "hunted food", "trapped food",
            "trapping food", "snared food", "snaring food",
            "sold food",
            "hunting consumption", "hunted consumption", "trapped consumption",
            "local consumption",
            "shot food", "shot consumption",
            "trapping consumption", "snared consumption", "snaring consumption",
            # "poaching for their meat", "poached for their meat",
            # "hunting for their meat", "hunted for their meat",
            # "poaching for meat", "poached for meat",
            # "hunted for meat", "hunting for meat",
            "\\bmeat\\b", # what to do "hunted for the cage-bird trade and meat"
            "body parts food",
            "venison",
            # generic
            "bushmeat", "restaurant", "delicacy",
            "subsistence hunting", "subsistence trapping", "killed subsistence",
            "hunted subsistence", "trapped subsistence", "subsistence harvest")

# 6 strings
meds.3 <- c("\\bmedicin", "\\bremedy", "\\bremedies", "\\bcurative",
            "pharmaceutical product", "pharmaceutical licence")

# hides and skins potentially not clear on end use (e.g wild ass - medicinal use)
# 16 strings
apparel.10 <- c("clothes", "clothing", "\\bcloth\\b",
                "\\bhat\\b", "\\bhats\\b", "leather",
                "fur trade", "fur buyers", "hunted(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+fur\\b",
                "trapped(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+fur\\b", "killed(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+fur\\b",
                "\\bskins\\b", "skin trade", 
                "skin(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+trade\\b", 
                "skin(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+market\\b", 
                "\\bhides")

# 13 strings
aesthetics.12 <- c("souvenir", "\\bcurio\\b", "\\bcurios\\b","headdress",
                   "\\bcarving", "\\bcarved", "casque", "tourist key ring",
                "arrow fletch", "ornament", "decorate", "decorative", "plumes")

# 22 strings
pets.13 <- c("cagebird", "cage-bird", "cage bird", 
             "aviary bird", "aviary-bird",
            "songbird", "song bird", "falconry",
            "\\bpet\\b", "\\bpets\\b", 
            "live-capture", "captive trade", "captive-trade",
            "nest poaching", "bird market", "bird-market",
            "live animal market", "live-animal market", 
            "wildbird trade", "wild bird trade",
            "\\bcompanion", "companion")

# 13 strings
sport.15 <- c("\\btrophy\\b", 
              "\\brecreational hunting", "\\bsport hunting",
              "hunting(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+sport",
              "hunted(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+recreation",
              "hunted(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+sport",
              "hunting(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+recreation",
              "killed(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+sport",
              "killed(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+recreation",
              "hunting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+using falcons",
              "hunting(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+using falconry",
              "hunted(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+using falcons",
              "hunted(?:[^\\w\\.]+\\w+){0,3}[^\\w\\.]+using falconry")


# Negative impact string
# 33 strings
neg.string <- c("(?<!not )declin", "(?<!poaching )declin", "(?<!hunting )declin",
                "(?<!offtakes )declin", "(?<!volumes )declin",
                "(?<!not )decreas", "(?<!poaching )decreas", "(?<!hunting )decreas",
                "(?<!offtakes )decreas", "(?<!volumes )decreas",
                "(?<!not )\\bthreat", "extinct",
                "(?<!not )negative impact", "(?<!not )negatively impact", "(?<!not )serious impact", "(?<!not )seriously impact",
                "(?<!not )negative effect", "(?<!not )negatively effect", "(?<!not )serious effect", "(?<!not )seriously effect",
                "(?<!not )negative affect", "(?<!not )negatively affect", "(?<!not )serious affect", "(?<!not )seriously affect",
                "(?<!not )reduced numbers", "(?<!not )reduced population", "(?<!not )population reduc",
                "(?<!not )unsustain", "not sustainab", "(?<!not )exceed sustainab",
                "(?<!not )above sustainab", "(?<!not )overexploit",
                "(?<!not )reduction", "(?<!not )overharvested")

# no negative impact string
# 45 strings
no.neg.string <- c("(?:not|nor) declin", "no declin", "no evidence declin", 
                 "unlikely drive declin","(?:not|nor) likely drive declin",
                 "unlikely drive impact","(?:not|nor) likely impact",
                 "unlikely drive effect","(?:not|nor) likely effect",
                 "unlikely drive affect","(?:not|nor) likely affect",
                 "minimal impact", "no impact",
                 "(?<!not )negligible decline", "(?<!not )negligible impact", 
                 "does not appear cause declin",
                 "not decreas", "no decreas", "no evidence decreas", 
                 "unlikely decreas", "unlikely drive decreas",
                 "(?:not|nor) likely drive decreas", "does not appear cause decreas",
                 "(?:not|nor) threat", "(?:not|nor) likely threat", "unlikely threat",
                 "(?:not|nor) seem major threat", "(?:not|nor) seem key threat",
                 "(?:not|nor) seem important threat",
                 "(?:not|nor) major threat", "(?:not|nor) key threat",
                 "(?:not|nor) likely major threat", "(?:not|nor) likely key threat", "(?:not|nor) likely threat",
                 "(?:not|nor) currently major threat", "(?:not|nor) currently key threat", "(?:not|nor) currently threat", 
                 "(?:not|nor) important threat", "(?:not|nor) considered threat", "(?:not|nor) meaningful threat",
                 "(?:not|nor) pose meaningful threat", "(?:not|nor) pose significant threat",
                 "(?:not|nor) pose substantial threat",
                 "(?:not|nor) drive extinct", "(?:not|nor) cause extinct", "(?:not|nor) increase extinct",
                 "not elevate extinct", "(?<!not)\\bsustainable", "(?<!not)\\bsustainably",
                 "not overexploit",
                 "(?:not|nor) negatively impact", "(?:not|nor) negatively effect",
                 "not thought significant", "not significant",
                 "(?:not|nor) driving population reduc", "no population reduc",
                 "(?:not|nor)(?:\\s+\\w+)?\\s+driving(?:\\s+\\w+)?\\s+population declines",
                 "(?:not|nor)(?:\\s+\\w+)?\\s+driving(?:\\s+\\w+)?\\s+population impacts",
                 "(?:not|nor)(?:\\s+\\w+)?\\s+driving(?:\\s+\\w+)?\\s+population reductions",
                 "(?:not|nor)(?:\\s+\\w+)?\\s+causing(?:\\s+\\w+)?\\s+population declines",
                 "(?:not|nor)(?:\\s+\\w+)?\\s+causing(?:\\s+\\w+)?\\s+population impacts",
                 "(?:not|nor)(?:\\s+\\w+)?\\s+causing(?:\\s+\\w+)?\\s+population reductions",
                 "(?<!not )positive impact", "(?<!no )positive impact")

# unknown impact string
# 11 strings
unc.string <- c("unknown effect", "unknown impact", "impacts unknown", "impact unknown",
                "threat posed unknown", "unquantified",
                "not(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+known",
                "not(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+quantified",
                "not(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+established",
                "threat(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+unknown",
                "threat(?:[^\\w\\.]+\\w+){0,2}[^\\w\\.]+uncertain")

# number of search combinations
str.tally <- data.frame(use = c("food", "meds", "apparel", "aesthetics", "pets", "sport"),
           use.str = c(length(food.1), length(meds.3), length(apparel.10),
                       length(aesthetics.12), length(pets.13), length(sport.15)),
           neg.str = length(neg.string),
           no.neg.str = length(no.neg.string),
           unc.str = length(unc.string)) %>%
  mutate(total = (use.str*neg.str) +(use.str*no.neg.str) + (use.str*unc.str))
sum(str.tally$total) # 14,715

# make strings, add exceptions for extra letters after the word.
# breaks prior to the words are specified were necessary above.
food.1.str <- str_c("(", str_c(food.1, collapse = "|"), ")[a-z]*\\b")
meds.3.str <- str_c("(", str_c(meds.3, collapse = "|"), ")[a-z]*\\b")
apparel.10.str <- str_c("(", str_c(apparel.10, collapse = "|"), ")[a-z]*\\b")
aesthetics.12.str <- str_c("(", str_c(aesthetics.12, collapse = "|"), ")[a-z]*\\b")
pets.13.str <- str_c("(", str_c(pets.13, collapse = "|"), ")[a-z]*\\b")
sport.15.str <- str_c("(", str_c( sport.15, collapse = "|"), ")[a-z]*\\b")
use.str.ls <- list("food" = food.1.str,
          "medicine" = meds.3.str,
          "apparel" = apparel.10.str,
          "aesthetic" = aesthetics.12.str,
          "pets" = pets.13.str,
          "sport" = sport.15.str)

neg.str <- str_c(neg.string, collapse = "|")
no.neg.str <- str_c(no.neg.string, collapse = "|")
unc.str <- str_c(unc.string, collapse = "|")


sentence.check <- function(sentence, use, impact) {
  any(str_detect(sentence, use)) && any(str_detect(sentence, impact))
}


misleading.str <- c("medicine trees", "medicinal trees", 
                    "medicinal plants", "medicine plants",
                    "medicinal herbs", "medicine herbs",
                    "recreational activities", "recreational watersports",
                    "recreational chasing",
                    "subsistence logging",
                    "subsistence cropping", "subsistence farming",
                    "subsistence agriculture", 
                    "leather leaf ferns", "leather fern", "leather leaf",
                    "ornamental crop", "ornamental shrub", "ornamental plant")
misleading.str <- paste(misleading.str, collapse = "|")


## Run the text classifier -----------------------------------------------------
#test.dat100 <- read.csv(paste0(data.path, "Outputs/threat.dataset/IUCN.BRU.Thr.multi.testset.Oct25.LABELLED.csv"))

IUCN.all.text <- rbind(IUCN.AVES.text, IUCN.MAM.text)
sp.use.out <- data.frame()
all.sp.use <- data.frame()
i <- 4311
j <- 1
for (i in 1:nrow(IUCN.all.text)) {
  cat(i, "\n")
  # make text block and clean structure - threat and assessment based text
  text.i <- IUCN.all.text[i,] %>%
  #text.i <- filter(IUCN.all.text, IUCN.name %in% test.dat100$IUCN.name)[i,] %>%
    mutate(all.text = paste(threat.txt, assmnt.txt, trend.just.txt, trend.rat.txt,
                            sep = " ")) %>%
    select(IUCN.name, all.text) %>%
    mutate(all.text = gsub("<[^>]+>", " ", all.text),# remove html
           all.text = trimws(all.text),
           all.text = gsub("&#160;|\\u00A0", " ", all.text),
           # Collapse multiple spaces
           all.text = gsub("\\s+", " ", all.text) %>% trimws()) %>%  # remove excess ws
    mutate(all.text = gsub(stopwords.pattern, "", all.text, ignore.case = TRUE),
           all.text = tolower(all.text), # put into lower case
           all.text = gsub("  ", " ", all.text), # remove blocks of space
           all.text = gsub("   ", " ", all.text),
           all.text = gsub("litt.", "litt", all.text),
           all.text = gsub("et al.", "et al", all.text),
           all.text = gsub("pers. comm.", "pers comm", all.text),
           all.text = gsub("however", ".", all.text), # split sentences at however (see threats https://www.iucnredlist.org/species/45430583/179386817)
           all.text = str_replace_all(all.text, "(?<=\\b[A-Za-z])\\.", ""),
           # Remove irrelevant strings risking confusion
           all.text = gsub(misleading.str, "", all.text, ignore.case = TRUE))
  
  # make text block and clean structure - threat and assessment based text + U&T text
  text.inc.UT.i <- IUCN.all.text[i,] %>%
    #text.i <- filter(IUCN.all.text, IUCN.name %in% test.dat100$IUCN.name)[i,] %>%
    mutate(all.text = paste(threat.txt, assmnt.txt, trend.just.txt, trend.rat.txt,
                            use.trade.txt,
                            sep = " ")) %>%
    select(IUCN.name, all.text) %>%
    mutate(all.text = gsub("<[^>]+>", "", all.text),# remove html
           all.text = trimws(all.text),
           all.text = gsub("&#160;|\\u00A0", " ", all.text),
           # Collapse multiple spaces
           all.text = gsub("\\s+", " ", all.text) %>% trimws()) %>%  # remove excess ws
    mutate(all.text = gsub(stopwords.pattern, "", all.text, ignore.case = TRUE),
           all.text = tolower(all.text), # put into lower case
           all.text = gsub("  ", " ", all.text), # remove blocks of space
           all.text = gsub("   ", " ", all.text),
           all.text = gsub("litt.", "litt", all.text),
           all.text = gsub("et al.", "et al", all.text),
           all.text = gsub("pers. comm.", "pers comm", all.text),
           all.text = gsub("however", ".", all.text), # split sentences at however (see threats https://www.iucnredlist.org/species/45430583/179386817)
           all.text = str_replace_all(all.text, "(?<=\\b[A-Za-z])\\.", ""),
           # Remove irrelevant strings risking confusion
           all.text = gsub(misleading.str, "", all.text, ignore.case = TRUE))
  
  # split threat, assesment and trend text into sentences.
  sentences.i <- str_split(text.i$all.text, "(?<=[.!?])\\s+")[[1]]
  sentences.inc.UT.i <- str_split(text.inc.UT.i$all.text, "(?<=[.!?])\\s+")[[1]]
  
  for (j in 1:length(use.str.ls)) {
    use.j <- use.str.ls[[j]]
    name.j <- names(use.str.ls[j])
    
    gen.matches <- str_detect(sentences.i, use.j)
    neg.matches <- sapply(sentences.inc.UT.i, sentence.check, use = use.j, impact = neg.str)
    no.neg.matches <- sapply(sentences.inc.UT.i, sentence.check, use = use.j, impact = no.neg.str)
    unknown.matches <- sapply(sentences.inc.UT.i, sentence.check, use = use.j, impact = unc.str)
    

    sp.use.j <- data.frame(use = name.j, 
              gen.mention = any(gen.matches == TRUE),
              neg.mention = any(neg.matches == TRUE),
              no.neg.mention = any(no.neg.matches == TRUE),
              unknown.mention = any(unknown.matches == TRUE),
              # verbose
              gen.string = paste(sentences.i[gen.matches], collapse = 'END'),
              neg.string = paste(sentences.inc.UT.i[neg.matches], collapse = 'END'),
              no.neg.string = paste(sentences.inc.UT.i[no.neg.matches], collapse = 'END'),
              unknown.string = paste(sentences.inc.UT.i[unknown.matches], collapse = 'END'))
    
    sp.use.out <- rbind(sp.use.out, sp.use.j)
    
  }
  sp.use.out$IUCN.name <- text.i$IUCN.name
  sp.use.out$sentences <- length(sentences.i)
  all.sp.use <- rbind(all.sp.use, sp.use.out)
  sp.use.out <- data.frame()
}
all.sp.use %>% select(IUCN.name, sentences) %>% distinct() %>%
  summarise(sum(sentences)) #206017
write.csv(all.sp.use, paste0(data.path, "Outputs/threat.dataset/IUCN.use.thr.all.classified.Nov25.csv"))


## Test classifier -------------------------------------------------------------
all.sp.use <- read.csv(paste0(data.path, "Outputs/threat.dataset/IUCN.use.thr.all.classified.Nov25.csv"))
test.dat200 <- read.csv(paste0(data.path, "Outputs/threat.dataset/IUCN.BRU.Thr.multi.final.testset.Oct25.LABELLED.csv"))

test.dat200 <- test.dat200 %>% arrange(IUCN.name) %>%
  mutate(across(-c(1:2), ~ case_when(
    .x == 1 ~ TRUE,
    is.na(.x) ~ FALSE,
    TRUE ~ as.logical(.x))))

all.test.sp.use <- all.sp.use %>% filter(IUCN.name %in% test.dat200$IUCN.name) %>% arrange(IUCN.name)

uses.ls <- list("food", "medicine", "apparel", "aesthetic", "pets", "sport")
i <- "aesthetic"
performance.out <- data.frame()
for (i in uses.ls) {
  fit.use.i <- all.test.sp.use %>% filter(use == i)
  
  gen.conf <- confusionMatrix(factor(fit.use.i$gen.mention, levels = c(FALSE, TRUE)), 
                  factor(select(test.dat200, paste0(i))[[1]], levels = c(FALSE, TRUE)))
  neg.conf <- confusionMatrix(factor(fit.use.i$neg.mention, levels = c(FALSE, TRUE)), 
                  factor(select(test.dat200, paste0(i, ".threat"))[[1]], levels = c(FALSE, TRUE)))
  no.neg.conf <- confusionMatrix(factor(fit.use.i$no.neg.mention, levels = c(FALSE, TRUE)),
                  factor(select(test.dat200, paste0(i, ".not.threat"))[[1]], levels = c(FALSE, TRUE)))
  insuf.conf <- confusionMatrix(factor(fit.use.i$unknown.mention, levels = c(FALSE, TRUE)),
                  factor(select(test.dat200, paste0(i, ".insuf.threat"))[[1]], levels = c(FALSE, TRUE)))

  summary.perf <- rbind(conf.as.df(gen.conf) %>% mutate(use = i, type = "general", 
                                                        tally = sum(select(test.dat200, paste0(i))[[1]])),
        conf.as.df(neg.conf) %>% mutate(use = i, type = "negative", 
                                        tally = sum(select(test.dat200, paste0(i, ".threat"))[[1]])),
        conf.as.df(no.neg.conf) %>% mutate(use = i, type = "no negative", 
                                           tally = sum(select(test.dat200, paste0(i, ".not.threat"))[[1]])),
        conf.as.df(insuf.conf) %>% mutate(use = i, type = "insufficient data", 
                                          tally = sum(select(test.dat200, paste0(i, ".insuf.threat"))[[1]])))
  
  performance.out <- rbind(performance.out, summary.perf)


}

performance.out.tidy <- performance.out %>% 
  mutate(prevalence = tally/200,
         across(where(is.numeric) & !c(tally), ~ round(.x, 2)),
         tally = paste0(tally, " (", prevalence, ")")) %>%
  select(use, type, tally, kappa, acc, acc.nir, spec, sens, bal.acc)

write.csv(performance.out.tidy, 
          paste0(data.path, "Outputs/threat.dataset/IUCN.BRU.Thr.multi.final.testset.Oct25.PERFORMANCE.METRICS.Oct25.csv"))


test.dat100
all.sp.use

check.i <- data.frame(IUCN.name = test.dat100$IUCN.name,
                      est = as.factor(fit.use.i$neg.mention), 
                      test = as.factor(select(test.dat100, paste0(i, ".threat"))[[1]]))

# https://www.iucnredlist.org/species/22686268/179290500

## Plotting --------------------------------------------------------------------
all.sp.use <- read.csv(paste0(data.path, "Outputs/threat.dataset/IUCN.use.thr.all.classified.Nov25.csv"))

# Tot 1558 sp.
classif.single <- all.sp.use %>% filter(IUCN.name %in% IUCN.BRU.Thr.single$IUCN.name) #776 sp.
classif.none <- all.sp.use %>% filter(IUCN.name %in% IUCN.BRU.Thr.none$IUCN.name) #153 sp.
classif.multi <- all.sp.use %>% filter(IUCN.name %in% IUCN.BRU.Thr.multi$IUCN.name) #629 sp.

#
classif.single2 <- classif.single %>% 
  mutate(use.threat = case_when(gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Insufficient infomation",
                                gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == TRUE & unknown.mention == FALSE ~ "Unlikely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == TRUE & unknown.mention == FALSE ~ "Unlikely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Potential",
                                gen.mention == TRUE & neg.mention == TRUE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Highly likely",
                                gen.mention == FALSE & neg.mention == TRUE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Highly likely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == TRUE ~ "Insufficient infomation",
                                gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == TRUE ~ "Insufficient infomation",
                                neg.mention == TRUE & no.neg.mention == TRUE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == TRUE & no.neg.mention == FALSE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == FALSE & no.neg.mention == TRUE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == TRUE & no.neg.mention == TRUE & unknown.mention == FALSE ~ "MANUAL"),
         uses = "single")

classif.none2 <- classif.none %>% 
  mutate(use.threat = case_when(gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Insufficient infomation",
                                gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == TRUE & unknown.mention == FALSE ~ "Unlikely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == TRUE & unknown.mention == FALSE ~ "Unlikely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Potential",
                                gen.mention == TRUE & neg.mention == TRUE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Highly likely",
                                gen.mention == FALSE & neg.mention == TRUE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Highly likely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == TRUE ~ "Insufficient infomation",
                                gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == TRUE ~ "Insufficient infomation",
                                neg.mention == TRUE & no.neg.mention == TRUE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == TRUE & no.neg.mention == FALSE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == FALSE & no.neg.mention == TRUE & unknown.mention == TRUE ~ "MANUAL"),
         uses = "none")

classif.multi2 <- classif.multi %>% 
  mutate(use.threat = case_when(gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Insufficient infomation",
                                gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == TRUE & unknown.mention == FALSE ~ "Unlikely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == TRUE & unknown.mention == FALSE ~ "Unlikely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Potential",
                                gen.mention == TRUE & neg.mention == TRUE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Highly likely",
                                gen.mention == FALSE & neg.mention == TRUE &
                                  no.neg.mention == FALSE & unknown.mention == FALSE ~ "Highly likely",
                                gen.mention == TRUE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == TRUE ~ "Insufficient infomation",
                                gen.mention == FALSE & neg.mention == FALSE &
                                  no.neg.mention == FALSE & unknown.mention == TRUE ~ "Insufficient infomation",
                                neg.mention == TRUE & no.neg.mention == TRUE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == TRUE & no.neg.mention == FALSE & unknown.mention == TRUE ~ "MANUAL",
                                neg.mention == FALSE & no.neg.mention == TRUE & unknown.mention == TRUE ~ "MANUAL"),
         uses = "multi")

f <- rbind(classif.multi2, classif.single2, classif.none2)

# all species with conflicting statement e.g. negative and uncertain effects
# manually reviewed
manual.threat.check <- rbind(classif.multi2, classif.single2, classif.none2) %>%
  filter(use.threat == "MANUAL")
# write.csv(manual.threat.check, 
#           paste0(data.path, "Outputs/threat.dataset/IUCN.threat.classif.manual.OUT.csv"))
# 23 species
manual.threat.check.in <- read.csv(paste0(data.path, 
                                          "Outputs/threat.dataset/IUCN.threat.classif.manual.IN.csv"))

## collate and widen classified data
all.classify <- rbind(classif.multi2, classif.single2, classif.none2) %>%
  filter(use.threat != "MANUAL") %>% rbind(manual.threat.check.in)
all.classify.wide <- all.classify %>% pivot_wider(id_cols = c(IUCN.name, uses), names_from = use, values_from = use.threat)

## get uses
IUCN.BRU.Thr.sum <- IUCN.BRU.Thr %>% select(IUCN.name, Class, status, BRU.code, scope, severity, timing)
IUCN.use.tidy.wide <- IUCN.use.tidy %>% select(IUCN.name, code) %>%
  mutate(use.per.UT = case_when(code == "IUCN.UT.1" ~ "food.UT",
                         code == "IUCN.UT.3" ~ "medicine.UT",
                         code == "IUCN.UT.10" ~ "apparel.UT",
                         code == "IUCN.UT.12" ~ "aesthetic.UT",
                         code == "IUCN.UT.13" ~ "pets.UT",
                         code == "IUCN.UT.15" ~ "sport.UT",
                         .default = "other.known.UT")) %>% select(-code) %>%
  group_by(IUCN.name, use.per.UT) %>%
  summarise(pres = 1) %>% pivot_wider(id_cols = IUCN.name, values_from = pres, names_from = use.per.UT)

## append threat and use data
## for species with no uses then keep any uses that are flagged as a threat or not.
## for species used for either apparel or 
t <- all.classify.wide %>% left_join(IUCN.use.tidy.wide) %>%
  mutate(food = case_when(is.na(food.UT) & uses != "none" ~ NA,
                          food == "Insufficient infomation" & uses == "none" ~ NA,
                          .default = food),
         medicine = case_when(is.na(medicine.UT) & uses != "none" ~ NA,
                              medicine == "Insufficient infomation" & uses == "none" ~ NA,
                          .default = medicine),
         pets = case_when(is.na(pets.UT) & uses != "none" ~ NA,
                          pets == "Insufficient infomation" & uses == "none" ~ NA,
                              .default = pets),
         sport = case_when(is.na(sport.UT) & uses != "none" ~ NA,
                           sport == "Insufficient infomation" & uses == "none" ~ NA,
                          .default = sport),
         aesthetic = case_when(is.na(aesthetic.UT) & is.na(apparel.UT) & uses != "none" ~ NA,
                               aesthetic == "Insufficient infomation" & is.na(aesthetic.UT) & uses != "none" ~ NA,
                               aesthetic == "Insufficient infomation" & uses == "none" ~ NA,
                           .default = aesthetic),
         apparel = case_when(is.na(aesthetic.UT) & is.na(apparel.UT) & uses != "none" ~ NA,
                             apparel == "Insufficient infomation" & is.na(apparel.UT) & uses != "none" ~ NA,
                             apparel == "Insufficient infomation" & uses == "none" ~ NA,
                               .default = apparel))
         

