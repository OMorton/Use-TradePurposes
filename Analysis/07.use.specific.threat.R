
library(tidyverse)

data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## Read in data ----------------------------------------------------------------

# U & T data
IUCN.AVES.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv")) %>% select(-X)
IUCN.MAM.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.MAMMALIA.Jul25.csv")) %>% select(-X)

# Verbose text descriptions
IUCN.AVES.text <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.verbose.txt.Jun25.csv"))
IUCN.MAM.text <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.verbose.txt.MAMMALIA.Jul25.csv"))

# BRU threat tables
IUCN.AVES.threat <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.threat.Jun25.csv")) %>% mutate(Class = "Aves")
IUCN.MAM.threat <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.threat.MAMMALIA.Jul25.csv")) %>% mutate(Class = "Mammalia")

# 
IUCN.AVES.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Jun25.csv"))
IUCN.MAM.taxo <-read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.MAMMALIA.Jul25.csv"))
sp.status <- rbind(IUCN.AVES.taxo, IUCN.MAM.taxo) %>% select(IUCN.name, status)

## IUCN species use ------------------------------------------------------------
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

length(unique(single.use.sp$IUCN.name)) # 4396
length(unique(multi.use.sp$IUCN.name)) # 1975
length(unique(IUCN.use.tidy$IUCN.name)) # 6371

# 6371 species with a use per the IUCN U & T 
# (Note - 264 are reported as threatened by BRU but have nothing flagged in U & T)
IUCN.use.sp <- mulit.use.sp %>%
  pivot_wider(id_cols = "IUCN.name", names_from = "code",
              values_from = "level") %>%
  mutate(used.per.UT = 1)

## BRU as a threat -------------------------------------------------------------
IUCN.threat <- rbind(IUCN.AVES.threat, IUCN.MAM.threat)

# 2455 species
IUCN.BRU.all <- IUCN.threat %>% filter(code %in% c("5_1_1", "5_1_4"))
# 2244 species
IUCN.BRU.filt <- IUCN.BRU.all %>% 
  filter(!timing %in% c("Past, Unlikely to Return")) %>%
  # add scoring
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

## Linking BRU threat to specific end uses -------------------------------------
# 1513 species (likely medium or highly threatened by use).
IUCN.BRU.Thr <- IUCN.BRU.filt %>% filter(threat.score >= 6)
# 1132 species after removing LC and EX species.
IUCN.BRU.Thr <- IUCN.BRU.filt %>% filter(threat.score >= 6) %>%
  left_join(sp.status) %>% filter(!status %in% c("EX", "LC"))

## Pathway 1 - if a species is likely threatened by use and only has 1 use 
## documented by the IUCN then we can reasonably assume that that must logically
## be the use associated with the threat.

# 545 species are only used
IUCN.BRU.Thr.single <- IUCN.BRU.Thr %>%
  filter(IUCN.name %in% single.use.sp$IUCN.name) %>%
  left_join(single.use.sp)

IUCN.BRU.Thr.single %>% group_by(description) %>% tally()

## Pathway 2 - where more than 1 use is associated with species threatened by BRU
## here it cannot be deduced from the species Threat table and Use & Trade
## taxonomy alone what explicit end use is threatening the species. 
## It could be one or multiple of the listed uses explicitly driving the threat 
## or it could not be possible to deduce this from the given data.

# 107 species are likely threatened by BRU but are not assessed by the IUCN U & T
# to have a known use.
IUCN.BRU.Thr.multi <- IUCN.BRU.Thr %>%
  filter(!IUCN.name %in% multi.use.sp$IUCN.name &
           !IUCN.name %in% single.use.sp$IUCN.name )

# 480 species are likely threatened by BRU and have multiple U & T end uses.
IUCN.BRU.Thr.multi <- IUCN.BRU.Thr %>%
  filter(IUCN.name %in% mulit.use.sp$IUCN.name)
