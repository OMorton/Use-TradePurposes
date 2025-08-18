
library(tidyverse)
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## Explanation -----------------------------------------------------------------

## Data sources
# 1. IUCN Use and Trade taxonomy
# Justification - clearly states what use is assigned to each species through the
# July_2020_Guidance_General_Use_and_Trade_Classification_Scheme.

# 2. IUCN Threat Classification table
# Justification - any species with BRU 5.1.1 (direct use) as a threat must 
# be used in some form. Use not explicitly linked to any particular purpose.

# 3. IUCN SpUD database
# Justification - collates a range of examples of species use from published sources,
# reports, grey literature etc. Details the type of use but not in the same
# format as the IUCN Use and Trade taxonomy. Range of purposes recorded.

# 4. Benítez-López et al., 2017 Science
# Justification - Comprehensive meta-analysis of studies assessing species hunted 
# for consumption. Species included can all be deemed as used, and based on 
# the search string and inclusion/exclusion criteria used for the analysis, can
# be deemed as used for human consumption.

# 5. Donald et al. 2024 Cons Bio
# Justification - compiled CITES, LEMIS, TRAFFIC, Lit review & IUCN data (but 
# not subsistence) to produce a list of "traded" species - specifically excluding
# non commercial use. Does not provide purposes.

# 6. WILDMEAT database
# Justification - comprehenisve review methodology of papers reporting the hunting 
# and consumption of african forest species. Limited to consumptive use only
# (commerical and subsistence).

# 7. Morton et al., 2021 Nat Eco Evo
# Justification - global review of studies assessing the impacts of trade. Documents
# species end use to the level of pet, consumption or assorted

# 8. Marshall et al. 2024/2025 Updated LEMIS database
# Justification - one of the largest databases of traded species, does contain
# purpose and type of species traded. Some of these codes can be combined to 
# infer the end use, many cannot. Also contains records that will be removed 
# (research, reintroduction trade etc.)

# 9. CITES v2025.1 Updated CITES database
# Justification - one of the largest databases of traded species, does contain
# purpose and type of species traded. Some of these codes can be combined to 
# infer the end use, many cannot. Also contains records that will be removed 
# (research, reintroduction trade etc.)

# 10. WiTIS TRAFFIC Database July 2025 version

# 11. Wikipedia species accounts 
# Justification - one of the few if not the only open access resource that collates
# general data around species. Data has full class coverage all detail varies 
# greatly between species. Can be used to assess if species are used and if given
# what the end purpose is.

## Processing - 1. IUCN Use and Trade data ----------------------------------------
IUCN.AVES.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv")) %>% select(-X)
IUCN.MAM.use <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.use.MAMMALIA.Jul25.csv")) %>% select(-X)

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

# 6371 species with a use per the IUCN U & T
IUCN.use.sp <- IUCN.use.tidy %>%
  pivot_wider(id_cols = "IUCN.name", names_from = "code",
              values_from = "level") %>%
  mutate(used.per.UT = 1)

## Processing - 2. IUCN Threat Classification table ----------------------------

IUCN.AVES.thr <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.threat.Jun25.csv")) %>% select(-X)
IUCN.MAM.thr <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.threat.MAMMALIA.Jul25.csv")) %>% select(-X)

IUCN.thr.all <- rbind(IUCN.AVES.thr, IUCN.MAM.thr)

#2434 sp
IUCN.thr.sp <- IUCN.thr.all %>% filter(code == "5_1_1") %>% 
  mutate(used.per.BRU = 1) %>%
  select(IUCN.name, used.per.BRU)

## Processing - 3. IUCN SpUD database ------------------------------------------
# focusing only on those extractive uses - e.g. not non extractive tourism
spud.AVES.df <- read.csv(paste0(data.path, "Data/SpUD/iucn.spud.taxo.match.csv")) %>% select(-X)
spud.MAM.df <- read.csv(paste0(data.path, "Data/SpUD/iucn.spud.mam.taxo.match.csv"))%>% select(-X)

spud.df <- rbind(spud.MAM.df, spud.AVES.df)

unique(spud.df$Purpose)
## 161 species
spud.sp <- spud.df %>% group_by(IUCN.name) %>%
  separate_longer_delim(Purpose, delim = ", ") %>%
  filter(!Purpose %in% c("Learning and education", "Damage mitigation (to reduce impacts on rural people",
                        "landscapes and activities)", "Conservation Management",
                        "Damage mitigation", "Also killed in response to instances of human-wildlife conflict.",
                        "Retaliatory", "Protection (of self and livestock)",
                        "Killed in human-wildlife conflicts; animals destroyed crops or killed humans (elephants)",
                        "Mitigation of Human-Wildlife conflict",
                        "retaliatory killing", "Removal of alien invasive pest parrot species",
                        "Poisining incidents")) %>%
  summarise(Purpose = toString(Purpose)) %>%
  mutate(SpUD.UT.1 = grepl("Food and feed", Purpose),
         SpUD.UT.3 = grepl("Medicine and hygiene", Purpose),
         SpUD.UT.12 = grepl("Decorative and aesthetic", Purpose),
         SpUD.UT.13 = grepl("pet", Purpose),
         SpUD.UT.13 = grepl("Keeping/companionship/display", Purpose),
         SpUD.UT.15 = grepl("Recreation", Purpose)
         # Collection/display unclear from examples if linked to UT13 or 15
         # Ceremony, religious, and ritual expression cannot be clearly linked to an end use
         # Poisining incidents likewise not linked to use - more persecution
         # Offer for sale/Commercial trade likewise not linked to use
         # Amusement likewise not linked to use
         # Monetary likewise not linked to use
         # Scientific Research could be just movement of specimens for study tagging/ringing etc - not clearly a use
  ) %>%
  mutate(used.per.SpUD = 1) %>%
  select(-Purpose)

## Processing - 4. Benítez-López et al., 2017 Science --------------------------

## 315 sp
BenLop.df <- read.csv(paste0(data.path, "Data/Benitez-Lopez_2017/IUCN.BL.taxo.match.csv"))
BenLop.sp <- BenLop.df %>% group_by(IUCN.name) %>%
  summarise(used.per.BenLop = 1,
         BenLop.UT.1 = 1)

## Processing - 5. Donald et al. 2024 Cons Bio ---------------------------------
Don.df <- read.csv(paste0(data.path, "Data/Donald.et.al.2024/IUCN.Donald.taxo.match.csv"))

## 4932 sp
Don.sp <- Don.df %>% filter(Trade.Prevalence.Score > 0) %>%
  group_by(IUCN.name) %>%
  summarise(used.per.Don = 1)

## Processing - 6. WILDMEAT Database -------------------------------------------
WM.df <- read.csv(paste0(data.path,"Data/WILDMEAT/IUCN.WM.taxo.match.csv"))

## 182 sp
WM.sp <- WM.df %>%
  group_by(IUCN.name) %>%
  summarise(WM.UT.1 = 1, used.per.WM = 1)

## Processing - 7. Morton et al. 2021 ------------------------------------------
Morton.df <- read.csv(paste0(data.path,"Data/Morton_et_al_2021/IUCN.Morton.taxo.match.csv")) %>% select(-X)

# 120 sp
Morton.sp <- Morton.df %>% 
  group_by(IUCN.name) %>% mutate(use = 1) %>%
  select(IUCN.name, TP, use) %>% distinct() %>%
  pivot_wider(id_cols = IUCN.name, names_from = TP, values_from = use) %>%
  rename("Mort.UT.1" = "Bushmeat", "used.per.Mort" = "Assorted", "Mort.UT.13" = "Pet") %>%
  mutate(used.per.Mort = 1)

## Processing - 8. Marshal et al. LEMIS data -----------------------------------
lemis.df <-  read.csv(paste0(data.path,"Data/LEMIS/IUCN.LEMIS.taxo.match.csv")) %>%
  select(-X, -X.1)

unique(lemis.df$purpose) 
# remove E (educational), B (breeding in captivity),
#  L (law enforcement/foresic), Y (reintroduction to the wild), 
# S (scientific - to broad to be definitively research and more than half the listed
# species are only traded under the S code many are likely to be just research specimens or the like)
# orignally considered these Zoos (Z)/botanic gardens (G)/circuses (Q) (if live fall under pets/display)
# to be display trade but in reflection this is difficult to be certain off so is now removed.
# Keep H (purpose = Sport/hunting), T (commercial - no purpose), P (personal - no purpose)
#  M (biomed - research), NA (unknown - no purpose),
# * and non-standard value (unknown - no purpose),
# meat is unclear - likely for humans (but could be for animal feed or medicinal use)
unique(lemis.df$description) 

lemis.purp.long <- lemis.df %>% filter(!purpose %in% c("E", "B", "L", "Y", "S", "Z", "G", "Q")) %>%
  mutate(LEMIS.UT.15 = ifelse(purpose == "H"|description == "TRO", 1, 0),
         LEMIS.UT.14 = ifelse(purpose %in% c("M"), 1, 0),
        # LEMIS.UT.13 = ifelse(purpose %in% c("Z", "G", "Q") & description == "LIV", 1, 0),
         LEMIS.UT.12 = ifelse(description %in% c("BOC", "CAR", "HOC", "IJW", "IVC", "JWL") & 
                                purpose != "H", 1, 0),
         LEMIS.UT.10 = ifelse(description %in% c("GAR", "LPS", "SHO", "TRI") & 
                                purpose != "H", 1, 0),
         LEMIS.UT.6 = ifelse(description == "MUS" & purpose != "H", 1, 0),
         LEMIS.UT.8 = ifelse(description == "FIB" & purpose != "H", 1, 0),
         LEMIS.UT.3 = ifelse(description == "MED" & purpose != "H", 1, 0),
         LEMIS.UT.11 = ifelse(description %in% c("KEY", "LPL", "PIV", "RUG") &
                                purpose != "H", 1, 0),
         LEMIS.UT.1 = ifelse(description %in% c("CAL", "CAV", "LEG", "SOU") &
                                purpose != "H", 1, 0))

# 3315 sp
LEMIS.sp <- lemis.purp.long %>% group_by(IUCN.name) %>%
  summarise(LEMIS.UT.15 = ifelse(sum(LEMIS.UT.15)>=1, 1, 0),
            LEMIS.UT.14 = ifelse(sum(LEMIS.UT.14)>=1, 1, 0),
            #LEMIS.UT.13 = ifelse(sum(LEMIS.UT.13)>=1, 1, 0),
            LEMIS.UT.12 = ifelse(sum(LEMIS.UT.12)>=1, 1, 0),
            LEMIS.UT.10 = ifelse(sum(LEMIS.UT.10)>=1, 1, 0),
            LEMIS.UT.6 = ifelse(sum(LEMIS.UT.6)>=1, 1, 0),
            LEMIS.UT.8 = ifelse(sum(LEMIS.UT.8)>=1, 1, 0),
            LEMIS.UT.3 = ifelse(sum(LEMIS.UT.3)>=1, 1, 0),
            LEMIS.UT.11 = ifelse(sum(LEMIS.UT.11)>=1, 1, 0),
            LEMIS.UT.1 = ifelse(sum(LEMIS.UT.1)>=1, 1, 0),
            used.per.LEMIS = 1)

## Processing - 9. CITES v2025.1 Updated CITES database ------------------------
CITES.df <- read.csv(paste0(data.path, "Data/CITES/IUCN.CITES.taxo.match.csv")) %>%
  select(-X)

unique(CITES.df$Purpose)
# REMOVE - B(breeding artifical prop), E (educational), L (law enforcement),
# N reintroduction, S scientific
# orignally considered these Zoos (Z)/botanic gardens (G)/circuses (Q) (if live fall under pets/display)
# to be display trade but in reflection this is difficult to be certain off so is now removed.
# KEEP - H (hunting trophy), P (personal), 
# T (commerical),  M (medical inc biomedical research cannot be clearly 
# split between medicinal and research)
# meat is unclear - likely for humans (but could be for animal feed or medicinal use)
sort(unique(CITES.df$Term))

CITES.purp.long <- CITES.df %>% 
  filter(!Purpose %in% c("B", "E", "L", "N", "S", "Z", "G", "Q")) %>%
  mutate(CITES.UT.15 = ifelse(Purpose == "H"|Term == "trophies", 1, 0),
         #CITES.UT.13 = ifelse(Purpose %in% c("Z", "G", "Q") & Term == "live", 1, 0),
         CITES.UT.12 = ifelse(Term %in% c("bone carvings", "carvings", "horn carvings", "ivory carvings") & 
                                Purpose != "H", 1, 0),
         CITES.UT.8 = ifelse(Term == "fibres", 1, 0),
         CITES.UT.11 = ifelse(Term %in% c("furniture", "leather products (large)", "sets of piano keys") &
                                Purpose != "H", 1, 0),
         CITES.UT.10 = ifelse(Term %in% c("garments", "leather products (small)", "shoes") & 
                                Purpose != "H", 1, 0),
         CITES.UT.1 = ifelse(Term %in% c("soup") & 
                               Purpose != "H", 1, 0),
         CITES.UT.3 = ifelse(Term == "medicine", 1, 0),
         CITES.UT.6 = ifelse(Term == "musk", 1, 0))

# 1699 sp
CITES.sp <- CITES.purp.long %>% group_by(IUCN.name) %>%
  summarise(CITES.UT.15 = replace_na(ifelse(sum(CITES.UT.15, na.rm = T)>1, 1, 0), 0),
            #CITES.UT.13 = ifelse(sum(CITES.UT.13, na.rm = T)>=1, 1, 0),
            CITES.UT.12 = ifelse(sum(CITES.UT.12, na.rm = T)>=1, 1, 0),
            CITES.UT.8 = ifelse(sum(CITES.UT.8, na.rm = T)>=1, 1, 0),
            CITES.UT.11 = ifelse(sum(CITES.UT.11, na.rm = T)>=1, 1, 0),
            CITES.UT.10 = ifelse(sum(CITES.UT.10, na.rm = T)>=1, 1, 0),
            CITES.UT.1 = ifelse(sum(CITES.UT.1, na.rm = T)>=1, 1, 0),
            CITES.UT.3 = ifelse(sum(CITES.UT.3, na.rm = T)>=1, 1, 0),
            CITES.UT.6 = ifelse(sum(CITES.UT.6, na.rm = T)>=1, 1, 0),
            used.per.CITES = 1)

## Processing - 10. WiTIS TRAFFIC Database -------------------------------------
WiTIS.df <- read.csv(paste0(data.path, "Data/WiTIS/IUCN.WiTIS.taxo.match.csv")) %>% select(-X)

unique(WiTIS.df$Category.of.Incident)
unique(WiTIS.df$Item...Commodity.Type)
# "5. Animal Injury / Mortality / Welfare" 
# reading highlights many of these are HWC, or unexplained deaths
# 6. Human-Wildlife Conflict
# not use or trade more persecution
# 7. Breeding / Ranching - unclear if conservation or use
# 4. Live Animals on Display (not for sale) 
# only 1 example seems to refer to a zoo/pet lion playing with a DJ

WiTIS.purp.long <- WiTIS.df %>% 
  filter(!Category.of.Incident %in% c("5. Animal Injury / Mortality / Welfare",
                                      "6. Human-Wildlife Conflict", 
                                      "7. Breeding / Ranching", 
                                      "4. Live Animals on Display (not for sale)")) %>%
  mutate(WiTIS.UT.12 = ifelse(Item...Commodity.Type %in% c("Bone - Worked", "Ivory - Worked",
                                          "Carvings", "Horn - Worked"), 1, 0),
         # WiTIS specific - all examples refer to tiger bone wine (medicinal use)
         WiTIS.UT.3 = ifelse(Item...Commodity.Type == "Wine", 1, 0),
         WiTIS.UT.1 = ifelse(Item...Commodity.Type == "Meat", 1, 0),
         WiTIS.UT.6 = ifelse(Item...Commodity.Type == "Musk", 1, 0))

# 1519 sp
WiTIS.sp <- WiTIS.purp.long %>% group_by(IUCN.name) %>%
  summarise(WiTIS.UT.12 = replace_na(ifelse(sum(WiTIS.UT.12, na.rm = T)>1, 1, 0), 0),
            WiTIS.UT.3 = ifelse(sum(WiTIS.UT.3, na.rm = T)>=1, 1, 0),
            WiTIS.UT.1 = ifelse(sum(WiTIS.UT.1, na.rm = T)>=1, 1, 0),
            WiTIS.UT.6 = ifelse(sum(WiTIS.UT.6, na.rm = T)>=1, 1, 0),
            used.per.WiTIS = 1)  

## Collate full use database ---------------------------------------------------
## taxo for 11,195 sp (11,031 extant)
iucn.taxo.AVES <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Jun25.csv")) %>%
  select(IUCN.name, common.name, familyName, orderName, status) %>% mutate(class = "Aves")
iucn.taxo.MAM <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.MAMMALIA.Jul25.csv"))%>%
  select(IUCN.name, common.name, familyName, orderName, status) %>% mutate(class = "Mammalia")

# 17,220 species total, 16,971 extant species
iucn.taxo <- rbind(iucn.taxo.MAM, iucn.taxo.AVES) %>% filter(status != "EX")

use.raw <- iucn.taxo %>%
  left_join(IUCN.use.sp) %>%
  left_join(IUCN.thr.sp) %>%
  left_join(spud.sp) %>%
  left_join(BenLop.sp) %>%
  left_join(Don.sp)%>%
  left_join(WM.sp)%>%
  left_join(Morton.sp) %>%
  left_join(LEMIS.sp) %>%
  left_join(CITES.sp) %>%
  left_join(WiTIS.sp)

use.raw <- use.raw %>% 
  mutate(across(starts_with('SpUD'), ~ifelse(.x == TRUE,1,0))) %>%
  mutate(IUCN.UT.1.sim = ifelse(is.na(IUCN.UT.1), 0, 1),
         IUCN.UT.2.sim = ifelse(is.na(IUCN.UT.2), 0, 1),
         IUCN.UT.3.sim = ifelse(is.na(IUCN.UT.3), 0, 1),
         IUCN.UT.4.sim = ifelse(is.na(IUCN.UT.4), 0, 1),
         IUCN.UT.5.sim = ifelse(is.na(IUCN.UT.5), 0, 1),
         IUCN.UT.6.sim = ifelse(is.na(IUCN.UT.6), 0, 1),
         IUCN.UT.7.sim = ifelse(is.na(IUCN.UT.7), 0, 1),
         IUCN.UT.8.sim = ifelse(is.na(IUCN.UT.8), 0, 1),
         IUCN.UT.9.sim = ifelse(is.na(IUCN.UT.9), 0, 1),
         IUCN.UT.10.sim = ifelse(is.na(IUCN.UT.10), 0, 1),
         IUCN.UT.11.sim = ifelse(is.na(IUCN.UT.11), 0, 1),
         IUCN.UT.12.sim = ifelse(is.na(IUCN.UT.12), 0, 1),
         IUCN.UT.13.sim = ifelse(is.na(IUCN.UT.13), 0, 1),
         IUCN.UT.14.sim = ifelse(is.na(IUCN.UT.14), 0, 1),
         IUCN.UT.15.sim = ifelse(is.na(IUCN.UT.15), 0, 1),
         IUCN.UT.16.sim = ifelse(is.na(IUCN.UT.16), 0, 1),
         IUCN.UT.17.sim = ifelse(is.na(IUCN.UT.17), 0, 1),
         IUCN.UT.18.sim = ifelse(is.na(IUCN.UT.18), 0, 1),
         across(7:24, ~ replace_na(.x, "0")), # IUCN text based columns
         across(25:82, ~ replace_na(.x, 0)), # all other columns inc new iucn numeric duplicates
    use = used.per.UT + used.per.BRU + used.per.SpUD + used.per.BenLop + 
      used.per.Don + used.per.WM + used.per.Mort + 
      used.per.CITES + used.per.LEMIS + used.per.WiTIS,
    use = ifelse(use > 0, 1, 0),
    any.purpose = ifelse(IUCN.UT.1.sim == 1|IUCN.UT.2.sim == 1|IUCN.UT.3.sim == 1|
                           IUCN.UT.4.sim  == 1|IUCN.UT.5.sim == 1|IUCN.UT.6.sim == 1|
                           IUCN.UT.7.sim == 1|IUCN.UT.8.sim == 1|IUCN.UT.9.sim == 1|
                           IUCN.UT.10.sim == 1|IUCN.UT.11.sim == 1|IUCN.UT.12.sim == 1|
                           IUCN.UT.13.sim == 1|IUCN.UT.14.sim == 1|IUCN.UT.15.sim == 1|
                           IUCN.UT.16.sim == 1|IUCN.UT.17.sim == 1|IUCN.UT.18.sim == 1|
                           SpUD.UT.1 == 1|SpUD.UT.3 == 1|
                         SpUD.UT.12 == 1|SpUD.UT.13 == 1|SpUD.UT.15 == 1|
                         BenLop.UT.1 == 1|Mort.UT.1 == 1|Mort.UT.13 == 1|
                         WM.UT.1 == 1|LEMIS.UT.15 == 1|LEMIS.UT.14 == 1|
                         LEMIS.UT.12 == 1|LEMIS.UT.10 == 1|LEMIS.UT.6 == 1|
                         LEMIS.UT.8 == 1|LEMIS.UT.3 == 1|LEMIS.UT.11 == 1|
                         LEMIS.UT.1 == 1|CITES.UT.15 == 1|CITES.UT.12 == 1|
                         CITES.UT.8 == 1|CITES.UT.11 == 1|CITES.UT.10 == 1|
                         CITES.UT.1 == 1|CITES.UT.3 == 1|CITES.UT.6 == 1|
                         WiTIS.UT.12 == 1|WiTIS.UT.3 == 1|WiTIS.UT.6 == 1,
                         1, 0))
sum(use.raw$use) # 7918 (08/08/25)
sum(use.raw$any.purpose) # 6732 (12/08/25)
use.raw %>% group_by(class) %>% summarise(sum(use)) # 5908 birds, 2010 mammals

## Processing - additional Wikipedia filter ------------------------------------

wiki.uses.AVES.df <- read.csv(paste0(data.path, "Data/Wikipedia/wiki.use.match.raw.csv")) %>% 
  select(-X, -X.1)
wiki.uses.MAM.df <- read.csv(paste0(data.path, "Data/Wikipedia/wiki.use.match.MAMMALIA.raw.csv")) %>% 
  select(-X, -X.1)

wiki.uses.df <- rbind(wiki.uses.AVES.df, wiki.uses.MAM.df)

# extract the positive hits per UT class
wiki.UT.1 <- wiki.uses.df %>% filter(food.UT.1 == TRUE) %>% select(IUCN.name)
wiki.UT.13 <- wiki.uses.df %>% filter(pet.UT.13 == TRUE) %>% select(IUCN.name)
wiki.UT.15 <- wiki.uses.df %>% filter(sport.UT.15 == TRUE) %>% select(IUCN.name)
wiki.UT.12 <- wiki.uses.df %>% filter(jewelry.UT.12 == TRUE) %>% select(IUCN.name)
wiki.UT.3 <- wiki.uses.df %>% filter(medicine.UT.3 == TRUE) %>% select(IUCN.name)
wiki.UT.10 <- wiki.uses.df %>% filter(clothing.UT.10 == TRUE) %>% select(IUCN.name)
wiki.USE <- wiki.uses.df %>% filter(generic == TRUE) %>% select(IUCN.name)

# filter the species not currently flagged in that class that wikipedia suggests is
UT.1.flag <- use.raw %>% filter(SpUD.UT.1 == 0 & BenLop.UT.1 == 0 & IUCN.UT.1.sim == 0 &
                                  Mort.UT.1 == 0 & WM.UT.1 == 0 & LEMIS.UT.1 == 0 &
                                  CITES.UT.1 == 0 & WiTIS.UT.1 == 0) %>%
  filter(IUCN.name %in% wiki.UT.1$IUCN.name)
UT.13.flag <- use.raw %>% filter(SpUD.UT.13 == 0 & IUCN.UT.13.sim == 0 & 
                                 Mort.UT.13 == 0) %>%
  filter(IUCN.name %in% wiki.UT.13$IUCN.name)
UT.15.flag <- use.raw %>% filter(SpUD.UT.15 == 0 & IUCN.UT.15.sim == 0 &
                                 LEMIS.UT.15 == 0 & CITES.UT.15 == 0) %>%
  filter(IUCN.name %in% wiki.UT.15$IUCN.name)
UT.12.flag <- use.raw %>% filter(SpUD.UT.12 == 0 & IUCN.UT.12 == 0 &
                                 LEMIS.UT.12 == 0 & CITES.UT.12 == 0 & WiTIS.UT.12 == 0) %>%
  filter(IUCN.name %in% wiki.UT.12$IUCN.name)
UT.3.flag <- use.raw %>% filter(SpUD.UT.3 == 0 & IUCN.UT.3.sim == 0 &
                                LEMIS.UT.3 == 0 & CITES.UT.3 == 0 & WiTIS.UT.3 == 0) %>%
  filter(IUCN.name %in% wiki.UT.3$IUCN.name)
UT.10.flag <- use.raw %>% filter(IUCN.UT.10.sim == 0 & LEMIS.UT.1 == 0 & 
                                 CITES.UT.10 == 0) %>%
  filter(IUCN.name %in% wiki.UT.10$IUCN.name)
USE.flag <- use.raw %>% filter(use==0) %>%
  filter(IUCN.name %in% wiki.USE$IUCN.name)

# manual review - 2027 species
to.rev <- data.frame(IUCN.name = unique(c(UT.1.flag$IUCN.name, UT.13.flag$IUCN.name,
                              UT.15.flag$IUCN.name, UT.12.flag$IUCN.name,
                              UT.3.flag$IUCN.name, USE.flag$IUCN.name)))

to.rev.out <- wiki.uses.df %>% select(-text) %>% 
  filter(IUCN.name %in% to.rev$IUCN.name)

# ## update old wiki searches
# wiki.uses1 <- read.csv(paste0(data.path, "Data/Wikipedia/wiki.flagged.uses.in.OLD.VERSION.csv")) %>% select(-X, -X.1)
# wiki.uses2 <- read.csv(paste0(data.path, "Data/Wikipedia/wiki.flagged.uses.in.OLD.VERSION2.csv")) %>% select(-X, -X.1)
# wiki.uses1 <- wiki.uses1 %>% select(IUCN.name, MAN.food.UT.1, MAN.pet.UT.13, MAN.sport.UT.15, 
#                       MAN.jewelry.UT.12, MAN.medicine.UT.3, MAN.generic) %>%
#   mutate(checked = 1, MAN.apparel.UT.10 = NA)
# 
# wiki.uses2 <- wiki.uses2 %>% select(IUCN.name, MAN.food.UT.1, MAN.pet.UT.13, MAN.sport.UT.15, 
#                                     MAN.jewelry.UT.12, MAN.medicine.UT.3, MAN.generic, MAN.apparel.UT.10, checked)
# old.uses <- rbind(wiki.uses1, wiki.uses2) %>% distinct() # 1882
# new.combi.list <- to.rev.out %>% left_join(old.uses)
# write.csv(new.combi.list, paste0(data.path, "Data/Wikipedia/wiki.flagged.uses.in.csv"))


write.csv(to.rev.out, paste0(data.path, "Data/Wikipedia/wiki.flagged.uses.out.csv"))

## Adding the Wikipedia data ---------------------------------------------------

wiki.uses <- read.csv(paste0(data.path, "Data/Wikipedia/wiki.flagged.uses.in.csv")) %>% select(-X)
wiki.uses <- wiki.uses %>% 
  select(IUCN.name, MAN.food.UT.1, MAN.pet.UT.13, MAN.sport.UT.15, 
         MAN.jewelry.UT.12, MAN.medicine.UT.3, MAN.apparel.UT.10, MAN.generic) %>% 
  rename("MAN.Wiki.UT.1" = "MAN.food.UT.1", "MAN.Wiki.UT.13" = "MAN.pet.UT.13",
         "MAN.Wiki.UT.15" = "MAN.sport.UT.15", "MAN.Wiki.UT.12" = "MAN.jewelry.UT.12",
         "MAN.Wiki.UT.3" = "MAN.medicine.UT.3","MAN.Wiki.UT.10" = "MAN.apparel.UT.10", 
         "used.per.wiki" = "MAN.generic") %>%
  mutate(across(starts_with('MAN'), ~ifelse(is.na(.x),0,1)),
         used.per.wiki = ifelse(is.na(used.per.wiki), 0, 1)) %>%
  mutate(used.per.wiki = ifelse(MAN.Wiki.UT.1 == 1 |MAN.Wiki.UT.13 == 1 |
                                  MAN.Wiki.UT.15 == 1 |MAN.Wiki.UT.12 == 1 |
                                  MAN.Wiki.UT.3 == 1 |MAN.Wiki.UT.10 == 1 |
                                  used.per.wiki == 1, 1, 0))

use.raw.wiki <- use.raw %>% left_join(wiki.uses) %>%
  mutate(across(starts_with('MAN.Wiki'), ~ifelse(is.na(.x)|.x == 0,0,1)),
         used.per.wiki = ifelse(is.na(used.per.wiki), 0, 1)) %>%
  mutate(use = used.per.UT + used.per.BRU + used.per.SpUD + used.per.BenLop + 
           used.per.Don + used.per.WM + used.per.Mort + 
           used.per.CITES + used.per.LEMIS + used.per.WiTIS + used.per.wiki,
         use = ifelse(use > 0, 1, 0),
         any.purpose = ifelse(IUCN.UT.1.sim == 1|IUCN.UT.2.sim == 1|IUCN.UT.3.sim == 1|
                                IUCN.UT.4.sim  == 1|IUCN.UT.5.sim == 1|IUCN.UT.6.sim == 1|
                                IUCN.UT.7.sim == 1|IUCN.UT.8.sim == 1|IUCN.UT.9.sim == 1|
                                IUCN.UT.10.sim == 1|IUCN.UT.11.sim == 1|IUCN.UT.12.sim == 1|
                                IUCN.UT.13.sim == 1|IUCN.UT.14.sim == 1|IUCN.UT.15.sim == 1|
                                IUCN.UT.16.sim == 1|IUCN.UT.17.sim == 1|IUCN.UT.18.sim == 1|
                                SpUD.UT.1 == 1|SpUD.UT.3 == 1|
                                SpUD.UT.12 == 1|SpUD.UT.13 == 1|SpUD.UT.15 == 1|
                                BenLop.UT.1 == 1|Mort.UT.1 == 1|Mort.UT.13 == 1|
                                WM.UT.1 == 1|LEMIS.UT.15 == 1|LEMIS.UT.14 == 1|
                                LEMIS.UT.12 == 1|LEMIS.UT.10 == 1|LEMIS.UT.6 == 1|
                                LEMIS.UT.8 == 1|LEMIS.UT.3 == 1|LEMIS.UT.11 == 1|
                                LEMIS.UT.1 == 1|CITES.UT.15 == 1|CITES.UT.12 == 1|
                                CITES.UT.8 == 1|CITES.UT.11 == 1|CITES.UT.10 == 1|
                                CITES.UT.1 == 1|CITES.UT.3 == 1|CITES.UT.6 == 1|
                                WiTIS.UT.12 == 1|WiTIS.UT.3 == 1|WiTIS.UT.6 == 1|
                                MAN.Wiki.UT.1 == 1 |MAN.Wiki.UT.13 == 1 |
                                MAN.Wiki.UT.15 == 1 |MAN.Wiki.UT.12 == 1 |
                                MAN.Wiki.UT.3 == 1 |MAN.Wiki.UT.10 == 1, 1, 0))
  

sum(use.raw.wiki$use) # (12/08/25) 8759
sum(use.raw.wiki$any.purpose) # (12/08/25) 6807
use.raw.wiki %>% group_by(class) %>% summarise(sum(use)) # 6344, 2415
write.csv(use.raw.wiki, paste0(data.path, "Outputs/use.dataset/aves.mam.full.uses.raw.csv"))


## Tidy up final dataset -------------------------------------------------------
sort(colnames(use.raw.wiki))
## unknown uses ignored here (but are part of the UT used sp total)
use.df <- use.raw.wiki %>%
  group_by(IUCN.name, common.name, familyName, orderName, status) %>%
  summarise(food.hum.1 = ifelse(IUCN.UT.1.sim == 1|SpUD.UT.1 == 1|BenLop.UT.1 == 1|
                                  WM.UT.1 == 1|Mort.UT.1 == 1|LEMIS.UT.1 == 1|
                                  CITES.UT.1 == 1|WiTIS.UT.1 == 1|MAN.Wiki.UT.1 == 1,
                                1, 0),
            food.an.2 = ifelse(IUCN.UT.2.sim == 1, 1, 0),
            med.3 = ifelse(IUCN.UT.3.sim == 1|SpUD.UT.3 == 1|MAN.Wiki.UT.3 == 1|
                             LEMIS.UT.3 == 1|CITES.UT.3 == 1|WiTIS.UT.3 == 1, 1, 0),
            other.chem.6 = ifelse(IUCN.UT.6.sim == 1|LEMIS.UT.6 == 1|CITES.UT.6 == 1|
                                  WiTIS.UT.6 == 1, 1, 0),
            fuels.7 = ifelse(IUCN.UT.7.sim == 1, 1, 0),
            apparel.10 = ifelse(IUCN.UT.10.sim == 1|LEMIS.UT.10 == 1|
                                CITES.UT.10 == 1, 1, 0),
            other.household.11 = ifelse(IUCN.UT.11.sim == 1|LEMIS.UT.11 == 1|
                                        CITES.UT.11 == 1, 1, 0),
            jewellery.12 = ifelse(IUCN.UT.12.sim == 1|MAN.Wiki.UT.12 == 1|
                                    SpUD.UT.12 == 1|LEMIS.UT.12 == 1|
                                    CITES.UT.12 == 1|WiTIS.UT.12 == 1, 1, 0),
            pets.13 = ifelse(IUCN.UT.13.sim == 1|MAN.Wiki.UT.13 == 1|SpUD.UT.13 == 1|
                               Mort.UT.13 == 1, 1, 0),
            research.14 = ifelse(IUCN.UT.14.sim == 1, 1, 0),
            sport.15 = ifelse(IUCN.UT.15.sim == 1|MAN.Wiki.UT.15 == 1|
                              SpUD.UT.15 == 1|LEMIS.UT.15 == 1|
                              CITES.UT.15 == 1, 1, 0),
            ex.situ.16 = ifelse(IUCN.UT.16.sim == 1, 1, 0),
            other.16 = ifelse(IUCN.UT.17.sim == 1, 1, 0),
            no.purpose = ifelse(IUCN.UT.1.sim == 1|IUCN.UT.2.sim == 1|IUCN.UT.3.sim == 1|
                                  IUCN.UT.4.sim  == 1|IUCN.UT.5.sim == 1|IUCN.UT.6.sim == 1|
                                  IUCN.UT.7.sim == 1|IUCN.UT.8.sim == 1|IUCN.UT.9.sim == 1|
                                  IUCN.UT.10.sim == 1|IUCN.UT.11.sim == 1|IUCN.UT.12.sim == 1|
                                  IUCN.UT.13.sim == 1|IUCN.UT.14.sim == 1|IUCN.UT.15.sim == 1|
                                  IUCN.UT.16.sim == 1|IUCN.UT.17.sim == 1|IUCN.UT.18.sim == 1|
                                  SpUD.UT.1 == 1|SpUD.UT.3 == 1|
                                  SpUD.UT.12 == 1|SpUD.UT.13 == 1|SpUD.UT.15 == 1|
                                  BenLop.UT.1 == 1|Mort.UT.1 == 1|Mort.UT.13 == 1|
                                  WM.UT.1 == 1|LEMIS.UT.15 == 1|LEMIS.UT.14 == 1|
                                  LEMIS.UT.12 == 1|LEMIS.UT.10 == 1|LEMIS.UT.6 == 1|
                                  LEMIS.UT.8 == 1|LEMIS.UT.3 == 1|LEMIS.UT.11 == 1|
                                  LEMIS.UT.1 == 1|CITES.UT.15 == 1|CITES.UT.12 == 1|
                                  CITES.UT.8 == 1|CITES.UT.11 == 1|CITES.UT.10 == 1|
                                  CITES.UT.1 == 1|CITES.UT.3 == 1|CITES.UT.6 == 1|
                                  WiTIS.UT.12 == 1|WiTIS.UT.3 == 1|WiTIS.UT.6 == 1|
                                  MAN.Wiki.UT.1 == 1 |MAN.Wiki.UT.13 == 1 |
                                  MAN.Wiki.UT.15 == 1 |MAN.Wiki.UT.12 == 1 |
                                  MAN.Wiki.UT.3 == 1 |MAN.Wiki.UT.10 == 1, 0, 1),
            use = use,
            used.no.purpose = ifelse(no.purpose == 1 & use == 1, 1, 0))

colSums(use.df[,6:21])

## add class
mam.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.MAMMALIA.Jul25.csv"))
aves.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Jun25.csv"))
class.df <- rbind(mam.taxo %>% select(IUCN.name) %>% mutate(Class = "Mammalia"),
                  aves.taxo %>% select(IUCN.name) %>% mutate(Class = "Aves"))

use.df <- use.df %>% 
  left_join(class.df)

write.csv(use.df, paste0(data.path, "Outputs/use.dataset/aves.mam.full.uses.tidy.csv"))

## manual checking
t <- use.raw.wiki %>% select(IUCN.name, use, contains("15")) %>%
  filter(SpUD.UT.15 == 1 |LEMIS.UT.15 == 1 | CITES.UT.15 == 1 | LEMIS.UT.15  == 1 |
           IUCN.UT.15.sim == 1 | MAN.Wiki.UT.15 == 1 )
colSums(t[,4:8])
