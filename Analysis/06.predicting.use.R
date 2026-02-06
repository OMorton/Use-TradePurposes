library(randomForest)
library(ranger)
library(tidyverse)
library(caret)
source("functions.R")
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## Read in use and IUCN data ---------------------------------------------------
use.all <- read.csv(paste0(data.path, "Outputs/use.dataset/aves.mam.full.uses.tidy.Dec25.csv"))

# IUCN data
AVES.loc <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.location.Oct25.csv"))
AVES.realms <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.realms.Oct25.csv"))
AVES.hab <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.habitats.Oct25.csv"))

MAM.loc <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.location.MAMMALIA.Oct25.csv"))
MAM.realms <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.realms.MAMMALIA.Oct25.csv"))
MAM.hab <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.habitats.MAMMALIA.Oct25.csv"))

endemism <- rbind(AVES.loc, MAM.loc) %>% select(IUCN.name, is_endemic) %>%
  distinct() %>% group_by(IUCN.name) %>% filter(n()>1 & is_endemic == TRUE | n()==1)

realms <-  rbind(MAM.realms, AVES.realms) %>% select(IUCN.name, description) %>%
  mutate(pres = "1") %>% 
  # drop 1 bat with no known origin
  filter(!is.na(description)) %>%
  pivot_wider(names_from = description, values_from = pres) %>% 
  distinct() %>% mutate_all(~replace(., is.na(.), 0))

habs <- rbind(MAM.hab, AVES.hab) %>%
  mutate(lv1 = gsub("\\_.*","",code)) %>%
  group_by(IUCN.name) %>% summarise(hab.breadth = n_distinct(lv1))

## read in data - AVES ---------------------------------------------------------
## AVONET trait data
avonet.df <- read.csv(paste0(data.path,
                             "Data/Tobias.et.al.AVONET/AVONET.data.csv"))
avonet.df <- avonet.df %>% select(Species1, 
                                  Beak.Length_Culmen, Beak.Length_Nares,  Beak.Width,
                                  Beak.Depth, Tarsus.Length, Wing.Length, Kipps.Distance,
                                  Hand.Wing.Index, Tail.Length, Mass,
                                  Habitat.Density, Migration, Trophic.Level,
                                  Trophic.Niche, Primary.Lifestyle, Min.Latitude, 
                                  Max.Latitude, Centroid.Latitude, 
                                  Centroid.Longitude, Range.Size, Order1)

avonet.key.df <- read.csv(paste0(data.path,
                                "Data/Tobias.et.al.AVONET/IUCN.AVONET.taxo.match.csv")) %>% select(-X)


## Bird et al trait data
bird.df <- read.csv(paste0(data.path,
                              "Data/Bird.et.al.2020/GenLengths.Table.S4.csv"))
bird.df <- bird.df %>% select(Scientific.name, Adult.survival, Age.at.first.breeding,
                              Maximum.longevity, Z, GenLength)
BirdLH.match <- read.csv(paste0(data.path,
                               "Data/Bird.et.al.2020/IUCN.BirdLH.taxo.match.csv")) %>% select(-X)

## Santigelli et al colour data
sant.key <- read.csv(paste0(data.path, "Data/Santageli_et_al_Aesthetics/IUCN.Santageli.taxo.match.csv")) %>%
  select(-X, -match)
sant.df <- read.csv(paste0(data.path,
                                "Data/Santageli_et_al_Aesthetics/Santagelli_2023.csv"))

sant.df2 <- sant.df %>% select(sciName_ebird2021, sex, black, blueD, blueL, redD, redL, brownD, brownL,
                   greenD, greenL, greyD, greyL, purpleD, purpleL, rufousD, rufousL, white, yellow, n.loci) %>%
  # get colour for the most colourful sex per bird where multiple given
  # remaining are where m and f are equal so take one at random
  group_by(sciName_ebird2021) %>% 
  filter(n.loci ==max(n.loci)) %>%
  group_by(sciName_ebird2021) %>% slice_head() %>% select(-sex)

sant.key.df <- sant.key %>% left_join(sant.df2, by = c("sant_ebird2021"= "sciName_ebird2021"))


# use data 11,021 species
use.all.birds <- use.all %>% filter(Class == "Aves") %>%
  left_join(avonet.key.df) %>%
  left_join(BirdLH.match) %>%
  left_join(sant.key)



# travel time data
AVES.travel <- read.csv(paste0(data.path, "Data/Travel.time/travel.time.oct25.BIRDS.csv")) %>%
  rename("IUCN.name" = "SCI_NAME", "trav.t.95" = "X95.", "trav.t.5" = "X5.",
         "trav.t.mn" = "mn") %>%
  select(-X, -X97.5., -X2.5.)

## Read in data - MAMMALS ------------------------------------------------------

mam.ranges <- read.csv(paste0(data.path, "Data/IUCN.range/mam.range.area.oct25.csv")) %>%
  select(-X)

mam.travel.time <- read.csv(paste0(data.path, "Data/Travel.time/travel.time.oct25.MAM.csv")) %>%
  select(-X)

soria.df <- read.csv(paste0(data.path,
                            "Data/Soria.et.al.COMBINE/trait_data_imputed.csv"))

Soria.match <- read.csv(paste0(data.path,
                               "Data/Soria.et.al.COMBINE/IUCN.Soria.taxo.match.csv"))

soria.short <- soria.df %>% select(iucn2020_binomial, phylacine_binomial, adult_mass_g, brain_mass_g,
                                adult_body_length_mm, max_longevity_d,
                                age_first_reproduction_d, gestation_length_d,
                                litter_size_n, litters_per_year_n,
                                generation_length_d, dispersal_km,
                                det_diet_breadth_n, trophic_level, foraging_stratum,
                                biogeographical_realm)  %>% 
  mutate(trophic_level = case_when(trophic_level == 1 ~ "Herbivore",
                                   trophic_level == 2 ~ "Omnivore",
                                   trophic_level == 3 ~ "Carnivore",
                                   .default = NA)) %>%
  group_by(iucn2020_binomial)%>%
  # occasionally multiple older names collapse to a single current name
  # we take the nominate
  filter(n()==1 | iucn2020_binomial == phylacine_binomial|
           phylacine_binomial %in% c("Petinomys sagitta", "Lagothrix lagotricha") |
           phylacine_binomial == "Saguinus fuscicollis" & iucn2020_binomial == "Leontocebus fuscicollis")


# use data 5940 species (5806 with trait data)
use.all.mam <- use.all %>% filter(Class == "Mammalia") %>%
  select(-X) %>%
  left_join(Soria.match) %>%
  left_join(soria.short, by = c("Soria.sp" = "iucn2020_binomial")) %>%
  filter(Soria.sp != "MISSING") %>% # down to 5881
  left_join(realms) %>%
  left_join(mam.ranges) %>%
  left_join(endemism) %>%
  left_join(habs) %>%
  filter(!is.na(Range.Size)) %>% # down to 5810 
  left_join(mam.travel.time, by = c("IUCN.name" = "SCI_NAME"))

full.unknown.mam <- use.all %>% filter(Class == "Mammalia") %>% filter(used.no.purpose == 1)

## Format data - Mammals -------------------------------------------------------

## check and drop NAs (5377)
use.all.mam %>% summarise(across(everything(), ~ sum(is.na(.))))

use.full.clean <- use.all.mam %>% select(-dispersal_km, -biogeographical_realm) %>%
  filter(!if_any(everything(), is.na))

## remove species to be predicted
known.all.mam <- use.full.clean %>% filter(used.no.purpose == 0) # 4991
unknown.all.mam <- use.full.clean %>% filter(used.no.purpose == 1) # 594
write.csv(unknown.all.mam, paste0(data.path, "Outputs/RF/tuning/unknown.all.mam.v2.csv"))

## Check correlated variables
check.corr <- known.all.mam %>% select(
  # size
  adult_mass_g, brain_mass_g, adult_body_length_mm, max_longevity_d,
  age_first_reproduction_d, gestation_length_d, litter_size_n,
  litters_per_year_n, generation_length_d, det_diet_breadth_n, hab.breadth,
  Centroid.Latitude, Centroid.Longtitude, Range.Size, trophic_level, foraging_stratum,
  X95., X5., mn)

corr.df <- cor(check.corr[,unlist(lapply(check.corr, is.numeric))]) %>% as.data.frame() %>%
  rownames_to_column(var = "var2") %>% 
  pivot_longer(!"var2", names_to = "var1", values_to = "corr") %>%
  filter(var1 != var2)

## Format data - AVES ----------------------------------------------------------

## append trait data
use.all.birds.full <- use.all.birds %>% 
  left_join(avonet.df, by = c("AVONET.sp" = "Species1")) %>%
  left_join(bird.df, by = c("BirdLH.sp" = "Scientific.name")) %>%
  left_join(sant.df2, by = c("sant_ebird2021"= "sciName_ebird2021")) %>%
  left_join(realms) %>%
  left_join(AVES.travel) %>%
  left_join(endemism) %>%
  left_join(habs)

## remove species to be predicted
known.all.birds <- use.all.birds.full %>% filter(used.no.purpose == 0) # 9628
unknown.all.birds <- use.all.birds.full %>% filter(used.no.purpose == 1) # 1393
write.csv(unknown.all.birds, paste0(data.path, "Outputs/RF/tuning/unknown.all.birds.v2.csv"))

## check and drop NAs
known.all.birds %>% summarise(across(everything(), ~ sum(is.na(.))))
known.use.full.clean <- known.all.birds %>% filter(!if_any(everything(), is.na)) # 9312

## Check correlated variables
check.corr <- known.use.full.clean %>% select(
                 # size
                 Mass,
                 # beak
                 Beak.Length_Culmen, Beak.Length_Nares, Beak.Width,
                 Beak.Depth, 
                 # leg
                 Tarsus.Length,
                 # wing
                 Wing.Length, Kipps.Distance, Hand.Wing.Index,
                 # tail
                 Tail.Length, 
                 # diet and foraging
                 Trophic.Level, Primary.Lifestyle,
                 # range
                 is_endemic, Range.Size,
                 Min.Latitude, Max.Latitude,
                 Centroid.Latitude, Centroid.Longitude,
                 Palearctic, Neotropical, Antarctic, Australasian,
                 Indomalayan, Afrotropical, Nearctic, Oceanian,
                 trav.t.95, trav.t.5, trav.t.mn,hab.breadth,
                 # taxa
                 orderName,
                 # demographic traits
                 Adult.survival, Age.at.first.breeding, Maximum.longevity, GenLength,
                 # colour
                 black, blueD, blueL, redD, redL, brownD, brownL,
                 greenD, greenL, greyD, greyL, purpleD, purpleL, rufousD, rufousL,
                 white, yellow, n.loci
                 ) %>%
  ## relative
  mutate(rel.beak = Beak.Length_Culmen/Mass,
         rel.tail = Tail.Length/Mass)

corr.df <- cor(check.corr[,unlist(lapply(check.corr, is.numeric))]) %>% as.data.frame() %>%
  rownames_to_column(var = "var2") %>% 
  pivot_longer(!"var2", names_to = "var1", values_to = "corr") %>%
  filter(var1 != var2)

## Tuning use models - AVES ----------------------------------------------------
set.seed(212)
use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
pred.out.ls <- list()
i <- "pets.13"

for (i in use.ls) {
  cat(i, "\n")
  

  known.use.full.clean2 <- known.use.full.clean %>% select(all_of(i), 
                                   # size
                                   Mass,
                                   # beak
                                   Beak.Length_Culmen, Beak.Length_Nares, Beak.Width,
                                   Beak.Depth, 
                                   # leg
                                   Tarsus.Length,
                                   # wing
                                   Wing.Length, Kipps.Distance, Hand.Wing.Index,
                                   # tail
                                   Tail.Length, 
                                   # diet and foraging
                                   Trophic.Level, Primary.Lifestyle,
                                   # range
                                   is_endemic, Range.Size, hab.breadth,
                                   Min.Latitude, Max.Latitude,
                                   Centroid.Latitude, Centroid.Longitude,
                                   Palearctic, Neotropical, Antarctic, Australasian,
                                   Indomalayan, Afrotropical, Nearctic, Oceanian,
                                   trav.t.95, trav.t.5, trav.t.mn,
                                   # taxa
                                   orderName,
                                   # demographic traits
                                   Adult.survival, Age.at.first.breeding, Maximum.longevity, GenLength,
                                   # colour
                                   black, blueD, blueL, redD, redL, brownD, brownL,
                                   greenD, greenL, greyD, greyL, purpleD, purpleL, rufousD, rufousL,
                                   white, yellow, n.loci) %>%
    ## relative
    mutate(rel.beak = Beak.Length_Culmen/Mass,
           rel.tail = Tail.Length/Mass) %>%
    mutate(!!i := ifelse(!!sym(i), "yes", "no")) %>%
    mutate(Order = "1") %>%
    pivot_wider(names_from = orderName, values_from = Order, values_fill = "0") %>%
    # drop orders with less than 20 species (12 orders to drop)
    select(-any_of(c("LEPTOSOMIFORMES", "OPISTHOCOMIFORMES", "CARIAMIFORMES", "EURYPYGIFORMES",
           "GAVIIFORMES", "MESITORNITHIFORMES", "PHAETHONTIFORMES", "COLIIFORMES",
           "PHOENICOPTERIFORMES", "CATHARTIFORMES", "PODICIPEDIFORMES", "PTEROCLIFORMES"))) %>%
    mutate(is_endemic = ifelse(is_endemic == TRUE, "1", "0")) %>%
    # remove highly corr variables
    select(-Min.Latitude, -Max.Latitude, -GenLength, 
                      -Beak.Length_Nares, -Beak.Width, -Wing.Length, -trav.t.5,
                      -Maximum.longevity, -rel.beak)
  
  train.index <- createDataPartition(known.use.full.clean2[[i]], p = 0.75, list = FALSE)
  
  df.train <- known.use.full.clean2[train.index, ]
  df.test <- known.use.full.clean2[-train.index, ] 

  ## Tune and optimize model per use
  rf.all.i <- data.frame()

    for(j in c(100, 500, 1000, 1500)){
      ntree <- j 
      cat(i, "use and ", ntree, "\n")
      
      fit <- train(
        formula(paste(i, "~ .")),
        data = df.train, method = "ranger",
        trControl = trainControl(
          method = "cv", number = 5,
          verboseIter = T, classProbs = TRUE,
          # custom verbose summary
          summaryFunction = mySummary), 
        tuneGrid = expand.grid(mtry = c(1, 3,5, 10, 15, 20, 30, 40),
                               min.node.size = 5, splitrule = c("gini", "extratrees")),
        importance = "permutation",
        num.trees = ntree) 
      
      results <- fit$results %>%
        mutate(ntree = ntree, .before = mtry, use = i)
      rf.all.i <- rbind(rf.all.i, results)
    }
  pred.out.ls[[paste0("train.", i)]] <- df.train 
  pred.out.ls[[paste0("test", i)]] <- df.test  
  pred.out.ls[[paste0("mod.results.", i)]] <- rf.all.i  
  write.csv(rf.all.i, paste0(data.path, "Outputs/RF/tuning/", i, "tuning.grid.csv"))
}

save(pred.out.ls, file = paste0(data.path, "Outputs/RF/tuning/all.tuning.data.Dec25.permutation.rdata"))

## Tuning use models - MAMMALIA ------------------------------------------------
set.seed(212)
use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
pred.out.ls <- list()
i <- "pets.13"

f <- known.all.mam %>% group_by(orderName) %>% tally()
for (i in use.ls) {
  cat(i, "\n")
  
  
  known.use.full.clean2 <- known.all.mam %>% select(all_of(i), 
                                                    adult_mass_g, adult_body_length_mm, max_longevity_d,
                                                    age_first_reproduction_d, gestation_length_d, litter_size_n,
                                                    litters_per_year_n, generation_length_d, det_diet_breadth_n,
                                                    hab.breadth, trophic_level, foraging_stratum,
                                                    # range
                                                    Centroid.Latitude, Centroid.Longtitude, Range.Size, is_endemic,
                                                    # trav
                                                    X95., X5., mn,
                                                    # realm
                                                   Palearctic, Neotropical, Antarctic, Australasian,
                                                   Indomalayan, Afrotropical, Nearctic, Oceanian,
                                                   # taxa
                                                   orderName) %>%
    mutate(!!i := ifelse(!!sym(i), "yes", "no") 
           #rel_brain_mass = brain_mass_g/adult_mass_g
           ) %>%
    mutate(Order = "1") %>%
    pivot_wider(names_from = orderName, values_from = Order, values_fill = "0") %>%
    # drop orders with less than 20 species (12 orders to drop)
    select(-any_of(c("PROBOSCIDEA", "TUBULIDENTATA", "DERMOPTERA", "NOTORYCTEMORPHIA",
                     "SIRENIA", "HYRACOIDEA", "MONOTREMATA", "PAUCITUBERCULATA",
                     "PHOLIDOTA", "PILOSA", "SCANDENTIA", "PERAMELEMORPHIA",
                     "PERISSODACTYLA", "MACROSCELIDEA"))) %>%
    mutate(is_endemic = ifelse(is_endemic == TRUE, "1", "0")) %>%
    # remove highly corr variables
    select(-generation_length_d, -adult_body_length_mm, ,-age_first_reproduction_d,
           -X5., -gestation_length_d)
  
  train.index <- createDataPartition(known.use.full.clean2[[i]], p = 0.75, list = FALSE)
  
  df.train <- known.use.full.clean2[train.index, ]
  df.test <- known.use.full.clean2[-train.index, ] 
  
  ## Tune and optimize model per use
  rf.all.i <- data.frame()
  
  for(j in c(100, 500, 1000, 1500)){
    ntree <- j 
    cat(i, "use and ", ntree, "\n")
    
    fit <- train(
      formula(paste(i, "~ .")),
      data = df.train, method = "ranger",
      trControl = trainControl(
        method = "cv", number = 5,
        verboseIter = T, classProbs = TRUE,
        # custom verbose summary
        summaryFunction = mySummary), 
      tuneGrid = expand.grid(mtry = c(1, 3,5, 10, 15, 20),
                             min.node.size = 5, splitrule = c("gini", "extratrees")),
      importance = "permutation",
      num.trees = ntree) 
    
    results <- fit$results %>%
      mutate(ntree = ntree, .before = mtry, use = i)
    rf.all.i <- rbind(rf.all.i, results)
  }
  pred.out.ls[[paste0("train.", i)]] <- df.train 
  pred.out.ls[[paste0("test", i)]] <- df.test  
  pred.out.ls[[paste0("mod.results.", i)]] <- rf.all.i  
  write.csv(rf.all.i, paste0(data.path, "Outputs/RF/tuning/", i, "tuning.grid.MAM.permutation.csv"))
}

save(pred.out.ls, file = paste0(data.path, "Outputs/RF/tuning/all.tuning.data.MAM.Dec25.permutation.rdata"))

## Fitting final models - AVES -------------------------------------------------

# read in test, train data and tuning grid
load(paste0(data.path, "Outputs/RF/tuning/all.tuning.data.Dec25.permutation.rdata"))

best <- pred.out.ls[grepl("mod.results", names(pred.out.ls))]
best.all <- do.call(rbind, best) %>% group_by(use) %>% filter(Accuracy == max(Accuracy))

use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
stats.out.ls <- list()

i <- "food.hum.1"
# Loop through each use refitting the tuned model, optimizinf thresholds and then
# summarising final fit statistics using the held out test set.
for (i in use.ls) {
  cat(i, "\n")
  
  # get data
  test.i <- pred.out.ls[[paste0("test",i)]]
  train.i <- pred.out.ls[[paste0("train.",i)]]
  tuning.grid.i <- pred.out.ls[[paste0("mod.results.",i)]]
  opt.i <- (tuning.grid.i %>% filter(Accuracy == max(Accuracy)))[1,]
  
  # fit final model to train data again
  fit.i <- train(
    formula(paste(i, "~ .")),
    data = train.i, method = "ranger",
    trControl = trainControl(
      method = "cv", number = 5,
      verboseIter = T, classProbs = TRUE,
      summaryFunction = mySummary), 
    tuneGrid = expand.grid(mtry = opt.i$mtry,
                           min.node.size = 5, splitrule = opt.i$splitrule),
    importance = "impurity",
    num.trees = opt.i$ntree)
  
  # optimize the probability threshold
  rf.pred.i <- predict(fit.i, train.i, type = "prob")
  # optimize for accuracy
  thresholds <- seq(0.1, 0.9, by = 0.05)
  accs <- sapply(thresholds, function(t) {
    preds <- rf.pred.i %>% summarise(preds = as.factor(ifelse(yes >= t, "yes", "no")))
    mean(preds$preds == train.i[[i]])
  })
  best.thresh <- thresholds[which.max(accs)]
  
  # using optimum threshold to predict for test data
  rf.pred.i <- predict(fit.i, test.i, type = "prob") %>% 
    summarise(pred =as.factor(ifelse(yes >= best.thresh, "yes", "no")))
  # Generate final fit statistics
  opt.matrix <- confusionMatrix(rf.pred.i$pred, as.factor(test.i[[i]]))

  # repeat for standard threshold
  # using optimum threshold to predict for test data
  rf.pred.i <- predict(fit.i, test.i)
  # Generate final fit statistics
  def.matrix <- confusionMatrix(rf.pred.i, as.factor(test.i[[i]]))
  
  # write out
  stats.out.ls[[paste0("final.mod.",i)]] <- fit.i
  stats.out.ls[[paste0("optimized.matrix.",i)]] <- opt.matrix
  stats.out.ls[[paste0("default.matrix.",i)]] <- def.matrix
  stats.out.ls[[paste0("opt.threshold.",i)]] <- best.thresh
  
}

save(stats.out.ls, file = paste0(data.path, "Outputs/RF/tuning/all.test.statistics.Dec25.permutation.rdata"))

load(paste0(data.path, "Outputs/RF/tuning/all.test.statistics.Dec25.permutation.rdata"))
names(stats.out.ls)
# top 3 work beyond that so few use cases prediction fails
stats.out.ls$optimized.matrix.pets.13
stats.out.ls$opt.threshold.pets.13

stats.out.ls$optimized.matrix.food.hum.1
stats.out.ls$opt.threshold.food.hum.1

stats.out.ls$optimized.matrix.sport.15
stats.out.ls$opt.threshold.sport.15

stats.out.ls$optimized.matrix.jewellery.12
stats.out.ls$opt.threshold.jewellery.12

stats.out.ls$optimized.matrix.apparel.10

stats.out.ls$optimized.matrix.med.3

## Get variable importance - AVES ----------------------------------------------
bird.food.1.vi <- stats.out.ls$final.mod.food.hum.1 %>% 
  vip::vi() %>% mutate(use = "food.hum.1")

bird.pets.13.vi <- stats.out.ls$final.mod.pets.13 %>% 
  vip::vi() %>% mutate(use = "pets.13")

bird.sport.15.vi <- stats.out.ls$final.mod.sport.15 %>% 
  vip::vi() %>% mutate(use = "sport.15")

var.imp.birds <- rbind(bird.food.1.vi, bird.pets.13.vi, bird.sport.15.vi)
save(var.imp.birds, file = paste0(data.path, "Outputs/RF/pdp/vi.all.birds.permutation.rdata"))

## Fitting final models - MAMMALIA ---------------------------------------------

# read in test, train data and tuning grid
load(paste0(data.path, "Outputs/RF/tuning/all.tuning.data.MAM.Dec25.permutation.rdata"))

best <- pred.out.ls[grepl("mod.results", names(pred.out.ls))]
best.all <- do.call(rbind, best) %>% group_by(use) %>% filter(Accuracy == max(Accuracy))

use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
stats.out.ls <- list()

i <- "pets.13"
# Loop through each use refitting the tuned model, optimizinf thresholds and then
# summarising final fit statistics using the held out test set.
for (i in use.ls) {
  cat(i, "\n")
  
  # get data
  test.i <- pred.out.ls[[paste0("test",i)]]
  train.i <- pred.out.ls[[paste0("train.",i)]]
  tuning.grid.i <- pred.out.ls[[paste0("mod.results.",i)]]
  opt.i <- (tuning.grid.i %>% filter(Accuracy == max(Accuracy)))[1,]
  
  #wts.i <- train.i %>% group_by(.data[[i]]) %>% tally()
  
  # fit final model to train data again
  fit.i <- train(
    formula(paste(i, "~ .")),
    data = train.i, method = "ranger",
    #class.weights = c((wts.i[1,2]/sum(wts.i$n))$n, (wts.i[2,2]/sum(wts.i$n))$n),
    trControl = trainControl(
      method = "cv", number = 5,
      verboseIter = T, classProbs = TRUE,
      summaryFunction = mySummary), 
    tuneGrid = expand.grid(mtry = opt.i$mtry,
                           min.node.size = 5, splitrule = opt.i$splitrule),
    importance = "permutation",
    num.trees = opt.i$ntree)
  
  # optimize the probability threshold
  rf.pred.i <- predict(fit.i, train.i, type = "prob")
  # optimize for accuracy
  thresholds <- seq(0.1, 0.9, by = 0.05)
  accs <- sapply(thresholds, function(t) {
    preds <- rf.pred.i %>% summarise(preds = as.factor(ifelse(yes >= t, "yes", "no")))
    mean(preds$preds == train.i[[i]])
  })
  best.thresh <- thresholds[which.max(accs)]
  
  # using optimum threshold to predict for test data
  rf.pred.i <- predict(fit.i, test.i, type = "prob") %>% 
    summarise(pred =as.factor(ifelse(yes >= best.thresh, "yes", "no")))
  # Generate final fit statistics
  opt.matrix <- confusionMatrix(rf.pred.i$pred, as.factor(test.i[[i]]))
  
  # repeat for standard threshold
  # using optimum threshold to predict for test data
  rf.pred.i <- predict(fit.i, test.i)
  # Generate final fit statistics
  def.matrix <- confusionMatrix(rf.pred.i, as.factor(test.i[[i]]))
  
  # write out
  stats.out.ls[[paste0("final.mod.",i)]] <- fit.i
  stats.out.ls[[paste0("optimized.matrix.",i)]] <- opt.matrix
  stats.out.ls[[paste0("default.matrix.",i)]] <- def.matrix
  stats.out.ls[[paste0("opt.threshold.",i)]] <- best.thresh
  
}

#save(stats.out.ls, file = paste0(data.path, "Outputs/RF/tuning/all.test.statistics.MAM.Dec25.permutation.rdata"))

load(paste0(data.path, "Outputs/RF/tuning/all.test.statistics.MAM.Dec25.permutation.rdata"))
names(stats.out.ls)
# top 3 work beyond that so few use cases prediction fails
stats.out.ls$optimized.matrix.pets.13
stats.out.ls$opt.threshold.pets.13

stats.out.ls$optimized.matrix.food.hum.1
stats.out.ls$opt.threshold.food.hum.1

stats.out.ls$optimized.matrix.sport.15
stats.out.ls$opt.threshold.sport.15

stats.out.ls$optimized.matrix.apparel.10

stats.out.ls$optimized.matrix.jewellery.12
stats.out.ls$opt.threshold.jewellery.12


stats.out.ls$optimized.matrix.med.3

## Get variable importance - MAMMALIA ------------------------------------------
mam.food.1.vi <- stats.out.ls$final.mod.food.hum.1 %>% 
  vip::vi() %>% mutate(use = "food.hum.1")

mam.sport.15.vi <- stats.out.ls$final.mod.sport.15 %>% 
  vip::vi() %>% mutate(use = "sport.15")

mam.apparel.10.vi <- stats.out.ls$final.mod.apparel.10 %>% 
  vip::vi() %>% mutate(use = "apparel.10")

mam.pets.13.vi <- stats.out.ls$final.mod.pets.13 %>% 
  vip::vi() %>% mutate(use = "pets.13")

mam.medicine.3.vi <- stats.out.ls$final.mod.med.3 %>% 
  vip::vi() %>% mutate(use = "medicine.3")

var.imp.mam <- rbind(mam.food.1.vi, mam.sport.15.vi, mam.apparel.10.vi,
                     mam.pets.13.vi, mam.medicine.3.vi)
save(var.imp.mam, file = paste0(data.path, "Outputs/RF/pdp/vi.all.mam.Dec25.permutation.rdata"))



## Collating model summary -----------------------------------------------------
load(paste0(data.path, "Outputs/RF/tuning/all.test.statistics.Dec25.permutation.rdata"))
aves.fit.out <- stats.out.ls
load(paste0(data.path, "Outputs/RF/tuning/all.test.statistics.MAM.Dec25.permutation.rdata"))
mam.fit.out <- stats.out.ls

use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
i <- "food.hum.1"

aves.all.fit.stats <- data.frame()
for (i in use.ls) {
  acc.i <- aves.fit.out[[paste0("optimized.matrix.", i)]]$overal["Accuracy"][[1]]
  kappa.i <- aves.fit.out[[paste0("optimized.matrix.", i)]]$overal["Kappa"][[1]]
  acc.nir.i <- aves.fit.out[[paste0("optimized.matrix.", i)]]$overal["AccuracyPValue"][[1]]
  
  sens.i <- aves.fit.out[[paste0("optimized.matrix.", i)]]$byClass["Sensitivity"][[1]]
  spec.i <- aves.fit.out[[paste0("optimized.matrix.", i)]]$byClass["Specificity"][[1]]
  bal.acc.i <- aves.fit.out[[paste0("optimized.matrix.", i)]]$byClass["Balanced Accuracy"][[1]]
  
  use.i <- data.frame(Class = "Aves", Use = i,
    Accuracy = acc.i, Specificity = spec.i, Sensitivity = sens.i,
    Balanced.accuracy = bal.acc.i, `Acc>NIR` = acc.nir.i,
    Kappa = kappa.i) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  aves.all.fit.stats <- rbind(aves.all.fit.stats, use.i)
}

## mam
mam.all.fit.stats <- data.frame()
for (i in use.ls) {
  acc.i <- mam.fit.out[[paste0("optimized.matrix.", i)]]$overal["Accuracy"][[1]]
  kappa.i <- mam.fit.out[[paste0("optimized.matrix.", i)]]$overal["Kappa"][[1]]
  acc.nir.i <- mam.fit.out[[paste0("optimized.matrix.", i)]]$overal["AccuracyPValue"][[1]]
  
  sens.i <- mam.fit.out[[paste0("optimized.matrix.", i)]]$byClass["Sensitivity"][[1]]
  spec.i <- mam.fit.out[[paste0("optimized.matrix.", i)]]$byClass["Specificity"][[1]]
  bal.acc.i <- mam.fit.out[[paste0("optimized.matrix.", i)]]$byClass["Balanced Accuracy"][[1]]
  
  use.i <- data.frame(Class = "Mammalia", Use = i,
                      Accuracy = acc.i, Specificity = spec.i, Sensitivity = sens.i,
                      Balanced.accuracy = bal.acc.i, `Acc>NIR` = acc.nir.i,
                      Kappa = kappa.i) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  mam.all.fit.stats <- rbind(mam.all.fit.stats, use.i)
}

write.csv(mam.all.fit.stats, 
          paste0(data.path, "Outputs/RF/tuning/summary.test.statistics.MAM.Dec25.permutation.csv"))
write.csv(aves.all.fit.stats, 
          paste0(data.path, "Outputs/RF/tuning/summary.test.statistics.AVES.Dec25.permutation.csv"))

## Get partial effects plots - AVES --------------------------------------------

library(DALEX)
library(DALEXtra)

load(paste0(data.path, "Outputs/RF/tuning/all.tuning.data.Dec25.permutation.rdata"))
load(paste0(data.path, "Outputs/RF/tuning/all.test.statistics.Dec25.permutation.rdata"))

uses <- c("food.hum.1", "pets.13", "sport.15")
pdp.df.list<- list()
i <- "food.hum.1"

for (i in uses) {
  
  cat(i, "\n")
  train.i <- pred.out.ls[[paste0("train.", i)]]
  mod.i <- stats.out.ls[[paste0("final.mod.", i)]]
  
  # build explainer
  explainer.rf <- explain(mod.i, data = dplyr::select(train.i, -any_of(c(paste0(i)))), 
    y = dplyr::select(train.i, any_of(c(paste0(i)))), label = "random forest")
  
  cat.vars <- dplyr::select(train.i, -any_of(c(paste0(i)))) %>%
    select(where(~ !is.numeric(.))) %>%
    names()
  num.vars <- dplyr::select(train.i, -any_of(c(paste0(i)))) %>%
    select(where(~ is.numeric(.))) %>%
    names()
  
  # model pdp
  cat.pdp.est <- model_profile(explainer.rf, variables = cat.vars)
  num.pdp.est <- model_profile(explainer.rf, variables = num.vars)
  
  cat.cp.profs <- data.frame(cat.pdp.est$cp_profiles) %>%
    rename("Variable" = "X_vname_", "Y"= "X_yhat_")
  num.cp.profs <- data.frame(num.pdp.est$cp_profiles) %>%
    rename("Variable" = "X_vname_", "Y"= "X_yhat_")
  
  cp.profs <- rbind(cat.cp.profs, num.cp.profs)
  
  cat.aggr.profs <- data.frame(cat.pdp.est$agr_profiles) %>% 
    rename("Variable" = "X_vname_", "X"="X_x_", "Y"= "X_yhat_") %>% 
    select(Variable, X, Y) %>% mutate(use = i)
  num.aggr.profs <- data.frame(num.pdp.est$agr_profiles) %>% 
    rename("Variable" = "X_vname_", "X"="X_x_", "Y"= "X_yhat_") %>% 
    select(Variable, X, Y) %>% mutate(use = i)
  
  # write out
  pdp.df.list[[paste0("cp.profs.",i)]] <- cp.profs
  pdp.df.list[[paste0("num.aggr.profs.",i)]] <- num.aggr.profs
  pdp.df.list[[paste0("cat.aggr.profs.",i)]] <- cat.aggr.profs
  
}
 
save(pdp.df.list, file = paste0(data.path, "Outputs/RF/pdp/pdp.all.birds.Dec25.permutation.rdata"))




## Get partial effects plots - MAMMALIA --------------------------------------------

library(DALEX)
library(DALEXtra)

load(paste0(data.path, "Outputs/RF/tuning/all.tuning.data.MAM.Dec25.permutation.rdata"))
load(paste0(data.path, "Outputs/RF/tuning/all.test.statistics.MAM.Dec25.permutation.rdata"))
load(paste0(data.path, "Outputs/RF/pdp/vi.all.mam.Dec25.permutation.rdata"))

uses <- c("food.hum.1", "sport.15", "med.3", "apparel.10", "pets.13")
pdp.df.list<- list()
i <- "food.hum.1"

for (i in uses) {
  
  cat(i, "\n")
  train.i <- pred.out.ls[[paste0("train.", i)]]
  mod.i <- stats.out.ls[[paste0("final.mod.", i)]]
  
  # build explainer
  explainer.rf <- explain(mod.i, data = dplyr::select(train.i, -any_of(c(paste0(i)))), 
                          y = dplyr::select(train.i, any_of(c(paste0(i)))), label = "random forest")
  
  cat.vars <- dplyr::select(train.i, -any_of(c(paste0(i)))) %>%
    select(where(~ !is.numeric(.))) %>%
    names()
  num.vars <- dplyr::select(train.i, -any_of(c(paste0(i)))) %>%
    select(where(~ is.numeric(.))) %>%
    names()
  
  # model pdp
  cat.pdp.est <- model_profile(explainer.rf, variables = cat.vars)
  num.pdp.est <- model_profile(explainer.rf, variables = num.vars)
  
  cat.cp.profs <- data.frame(cat.pdp.est$cp_profiles) %>%
    rename("Variable" = "X_vname_", "Y"= "X_yhat_")
  num.cp.profs <- data.frame(num.pdp.est$cp_profiles) %>%
    rename("Variable" = "X_vname_", "Y"= "X_yhat_")
  
  cp.profs <- rbind(cat.cp.profs, num.cp.profs)
  
  cat.aggr.profs <- data.frame(cat.pdp.est$agr_profiles) %>% 
    rename("Variable" = "X_vname_", "X"="X_x_", "Y"= "X_yhat_") %>% 
    select(Variable, X, Y) %>% mutate(use = i)
  num.aggr.profs <- data.frame(num.pdp.est$agr_profiles) %>% 
    rename("Variable" = "X_vname_", "X"="X_x_", "Y"= "X_yhat_") %>% 
    select(Variable, X, Y) %>% mutate(use = i)
  
  # write out
  pdp.df.list[[paste0("cp.profs.",i)]] <- cp.profs
  pdp.df.list[[paste0("num.aggr.profs.",i)]] <- num.aggr.profs
  pdp.df.list[[paste0("cat.aggr.profs.",i)]] <- cat.aggr.profs}

save(pdp.df.list, file = paste0(data.path, "Outputs/RF/pdp/pdp.all.mam.Dec25.permutation.rdata"))

