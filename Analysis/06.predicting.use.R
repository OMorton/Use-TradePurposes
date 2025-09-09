library(randomForest)
library(ranger)
library(tidyverse)
library(caret)
source("functions.R")
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## read in data ----------------------------------------------------------------
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
                                  Centroid.Longitude, Range.Size)

avonet.key.df <- read.csv(paste0(data.path,
                                "Data/Tobias.et.al.AVONET/IUCN.AVONET.taxo.match.csv")) %>% select(-X)


## Bird et al trait data
bird.df <- read.csv(paste0(data.path,
                              "Data/Bird.et.al.2020/GenLengths.Table.S4.csv"))
bird.df <- bird.df %>% select(Scientific.name, Adult.survival, Age.at.first.breeding,
                              Maximum.longevity, Z, GenLength)
BirdLH.match <- read.csv(paste0(data.path,
                               "Data/Bird.et.al.2020/IUCN.BirdLH.taxo.match.csv")) %>% select(-X)

# use data 11,031 species
use.all <- read.csv(paste0(data.path, "Outputs/use.dataset/aves.mam.full.uses.tidy.csv"))
use.all.birds <- use.all %>% filter(Class == "Aves") %>%
  left_join(avonet.key.df) %>%
  left_join(BirdLH.match)

# IUCN data
AVES.loc <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.location.Jun25.csv"))
AVES.realms <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.realms.Jun25.csv"))
MAM.loc <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.location.MAMMALIA.Jul25.csv"))
MAM.realms <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.realms.MAMMALIA.Jul25.csv"))

endemism <- rbind(AVES.loc, MAM.loc) %>% select(IUCN.name, is_endemic) %>% distinct()
realms <-  rbind(MAM.realms, AVES.realms) %>% select(IUCN.name, description) %>%
  mutate(pres = "1") %>% 
  # drop 1 bat with no known origin
  filter(!is.na(description)) %>%
  pivot_wider(names_from = description, values_from = pres) %>% 
  distinct() %>% mutate_all(~replace(., is.na(.), 0))

# travel time data
AVES.travel <- read.csv(paste0(data.path, "Data/Travel.time/travel.time.csv")) %>%
  rename("IUCN.name" = "SCI_NAME", "trav.t.95" = "X95.", "trav.t.5" = "X5.",
         "trav.t.mn" = "mn") %>%
  select(-X, -X97.5., -X2.5.)

## Format data -----------------------------------------------------------------

## remove species to be predicted
known.all.birds <- use.all.birds %>% filter(used.no.purpose == 0) # 9669
unknown.all.birds <- use.all.birds %>% filter(used.no.purpose == 1) # 1362

## append trait data
known.use.full <- known.all.birds %>% 
  left_join(avonet.df, by = c("AVONET.sp" = "Species1")) %>%
  left_join(bird.df, by = c("BirdLH.sp" = "Scientific.name")) %>%
  left_join(endemism)%>%
  left_join(realms) %>%
  left_join(AVES.travel)

## check and drop NAs
known.use.full %>% summarise(across(everything(), ~ sum(is.na(.))))
known.use.full.clean <- known.use.full %>% filter(!if_any(everything(), is.na))

## Check correlated variables
check.corr <- known.use.full.clean %>% select(
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
                 trav.t.95, trav.t.5, trav.t.mn,
                 # taxa
                 orderName,
                 # demographic traits
                 Adult.survival, Age.at.first.breeding, Maximum.longevity, GenLength)

corr.df <- cor(check.corr[,unlist(lapply(check.corr, is.numeric))]) %>% as.data.frame() %>%
  rownames_to_column(var = "var2") %>% 
  pivot_longer(!"var2", names_to = "var1", values_to = "corr") %>%
  filter(var1 != var2)

## Tuning use models --------------------------------------------------------------
set.seed(212)
use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
pred.out.ls <- list()
i <- "food.hum.1"

for (i in use.ls) {
  cat(i, "\n")
  
  train.index <- createDataPartition(known.use.full.clean[[i]], p = 0.75, list = FALSE)
  
  df.train <- known.use.full.clean[train.index, ]
  df.test <- known.use.full.clean[-train.index, ] 
  
  df.train.clean <- df.train %>% select(all_of(i), 
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
                                   trav.t.95, trav.t.5, trav.t.mn,
                                   # taxa
                                   orderName,
                                   # demographic traits
                                   Adult.survival, Age.at.first.breeding, Maximum.longevity, GenLength) %>%
    mutate(!!i := ifelse(!!sym(i), "yes", "no")) %>%
    mutate(Order = 1) %>%
    pivot_wider(names_from = orderName, values_from = Order, values_fill = 0) %>%
    # drop orders with less than 20 species (12 orders to drop)
    select(-LEPTOSOMIFORMES, -OPISTHOCOMIFORMES, -CARIAMIFORMES, -EURYPYGIFORMES,
           -GAVIIFORMES, -MESITORNITHIFORMES, -PHAETHONTIFORMES, -COLIIFORMES,
           -PHOENICOPTERIFORMES, -CATHARTIFORMES, -PODICIPEDIFORMES, -PTEROCLIFORMES) %>%
    # remove highly corr variables
    select(-Min.Latitude, -Max.Latitude, -GenLength, 
                      -Beak.Length_Nares, -Beak.Width, -Wing.Length, -trav.t.5,
                      -Maximum.longevity)

  ## Tune and optimize model per use
  rf.all.i <- data.frame()

    for(j in c(100, 500, 1000, 1500)){
      ntree <- j 
      cat(i, "use and ", ntree, "\n")
      
      fit <- train(
        formula(paste(i, "~ .")),
        data = df.train.clean, method = "ranger",
        trControl = trainControl(
          method = "cv", number = 5,
          verboseIter = T, classProbs = TRUE,
          summaryFunction = mySummary), 
        tuneGrid = expand.grid(mtry = c(1, 3,5, 10, 15, 20, 30, 40),
                               min.node.size = 5, splitrule = c("gini", "extratrees")),
        importance = "impurity",
        num.trees = ntree) 
      
      results <- fit$results %>%
        mutate(ntree = ntree, .before = mtry, use = i)
      rf.all.i <- rbind(rf.all.i, results)
    }
  pred.out.ls[[paste0("train.", i)]] <- df.train.clean 
  pred.out.ls[[paste0("test", i)]] <- df.test  
  pred.out.ls[[paste0("mod.results.", i)]] <- rf.all.i  
  write.csv(rf.all.i, paste0(data.path, "Outputs/RF/tuning/", i, "tuning.grid.csv"))
}

save(pred.out.ls, file = paste0(data.path, "Outputs/RF/tuning/all.tuning.data.v2.rdata"))

## Fitting final models ------------------------------------------------------------

# read in test, train data and tuning grid
load(paste0(data.path, "Outputs/RF/tuning/all.tuning.data.v1.rdata"))
 
use.ls <- c("food.hum.1", "med.3", "apparel.10", "jewellery.12", "pets.13", "sport.15")
stats.out.ls <- list()

i <- "food.hum.1"

# Loop through each use refitting the tuned model, optimizinf thresholds and then
# summarising final fit statistics using the held out test set.
for (i in use.ls) {
  cat(i, "\n")
  
  # get data
  test.i <- pred.out.ls[[paste0("test",i)]] %>%
    mutate(!!i := ifelse(!!sym(i), "yes", "no"))
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
  best_thresh <- thresholds[which.max(accs)]
  
  # using optimum threshold to predict for test data
  rf.pred.i <- predict(fit.i, test.i, type = "prob") %>% 
    summarise(pred =as.factor(ifelse(yes >= best_thresh, "yes", "no")))
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
}
  