library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(tidyterra)
library(ggpubr)
## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

use.all <- read.csv(paste0(data.path, "Outputs/use.dataset/aves.mam.full.uses.tidy.csv"))
use.all.birds <- use.all %>% filter(Class == "Aves")
use.all.mam <- use.all %>% filter(Class == "Mammalia")

## Simple summaries ------------------------------------------------------------

use.long <- use.all %>% 
  select(-X) %>%
  pivot_longer(!c("IUCN.name", "common.name", "Class", "familyName", "orderName",
                        "status"), names_to = "use.group", values_to = "pres") %>%
  mutate(use.coarse = ifelse(use.group %in% c("ex.situ.16", "food.an.2",
                                              "fuels.7", "other.16", "other.chem.6",
                                              "other.household.11", "research.14"),
                             "other_known", use.group),
         status = factor(status, levels = c("EW", "CR", "EN", "VU", "NT", "LC", "DD")))

use.status.sum <- use.long %>%
  group_by(Class,use.coarse, status)  %>% 
  summarise(sp.count = sum(pres)) %>% 
  filter(use.coarse != "no.purpose")

use.long %>%
  group_by(use.coarse)  %>% 
  summarise(sp.count = sum(pres)) %>% 
  filter(use.coarse != "no.purpose")

use.ovr <- use.status.sum %>% filter(use.coarse == "use") %>%
  group_by(Class,use.coarse) %>% summarise(sp.count = sum(sp.count))
use.unknown <- use.status.sum %>% filter(use.coarse == "used.no.purpose") %>%
  group_by(Class,use.coarse) %>% summarise(sp.count = sum(sp.count))

use.plt <- ggplot(use.status.sum, aes(use.coarse, sp.count, fill = status)) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "grey") +
  facet_wrap(~Class, scales = "free_y") +
  geom_col() +
  geom_col(data = use.ovr, 
           fill = NA, colour = "black", linewidth = .75) +
  geom_col(data = use.unknown, 
           fill = NA, colour = "red", linewidth = .75) +
  scale_x_discrete(limits = c("use", "pets.13", "food.hum.1", "sport.15",
                              "jewellery.12", "apparel.10", "med.3", "other_known", 
                              "used.no.purpose"),
                   labels = c("Total", "Pets", "Food", "Sport", "Ornamental \nproducts",
                              "Apparel","Medicinal", "Other \n(Known)", 
                              "Purpose \nunknown")) +
  scale_fill_manual(values = c("black", "#67001f", "#d6604d", "#fddbc7", "#92c5de", "#2166ac", "grey15")) +
  xlab("Use and purpose") +
  ylab("Species") +
  
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1, 
                                   face = c("bold", "plain", "plain", "plain",
                                            "plain", "plain", "plain", "plain")))

ggsave(path = paste0(data.path,"Outputs/Figures/Initial"),
       filename = "mam.aves.use.bar.v1.png",
       use.plt, bg = "white",
       device = "png", width = 20, height = 12, units = "cm")

## Simple mapping --------------------------------------------------------------

raster.birds <- data.frame(path = list.files("E:/Data/IUCN/Ranges/Birds/Rasters/QuarterDegree/Species/", full.names = T)) %>%
  mutate(IUCN.name = gsub("E:/Data/IUCN/Ranges/Birds/Rasters/QuarterDegree/Species/", "", path),
         IUCN.name = gsub(".range.rasters.0.25.degree.tif", "", IUCN.name))
raster.mam <- data.frame(path = list.files("E:/Data/IUCN/Ranges/Mammals/Rasters/QuarterDegree/Species/", full.names = T)) %>%
  mutate(IUCN.name = gsub("E:/Data/IUCN/Ranges/Mammals/Rasters/QuarterDegree/Species/", "", path),
         IUCN.name = gsub(".range.rasters.0.25.degree.tif", "", IUCN.name))

uses.ls <- c("use", "pets.13","apparel.10", "food.hum.1", "sport.15","jewellery.12", "med.3", 
          "used.no.purpose")

## make summed sr rasters ------------------------------------------------------
for (i in 1:length(uses.ls)) {
  cat(i, "\n")
  use.i <- uses.ls[[i]]
  sp.i <- use.all.birds %>% filter(!!sym(use.i) == 1) %>% select(IUCN.name)
  raster.paths.i <- raster.birds %>% filter(IUCN.name %in% sp.i$IUCN.name)
  raster.ls <- rast(raster.paths.i$path)
  app(raster.ls, fun = 'sum', na.rm = TRUE,overwrite=TRUE,
                filename = paste0(data.path,"Outputs/raster.summaries/birds/", 
                                  use.i, ".tif"))
}

for (i in 1:length(uses.ls)) {
  cat(i, "\n")
  use.i <- uses.ls[[i]]
  sp.i <- use.all.mam %>% filter(!!sym(use.i) == 1) %>% select(IUCN.name)
  raster.paths.i <- raster.mam %>% filter(IUCN.name %in% sp.i$IUCN.name)
  raster.ls <- rast(raster.paths.i$path)
  app(raster.ls, fun = 'sum', na.rm = TRUE,overwrite=TRUE,
      filename = paste0(data.path,"Outputs/raster.summaries/mammals/", 
                        use.i, ".tif"))
}

## Make a hotspots raster ------------------------------------------------------
i <- 1
hs.list <- list()
for (i in 1:length(uses.ls)) {
  use.i <- uses.ls[[i]]
  rast.birds.i <- rast(paste0(data.path,"Outputs/raster.summaries/birds/",use.i,".tif"))
  rast.mammals.i <- rast(paste0(data.path,"Outputs/raster.summaries/mammals/",use.i,".tif"))
  
  cutoff.b <- quantile(values(rast.birds.i), probs = 0.95, na.rm = TRUE)
  cutoff.m <- quantile(values(rast.mammals.i), probs = 0.95, na.rm = TRUE)
  
  mat.m <- c(0, cutoff.m[[1]], NA,
             cutoff.m[[1]], Inf, 1)
  pclmat.m <- matrix(mat.m, ncol=3, byrow=TRUE)
  
  mat.b <- c(0, cutoff.b[[1]], NA,
             cutoff.b[[1]], Inf, 10)
  pclmat.b <- matrix(mat.b, ncol=3, byrow=TRUE)
  
  top10.m <- classify(rast.mammals.i, pclmat.m, include.lowest = TRUE)
  top10.b <- classify(rast.birds.i, pclmat.b, include.lowest = TRUE)
  
  combi.hs <- app(c(top10.m,top10.b), fun = 'sum', na.rm = TRUE,
                  filename = paste0(data.path,"Outputs/raster.summaries/combined.hotspots/", 
                                    use.i, ".tif"), overwrite=TRUE)
  hs.list[[paste0(use.i, "combi")]] <- combi.hs 
}

## plot summed rasters ---------------------------------------------------------
world <- ne_countries(scale = 50, returnclass = "sf")
map.ls <- list()
for (i in 1:length(uses.ls)) {
  use.i <- uses.ls[[i]]
  rast.bird.i <- rast(paste0(data.path,"Outputs/raster.summaries/birds/",use.i,".tif"))
  rast.mam.i <- rast(paste0(data.path,"Outputs/raster.summaries/mammals/",use.i,".tif"))
  
  rast.bird.i <- mask(rast.bird.i, world)
  rast.mam.i <- mask(rast.mam.i, world)
  
  range.bird.i <- data.frame(minmax(rast.bird.i))
  range.mam.i <- data.frame(minmax(rast.mam.i))
  
  map.bird.i <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = rast.bird.i, aes(fill = sum)) +
    scale_fill_gradient2(na.value = NA, high = "#a50026", mid = "#ffffbf", low = "#313695",
                         midpoint = sum(range.bird.i$sum)/2, "Species") +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank())
  
  map.mam.i <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = rast.mam.i, aes(fill = sum)) +
    scale_fill_gradient2(na.value = NA, high = "#a50026", mid = "#ffffbf", low = "#313695",
                         midpoint = sum(range.mam.i$sum)/2, "Species") +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank())
  
  map.ls[[paste0(use.i, ".bird.plt")]] <- map.bird.i 
  map.ls[[paste0(use.i, ".mammal.plt")]] <- map.mam.i 
}


## plot hotspot rasters --------------------------------------------------------

hs.plt.ls <- list()
for (i in 1:length(uses.ls)) {
  use.i <- uses.ls[[i]]
  hs.i <- rast(paste0(data.path,"Outputs/raster.summaries/combined.hotspots/",use.i,".tif"))

  levels(hs.i) <- data.frame(sum = c(1, 10, 11), hs = c("Mammal", "Bird", "Joint"))

  hs.plt <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = hs.i) +
    scale_fill_manual(values = c("yellow", "purple", "black"), 
                      # mam, bird, joint
                      na.value = NA, na.translate =FALSE ) +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.title = element_blank(), legend.position = "none")
  
  hs.plt.ls[[paste0(use.i)]] <- hs.plt 
  
}

use.arr <-  ggarrange(map.ls$pets.13.bird.plt, map.ls$pets.13.mammal.plt, hs.plt.ls$pets.13 + theme(legend.position = "none"),
                     map.ls$food.hum.1.bird.plt,map.ls$food.hum.1.mammal.plt, hs.plt.ls$food.hum.1,
                     map.ls$sport.15.bird.plt,map.ls$sport.15.mammal.plt, hs.plt.ls$sport.15,
                     map.ls$jewellery.12.bird.plt,map.ls$jewellery.12.mammal.plt, hs.plt.ls$jewellery.12,
                     map.ls$apparel.10.bird.plt,map.ls$apparel.10.mammal.plt, hs.plt.ls$apparel.10,
                     map.ls$med.3.bird.plt,map.ls$med.3.mammal.plt, hs.plt.ls$med.3,
                     ncol = 3, nrow = 6,
                     labels = c("a - Pets", "", "",
                                "b - Food", "", "",
                                "c - Sport", "", "",
                                "d - Ornamental", "", "",
                                "e - Apparel", "", "",
                                "f - Medicine", "", ""), hjust = 0)
  hs.plt.ls$pets.13 + theme(legend.position = "none"),
  hs.plt.ls$pets.13 + theme(legend.position = "none"),
  hs.plt.ls$pets.13 + theme(legend.position = "none"),
  hs.plt.ls$pets.13 + theme(legend.position = "none"),
  hs.plt.ls$pets.13 + theme(legend.position = "none"),
  hs.plt.ls$pets.13 + theme(legend.position = "none"))

ggsave(path = paste0(data.path,"Outputs/Figures/Initial"),
       filename = "mam.aves.use.map.v2.png",
       use.arr, bg = "white",
       device = "png", width = 25, height = 25, units = "cm")



