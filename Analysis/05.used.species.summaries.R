library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(tidyterra)
library(ggpubr)
library(png)
library(grid)
library(cowplot)
library(clootl)
library(ape)
library(phytools)
library(ggtree)

## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

use.all <- read.csv(paste0(data.path, "Outputs/use.dataset/aves.mam.full.uses.tidy.Dec25.csv"))
use.all.birds <- use.all %>% filter(Class == "Aves") # 11021
use.all.mam <- use.all %>% filter(Class == "Mammalia") # 5946

## Simple summaries ------------------------------------------------------------

use.long <- use.all %>% 
  select(-X) %>%
  pivot_longer(!c("IUCN.name", "common.name", "Class", "familyName", "orderName",
                  "status"), names_to = "use.group", values_to = "pres") %>%
  mutate(use.coarse = ifelse(use.group %in% c("ex.situ.16", "food.an.2",
                                              "poison.4", "man.chem.5",
                                              "other.chem.6", "fuels.7", 
                                              "fibre.8", "construct.9",
                                              "other.household.11", "research.14",
                                              "other.17"),
                             "other_known", use.group),
         status = factor(status, levels = c("EW", "CR", "EN", "VU", "NT", "LC", "DD")))

use.status.sum <- use.long %>%
  group_by(Class,use.coarse, status)  %>% 
  summarise(sp.count = sum(pres)) %>% 
  filter(use.coarse != "no.purpose")

# % for text
use.long %>%
  group_by(Class,use.coarse)  %>% 
  summarise(sp.count = sum(pres)) %>% 
  filter(use.coarse != "no.purpose") %>%
  mutate(totals = ifelse(Class == "Aves", 11021, 5946),
         perc = round((sp.count/totals)*100,1))

use.long %>%
  group_by(use.coarse)  %>% 
  summarise(sp.count = sum(pres)) %>% 
  filter(use.coarse != "no.purpose")

length(unique(use.long$IUCN.name))
use.long %>% filter(use.coarse == "use" & pres == 1) #8797
use.long %>% filter(use.coarse == "used.no.purpose" & pres == 1) #2012
use.long %>% filter(!use.coarse %in% c("no.purpose", "used.no.purpose", "use")) %>% filter(pres == 1) %>%
  summarise(length(unique(IUCN.name))) # 6785


use.ovr <- use.status.sum %>% filter(use.coarse == "use") %>%
  group_by(Class,use.coarse) %>% summarise(sp.count = sum(sp.count))
use.unknown <- use.status.sum %>% filter(use.coarse == "used.no.purpose") %>%
  group_by(Class,use.coarse) %>% summarise(sp.count = sum(sp.count))

use.aves <- ggplot(filter(use.status.sum, Class == "Aves" ), aes(use.coarse, sp.count, fill = status)) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "grey") +
  #facet_wrap(~Class, scales = "free_y") +
  geom_col() +
  geom_col(data = filter(use.ovr, Class == "Aves" ), 
           fill = NA, colour = "black", linewidth = .75) +
  geom_col(data = filter(use.unknown, Class == "Aves" ), 
           fill = NA, colour = "red", linewidth = .75) +
  scale_x_discrete(limits = c("use", "pets.13", "food.hum.1", "sport.15",
                              "jewellery.12", "apparel.10", "med.3", "other_known", 
                              "used.no.purpose"),
                   labels = c("Total", "Pets", "Food", "Sport", "Ornamental \nproducts",
                              "Apparel","Medicinal", "Other \n(Known)", 
                              "Purpose \nunknown")) +
  annotate("text", x = 2, y = 4500, label = "1") +
  annotate("text", x = 3, y = 2000, label = "2") +
  annotate("text", x = 4, y = 1600, label = "3") +
  #scale_fill_manual(values = c("black", "#67001f", "#d6604d", "#fddbc7", "#92c5de", "#2166ac", "grey15")) +
  scale_fill_manual(values = c("black", "#3f007d", "#6a51a3", "#807dba", "#bcbddc", "#efedf5", "grey50")) +
  xlab("Use and purpose") +
  ylab("Species") +
  theme_minimal() +
  theme(legend.position = "none", strip.text.x.top = element_blank(),
        axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1, 
                                   face = c("bold", "plain", "plain", "plain",
                                            "plain", "plain", "plain", "plain")))

use.mam <- ggplot(filter(use.status.sum, Class == "Mammalia" ), aes(use.coarse, sp.count, fill = status)) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "grey") +
  #facet_wrap(~Class, scales = "free_y") +
  geom_col() +
  geom_col(data = filter(use.ovr, Class == "Mammalia" ), 
           fill = NA, colour = "black", linewidth = .75) +
  geom_col(data = filter(use.unknown, Class == "Mammalia" ), 
           fill = NA, colour = "red", linewidth = .75) +
  scale_x_discrete(limits = c("use", "pets.13", "food.hum.1", "sport.15",
                              "jewellery.12", "apparel.10", "med.3", "other_known", 
                              "used.no.purpose"),
                   labels = c("Total", "Pets", "Food", "Sport", "Ornamental \nproducts",
                              "Apparel","Medicinal", "Other \n(Known)", 
                              "Purpose \nunknown")) +
  #scale_fill_manual(values = c("black", "#67001f", "#d6604d", "#fddbc7", "#92c5de", "#2166ac", "grey15")) +
  scale_fill_manual(values = c("black", "#662506", "#993404", "#fec44f", "#fee391", "#fff7bc", "grey50")) +
  annotate("text", x = 2, y = 500, label = "1") +
  annotate("text", x = 3, y = 1600, label = "2") +
  annotate("text", x = 4, y = 700, label = "3") +
  annotate("text", x = 6, y = 700, label = "4") +
  xlab("Use and purpose") +
  ylab("Species") +
  theme_minimal() +
  theme(legend.position = "none", strip.text.x.top = element_blank(),
        axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1, 
                                   face = c("bold", "plain", "plain", "plain",
                                            "plain", "plain", "plain", "plain")))

use.plt <- ggarrange(use.aves, use.mam, ncol = 2, labels = c("a", "b"))

use.plt2 <- ggdraw(use.plt) +
  draw_plot(use.plt) +
  draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
             x = 0.4, y = 0.9, width = 0.1, height = 0.1) +
  draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
             x = 0.9, y = 0.9, width = 0.1, height = 0.1)

ggsave(path = paste0(data.path,"Outputs/Figures/Initial"),
       filename = "mam.aves.use.bar.v3.unicol.png",
       use.plt2, bg = "white",
       device = "png", width = 20, height = 12, units = "cm")

## Taxon mapping ---------------------------------------------------------------

aves.sr <- use.long %>% filter(pres == 1, !use.group %in% c("no.purpose", "use", "used.no.purpose"), Class == "Aves") %>% 
  group_by(orderName) %>%
  summarise(order.sr = n_distinct(IUCN.name)) %>% arrange(-order.sr)

aves.order <- use.long %>% filter(!use.group %in% c("no.purpose", "use", "used.no.purpose"), Class == "Aves") %>% 
  group_by(orderName, use.coarse) %>%
  summarise(order.sr = n_distinct(IUCN.name), used.sp = sum(pres)) %>%
  filter(use.coarse %in% c("food.hum.1", "sport.15", "pets.13")) %>%
  mutate(perc = (used.sp/order.sr)*100) %>%
  filter(orderName %in% c(aves.sr$orderName[1:10]))

ggplot(aves.order, aes(use.coarse, perc)) +
  geom_col() + facet_wrap(~orderName)+
  theme_minimal()
i <- "ACCIPITRIFORMES"

order.plt.ls <- list()
for (i in aves.sr$orderName[1:10]) {
  
  order.i <- aves.order %>% filter(orderName == i)
  
  order.i.plt <- ggplot(order.i, aes(use.coarse, perc)) +
    geom_col(fill = "#9e9ac8") +
    scale_x_discrete(labels = c("1", "2", "3"), breaks = c("pets.13", "food.hum.1", "sport.15"),
                     limits = c("pets.13", "food.hum.1", "sport.15")) +
    #ylab("%") +
    xlab(paste0(str_to_title(i), "\n(n = ", unique(order.i$order.sr),")")) +
    coord_cartesian(ylim = c(0, 100), expand = F) +
    scale_y_continuous(labels = c("0", "50", "100"), breaks = c(0, 50, 100))+
    theme_classic(base_size = 10) +
    theme(axis.title.y = element_blank())
  
  order.plt.ls[[paste0(i)]] <- order.i.plt
}


## plot trees
tax <- taxonomyGet(taxonomy_year = 2024)
representatives <- tax %>%
  group_by(ORDER) %>%                  
  slice(1) %>%
  ungroup() %>% filter(ORDER %in% str_to_title(aves.sr$orderName[1:10]))
head(tax)
tip_keep <- representatives$SCI_NAME
order_tree <- extractTree(species = tip_keep)
matched_orders <- representatives$ORDER[match(order_tree$tip.label, gsub(" ", "_", representatives$SCI_NAME))]

# Now safely rename tips
order_tree$tip.label <- matched_orders

order_tree_ultra <- chronos(order_tree, lambda = 1)  # lambda = 1 = moderate smoothing

ggtree(order_tree_ultra, layout = "rectangular") +
  coord_flip() +              
  scale_x_reverse()+                
  geom_tiplab(size = 3, hjust = 0, fontface = "italic") 

clean.tree <- ggtree(order_tree_ultra, layout = "rectangular") +
  coord_flip(xlim = c(0.35, 0)) +              
  scale_x_reverse() +   
  theme_void()

library(ggpubr)

empty <- ggplot() + theme_void()
aves.tree.arrange <-ggarrange(clean.tree,
                              empty,
                              ggarrange(order.plt.ls$ANSERIFORMES, order.plt.ls$GALLIFORMES, order.plt.ls$COLUMBIFORMES,
                                        order.plt.ls$CHARADRIIFORMES, order.plt.ls$CAPRIMULGIFORMES,
                                        order.plt.ls$PASSERIFORMES, order.plt.ls$PSITTACIFORMES,
                                        order.plt.ls$PICIFORMES, order.plt.ls$ACCIPITRIFORMES,
                                        order.plt.ls$STRIGIFORMES,
                                        ncol = 10), nrow = 3, heights = c(.9, .25, 1))

## mams
mam.sr <- use.long %>% filter(pres == 1, !use.group %in% c("no.purpose", "use", "used.no.purpose"), Class == "Mammalia") %>% 
  group_by(orderName) %>%
  summarise(order.sr = n_distinct(IUCN.name)) %>% arrange(-order.sr)

mam.order <- use.long %>% filter(!use.group %in% c("no.purpose", "use", "used.no.purpose"), Class == "Mammalia") %>% 
  group_by(orderName, use.coarse) %>%
  summarise(order.sr = n_distinct(IUCN.name), used.sp = sum(pres)) %>%
  filter(use.coarse %in% c("food.hum.1", "sport.15", "pets.13", "apparel.10")) %>%
  mutate(perc = (used.sp/order.sr)*100) %>%
  filter(orderName %in% c(mam.sr$orderName[1:10]))

ggplot(mam.order, aes(use.coarse, perc)) +
  geom_col() + facet_wrap(~orderName)+
  theme_minimal()
i <- "ACCIPITRIFORMES"

mam.order.plt.ls <- list()
for (i in mam.sr$orderName[1:10]) {
  
  order.i <- mam.order %>% filter(orderName == i)
  
  order.i.plt <- ggplot(order.i, aes(use.coarse, perc)) +
    geom_col(fill = "#d8b365") +
    scale_x_discrete(labels = c("1", "2", "3", "4"), 
                     breaks = c("pets.13", "food.hum.1", "sport.15", "apparel.10"),
                     limits = c("pets.13", "food.hum.1", "sport.15", "apparel.10")) +
    #ylab("%") +
    xlab(paste0(str_to_title(i), "\n(n = ", unique(order.i$order.sr),")")) +
    coord_cartesian(ylim = c(0, 100), expand = F) +
    scale_y_continuous(labels = c("0", "50", "100"), breaks = c(0, 50, 100))+
    theme_classic(base_size = 10) +
    theme(axis.title.y = element_blank())
  
  mam.order.plt.ls[[paste0(i)]] <- order.i.plt
}

# Suppose the tree file is named “Mammalia_10000_completed_trees.tre”
treefile <- paste0(data.path, "Data/Upham.Mam/tree.samp/MamPhy_BDvr_Completed_5911sp_topoCons_FBDasZhouEtAl_v2_tree0000.tre")

# Read one tree (if file is a multi‐tree file)
mam.tree <- read.tree(treefile)

mam.tax <- read.csv(paste0(data.path, "Data/Upham.Mam/taxonomy_mamPhy_5911species.csv"))

# CETARTIODACTYLA in Upham (ARTIODACTYLA and Cetacea split in our data)
mam.order.names <- mam.tax %>% group_by(ord) %>% slice(1) %>% filter(ord %in% c(mam.sr$orderName[1:10],"CETARTIODACTYLA"))

mam.order.tree <- drop.tip(mam.tree, setdiff(mam.tree$tip.label, mam.order.names$Species_Name))
matched_orders <- mam.order.names$ord[match(mam.order.tree$tip.label, gsub(" ", "_", mam.order.names$Species_Name))]
mam.order.tree$tip.label <- matched_orders

ggtree(mam.order.tree, layout = "rectangular") +
  coord_flip() +              
  scale_x_reverse() +   
  theme_void()+                
  geom_tiplab(size = 3, hjust = 0, fontface = "italic") 

mam.order.tree.plt <- ggtree(mam.order.tree, layout = "rectangular") +
  coord_flip(xlim = c(130, 0)) +              
  scale_x_reverse() +
  theme_void()

empty <- ggplot() + theme_void()
mam.tree.arrange <- ggarrange(mam.order.tree.plt,
                              empty,
                              ggarrange(mam.order.plt.ls$DIPROTODONTIA, mam.order.plt.ls$CINGULATA, 
                                        mam.order.plt.ls$PILOSA, mam.order.plt.ls$PRIMATES,
                                        mam.order.plt.ls$LAGOMORPHA, mam.order.plt.ls$RODENTIA,
                                        mam.order.plt.ls$EULIPOTYPHLA, mam.order.plt.ls$CHIROPTERA,
                                        mam.order.plt.ls$CARNIVORA, mam.order.plt.ls$ARTIODACTYLA,
                                        ncol = 10), nrow = 3, heights = c(.95, 0.25, 1))

## full arrange

full.fig1 <- ggarrange(ggarrange(use.aves, use.mam, ncol = 2, labels = c("a", "b")),
                       aves.tree.arrange, mam.tree.arrange,
                       nrow = 3, labels = c("", "c" , "d"))

full.fig1.2 <- ggdraw(full.fig1) +
  draw_plot(full.fig1) +
  draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
             x = 0.43, y = 0.94, width = 0.05, height = 0.05) +
  draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
             x = 0.93, y = 0.94, width = 0.05, height = 0.05) +
  ## birds
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Anas.crecca.anseriformes.png"),
             x = 0.045, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Lagopus.lagopus.galliformes.png"),
             x = 0.145, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Streptopelia.decaocto.columbiformes.png"),
             x = 0.24, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Uria.aalge.charadiformes.png"),
             x = 0.34, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Caprimulgus.caprimulgiformes.png"),
             x = 0.435, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Pycnonotus leucogenis.passeriformes.png"),
             x = 0.535, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Cyanopsitta spixii.psittaciformes.png"),
             x = 0.635, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Ramphastos.piciformes.png"),
             x = 0.735, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Milvus milvus.accipitriformes.png"),
             x = 0.835, y = .48, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Bubo scandiacus.strigiformes.png"),
             x = 0.935, y = .48, width = 0.035, height = 0.035) +
  # draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
  #            x = 0.93, y = 0.60, width = 0.05, height = 0.05) +
  ## mam
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Macropus fuliginosus.diprodontia.png"),
             x = 0.04, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Dasypus novemcinctus.cingulata.png"),
             x = 0.145, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Bradypus.pilosa.png"),
             x = 0.24, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Macaca mulatta.primate.png"),
             x = 0.34, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Sylvilagus bachmani.lagomorpha.png"),
             x = 0.435, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Nesomyidae.rodentia.png"),
             x = 0.535, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Erinaceus europaeus.eulipotphyla.png"),
             x = 0.635, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Myotis daubentonii.chiroptera.png"),
             x = 0.735, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Ursus arctos.carnivora.png"),
             x = 0.835, y = .15, width = 0.035, height = 0.035) +
  draw_image(paste0(data.path,"Data/Inset.pics/Fig1/Dama dama.artiodactyla.png"),
             x = 0.935, y = .15, width = 0.035, height = 0.035) 
# draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
#            x = 0.93, y = 0.27, width = 0.05, height = 0.05)

ggsave(path = paste0(data.path,"Outputs/Figures/Fig1"),
       filename = "mam.aves.use.phylobar.unicol.png",
       full.fig1.2, bg = "white",
       device = "png", width = 30, height = 30, units = "cm")

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
i <- 4
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

plot(top10.b)
## plot summed rasters ---------------------------------------------------------
uses.ls <- c("use", "pets.13","apparel.10", "food.hum.1", "sport.15","jewellery.12", "med.3", 
             "used.no.purpose")
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
    theme(legend.position = "right", legend.title = element_blank(),
          legend.key.width = unit(.25, 'cm'))
  
  map.mam.i <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = rast.mam.i, aes(fill = sum)) +
    scale_fill_gradient2(na.value = NA, high = "#a50026", mid = "#ffffbf", low = "#313695",
                         midpoint = sum(range.mam.i$sum)/2, "Species") +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.key.width = unit(.25, 'cm'))
  
  ## alt single colour scale
  uni.map.bird.i <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = rast.bird.i, aes(fill = sum)) +
    scale_fill_gradient(na.value = NA, high = "#3f007d", low = "#efedf5", 
                        "Species") +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.key.width = unit(.25, 'cm'))
  
  uni.map.mam.i <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = rast.mam.i, aes(fill = sum)) +
    scale_fill_gradient(na.value = NA, high = "#662506", low = "#fff7bc",
                         "Species") +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.position = "right", legend.title = element_blank(),
          legend.key.width = unit(.25, 'cm'))
  
  map.ls[[paste0(use.i, ".bird.plt")]] <- map.bird.i 
  map.ls[[paste0(use.i, ".mammal.plt")]] <- map.mam.i 
  map.ls[[paste0(use.i, ".bird.uni.plt")]] <- uni.map.bird.i 
  map.ls[[paste0(use.i, ".mammal.uni.plt")]] <- uni.map.mam.i 
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
  
  uni.hs.plt <- ggplot() +
    geom_spatvector(data = world, fill = "grey90", colour = NA) +
    geom_spatraster(data = hs.i, alpha = 1) +
    scale_fill_manual(values = c("#d8b365", "#9e9ac8", "black"), 
                      # mam, bird, joint
                      na.value = NA, na.translate =FALSE ) +
    coord_sf(ylim = c(-55, 75)) +
    theme_void() +
    theme(legend.title = element_blank(), legend.position = "none")
  
  hs.plt.ls[[paste0(use.i)]] <- hs.plt 
  hs.plt.ls[[paste0(use.i,".uni")]] <- uni.hs.plt 
  
}

## bivariate colours
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

use.arr2 <- ggdraw(use.arr) +
  draw_plot(use.arr) +
  draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
             x = 0.33-0.08, y = 0.96, width = 0.05, height = 0.05) +
  draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
             x = 0.66-0.08, y = 0.96, width = 0.05, height = 0.05)

ggsave(path = paste0(data.path,"Outputs/Figures/Initial"),
       filename = "mam.aves.use.map.oct25.v4.png",
       use.arr2, bg = "white",
       device = "png", width = 25, height = 25, units = "cm")



## uni colours
use.arr <-  ggarrange(map.ls$pets.13.bird.uni.plt, map.ls$pets.13.mammal.uni.plt, hs.plt.ls$pets.13.uni + theme(legend.position = "none"),
                      map.ls$food.hum.1.bird.uni.plt,map.ls$food.hum.1.mammal.uni.plt, hs.plt.ls$food.hum.1.uni,
                      map.ls$sport.15.bird.uni.plt,map.ls$sport.15.mammal.uni.plt, hs.plt.ls$sport.15.uni,
                      map.ls$jewellery.12.bird.uni.plt,map.ls$jewellery.12.mammal.uni.plt, hs.plt.ls$jewellery.12.uni,
                      map.ls$apparel.10.bird.uni.plt,map.ls$apparel.10.mammal.uni.plt, hs.plt.ls$apparel.10.uni,
                      map.ls$med.3.bird.uni.plt,map.ls$med.3.mammal.uni.plt, hs.plt.ls$med.3.uni,
                      ncol = 3, nrow = 6,
                      labels = c("a - Pets", "", "",
                                 "b - Food", "", "",
                                 "c - Sport", "", "",
                                 "d - Ornamental", "", "",
                                 "e - Apparel", "", "",
                                 "f - Medicine", "", ""), hjust = 0)

use.arr2 <- ggdraw(use.arr) +
  draw_plot(use.arr) +
  draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
             x = 0.33-0.08, y = 0.96, width = 0.05, height = 0.05) +
  draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
             x = 0.66-0.08, y = 0.96, width = 0.05, height = 0.05)

ggsave(path = paste0(data.path,"Outputs/Figures/Fig2.Mapping"),
       filename = "mam.aves.use.map.unicol.png",
       use.arr2, bg = "white",
       device = "png", width = 25, height = 25, units = "cm")

## all used species maps
all.use.plt <- ggarrange(map.ls$use.bird.uni.plt, map.ls$use.mammal.uni.plt, 
                         hs.plt.ls$use.uni + theme(legend.position = "none"),
                         ncol = 1, nrow = 3, labels = c("a", "b", "c"))

all.use.plt2 <- ggdraw(all.use.plt) +
  draw_plot(all.use.plt) +
  draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
             x = 0.95, y = 0.95, width = 0.05, height = 0.05) +
  draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
             x = 0.95, y = 0.61, width = 0.05, height = 0.05)

ggsave(path = paste0(data.path,"Outputs/Figures/SM"),
       filename = "any.use.map.png",
       all.use.plt2, bg = "white",
       device = "png", width = 15, height = 18, units = "cm")

## used no known purpose species maps
unknown.plt <- ggarrange(map.ls$used.no.purpose.bird.uni.plt, map.ls$used.no.purpose.mammal.uni.plt, 
                         hs.plt.ls$used.no.purpose.uni + theme(legend.position = "none"),
                         ncol = 1, nrow = 3, labels = c("a", "b", "c"))

unknown.plt2 <- ggdraw(unknown.plt) +
  draw_plot(unknown.plt) +
  draw_image(paste0(data.path,"Data/Inset.pics/Buceros.bicornis2.png"),
             x = 0.95, y = 0.95, width = 0.05, height = 0.05) +
  draw_image(paste0(data.path,"Data/Inset.pics/Smutsia.gigantea2.png"),
             x = 0.95, y = 0.61, width = 0.05, height = 0.05)

ggsave(path = paste0(data.path,"Outputs/Figures/SM"),
       filename = "unknown.use.map.png",
       unknown.plt2, bg = "white",
       device = "png", width = 15, height = 18, units = "cm")

## SM - Number of uses ---------------------------------------------------------

use.long <- use.all %>% 
  select(-X) %>%
  pivot_longer(!c("IUCN.name", "common.name", "Class", "familyName", "orderName",
                  "status"), names_to = "use.group", values_to = "pres") %>%
  mutate(use.coarse = ifelse(use.group %in% c("ex.situ.16", "food.an.2",
                                              "poison.4", "man.chem.5",
                                              "other.chem.6", "fuels.7", 
                                              "fibre.8", "construct.9",
                                              "other.household.11", "research.14",
                                              "other.17"),
                             "other_known", use.group),
         status = factor(status, levels = c("EW", "CR", "EN", "VU", "NT", "LC", "DD")))

use.tally <- use.long %>% filter(!use.coarse %in% c("used.no.purpose", "use", "no.purpose"), pres == 1) %>%
  group_by(IUCN.name, Class, status) %>%
  summarise(uses.n = length(unique(use.coarse))) %>%
  filter(uses.n>0) %>%
  mutate(uses.n2 = ifelse(uses.n >4, ">5", as.character(uses.n)))

mean(use.tally$uses.n)
use.tally %>% group_by(Class) %>% summarise(mean(uses.n))
use.tally %>% group_by(status) %>% summarise(mean(uses.n))
use.tally.sum <- use.tally %>% group_by(Class, uses.n2) %>% 
  summarise(sp.n = length(unique(IUCN.name))) %>%
  mutate(all.sp = 6754,
         prop = (sp.n/all.sp)*100)

use.tally.plt <- ggplot(use.tally.sum, aes(uses.n2, sp.n, fill = Class)) +
  geom_col() +
  facet_wrap(~Class) +
  scale_x_discrete(limits = c("1", "2", "3", "4", ">5")) +
  scale_fill_manual(values = c("#9e9ac8", "#d8b365")) +
  xlab("Number of uses") +
  ylab("Species richness") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"), legend.position = "none")

ggsave(path = paste0(data.path,"Outputs/Figures/SM"),
       filename = "use.tally.png",
       use.tally.plt, bg = "white",
       device = "png", width = 20, height = 12, units = "cm")


# # check that all species marked as used and with a purpose have a purpose
# # 8784
# use.long %>% filter(use.coarse == "use" & pres == 1)
# # 2030
# use.long %>% filter(use.coarse == "used.no.purpose" & pres == 1)
# #8784 -2030 = 6754
# used.no.purp.sp <- use.long  %>% group_by(IUCN.name) %>% 
#   mutate(use.combi = paste0(use.coarse, ".", pres)) %>%
#   filter(use.combi %in% c("use.1", "used.no.purpose.0")) %>%
#   filter(n()==2)
# length(unique(used.no.purp.sp$IUCN.name)) # 6754
# use.tally %>% filter(!IUCN.name %in% used.no.purp.sp$IUCN.name)


  



