
library(rredlist)
library(tidyverse)

API.key <- "RbNsYhKCPg1awiucYB2tCnLdY8zJ6NFyHkV8"
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## Use summary across taxa -----------------------------------------------------
use.key <- rl_use_and_trade(key = API.key)$use_and_trade
use.store <- data.frame()

for (i in 1:18) {
  use.i <- rl_use_and_trade(paste0(i), key = API.key)$assessments
  use.store <- rbind(use.store, use.i)
}
#write_rds(use.store, "Data/IUCN/raw.use.data.all.rds")


## Get use, threat and distribution data for all BIRDS -------------------------
aves.assessments <- rl_class(class = "Aves", latest = TRUE, key = API.key)

## 11195 global assessments
aves.assess <- aves.assessments$assessments %>% as.data.frame() %>%
  mutate(scopes = as.character(scopes)) %>%
  filter(scopes == "list(description = list(en = \"Global\"), code = \"1\")" )

write.csv(aves.assess, paste0(data.path, "Data/IUCN/aves.assess.metadata.csv"))

order.txt <- data.frame()
order.taxo <- data.frame()
order.threat <- data.frame()
order.use <- data.frame()
order.location <- data.frame()
order.habs <- data.frame()
order.realms <- data.frame()


for (k in 1:nrow(aves.assess)){
  
  cat(k, "\n")
  Sys.sleep(0.5)
  sp.k <- aves.assess[k,]
  
  assmt.k <- rl_assessment(sp.k$assessment_id, key = API.key)
  
  taxo.k <- data.frame(orderName = assmt.k$taxon$order_name, 
                       familyName = assmt.k$taxon$family_name,
                       IUCN.name = assmt.k$taxon$scientific_name,
                       common.name = filter(assmt.k$taxon$common_names, main == TRUE)$name,
                       SIS = assmt.k$sis_taxon_id, status = assmt.k$red_list_category$code,
                       migratory = ifelse(is.null(assmt.k$supplementary_info$movement_patterns),
                                          "Not given",assmt.k$supplementary_info$movement_patterns),
                       EOO = ifelse(is.null(assmt.k$supplementary_info$estimated_extent_of_occurence),
                                    "Not given",assmt.k$supplementary_info$estimated_extent_of_occurence))
  
  if (is_empty(assmt.k$threats)){
    assmt.k$threats <- data.frame(scope = NA, timing = NA, 
                                  internationalTrade = NA, score = NA,
                                  severity = NA, ancestry = NA, virus = NA,
                                  ias = NA, text = NA, description = NA, code = NA)
  }
  if (is_empty(assmt.k$use_and_trade)){
    assmt.k$use_and_trade <- data.frame(international = NA, national = NA, 
                                        other = NA, subsistence = NA,
                                        description = NA, code = NA)
  }
  if (is_empty(assmt.k$locations)){
    assmt.k$locations <- data.frame(is_endemic = NA, formerlyBred = NA, 
                                    origin = NA, presence = NA, seasonality = NA,
                                    description = NA, code = NA)
  }
  if (is_empty(assmt.k$habitats)){
    assmt.k$habitats <- data.frame(majorImportance = NA, season = NA, 
                                   suitability = NA, en = NA, code = NA)
  }
  
  if (is_empty(assmt.k$biogeographical_realms)){
    assmt.k$biogeographical_realms <- data.frame(en = NA, code = NA)
  }
  
  if (is_empty(assmt.k$documentation$threats)){
    assmt.k$documentation$threats <- NA
  }
  
  if (is_empty(assmt.k$documentation$use_trade)){
    assmt.k$documentation$use_trade <- NA
  }
  
  if (is_empty(assmt.k$documentation$rationale)){
    assmt.k$documentation$rationale <- NA
  }
  
  if (is_empty(assmt.k$documentation$trend_justification)){
    assmt.k$documentation$trend_justification <- NA
  }
  
  verbose.k <- data.frame(IUCN.name = assmt.k$taxon$scientific_name,
                          assmnt.txt = assmt.k$documentation$rationale,
                          trend.txt = assmt.k$documentation$trend_justification,
                          threat.txt = assmt.k$documentation$threats,
                          use.trade.txt = assmt.k$documentation$use_trade)
                          
  
  assmt.k$threats$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$use_and_trade$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$locations$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$habitats$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$biogeographical_realms$IUCN.name <-assmt.k$taxon$scientific_name
  
  assmt.k$locations$seasonality <- as.character(assmt.k$locations$seasonality)
  
  order.taxo <- rbind(order.taxo, taxo.k)
  order.threat <- rbind(order.threat, as.data.frame(as.matrix(assmt.k$threats)))
  order.use <- rbind(order.use, as.data.frame(as.matrix(assmt.k$use_and_trade)))
  order.location <- rbind(order.location, as.data.frame(as.matrix(assmt.k$locations)))
  order.habs <- rbind(order.habs, as.data.frame(as.matrix(assmt.k$habitats)))
  order.realms <- rbind(order.realms, as.data.frame(as.matrix(assmt.k$biogeographical_realms)))
  order.txt <- rbind(order.txt, verbose.k)
  
}

write.csv(order.taxo, paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Jun25.csv"))
write.csv(order.threat, paste0(data.path, "Data/IUCN/raw.iucn.threat.Jun25.csv"))
write.csv(order.use, paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv"))
write.csv(order.location, paste0(data.path, "Data/IUCN/raw.iucn.location.Jun25.csv"))
write.csv(order.habs, paste0(data.path, "Data/IUCN/raw.iucn.habitats.Jun25.csv"))
write.csv(order.realms, paste0(data.path, "Data/IUCN/raw.iucn.realms.Jun25.csv"))
write.csv(order.txt, paste0(data.path, "Data/IUCN/raw.iucn.verbose.txt.Jun25.csv"))

## Get use, threat and distribution data for all MAMMALS -----------------------
mam.assessments <- rl_class(class = "Mammalia", latest = TRUE, key = API.key)

## 5897 global assessments
mam.assess <- mam.assessments$assessments %>% as.data.frame() %>%
  mutate(scopes = as.character(scopes)) %>%
  filter(scopes == "list(description = list(en = \"Global\"), code = \"1\")" )

write.csv(mam.assess, paste0(data.path, "Data/IUCN/mam.assess.metadata.csv"))

order.txt <- data.frame()
order.taxo <- data.frame()
order.threat <- data.frame()
order.use <- data.frame()
order.location <- data.frame()
order.habs <- data.frame()
order.realms <- data.frame()

k <- 4
for (k in 1:nrow(mam.assess)){
  
  cat(k, "\n")
  Sys.sleep(0.5)
  sp.k <- mam.assess[k,]
  
  assmt.k <- rl_assessment(sp.k$assessment_id, key = API.key)
  
  taxo.k <- data.frame(orderName = assmt.k$taxon$order_name, 
                       familyName = assmt.k$taxon$family_name,
                       IUCN.name = assmt.k$taxon$scientific_name,
                       common.name = ifelse(is_empty(assmt.k$taxon$common_names), "Not given",
                                            ifelse(nrow(filter(assmt.k$taxon$common_names, main == TRUE)) == 0,
                                            "Not given",
                                            filter(assmt.k$taxon$common_names, main == TRUE)$name)),
                       
                       SIS = assmt.k$sis_taxon_id, status = assmt.k$red_list_category$code,
                       migratory = ifelse(is.null(assmt.k$supplementary_info$movement_patterns),
                                          "Not given",assmt.k$supplementary_info$movement_patterns),
                       EOO = ifelse(is.null(assmt.k$supplementary_info$estimated_extent_of_occurence),
                                    "Not given",assmt.k$supplementary_info$estimated_extent_of_occurence))
  
  if (is_empty(assmt.k$threats)){
    assmt.k$threats <- data.frame(scope = NA, timing = NA, 
                                  internationalTrade = NA, score = NA,
                                  severity = NA, ancestry = NA, virus = NA,
                                  ias = NA, text = NA, description = NA, code = NA)
  }
  if (is_empty(assmt.k$use_and_trade)){
    assmt.k$use_and_trade <- data.frame(international = NA, national = NA, 
                                        other = NA, subsistence = NA,
                                        description = NA, code = NA)
  }
  if (is_empty(assmt.k$locations)){
    assmt.k$locations <- data.frame(is_endemic = NA, formerlyBred = NA, 
                                    origin = NA, presence = NA, seasonality = NA,
                                    description = NA, code = NA)
  }
  if (is_empty(assmt.k$habitats)){
    assmt.k$habitats <- data.frame(majorImportance = NA, season = NA, 
                                   suitability = NA, description = NA, code = NA)
  }
  
  if (is_empty(assmt.k$biogeographical_realms)){
    assmt.k$biogeographical_realms <- data.frame(description = NA, code = NA)
  }
  
  if (is_empty(assmt.k$documentation$threats)){
    assmt.k$documentation$threats <- NA
  }
  
  if (is_empty(assmt.k$documentation$use_trade)){
    assmt.k$documentation$use_trade <- NA
  }
  
  if (is_empty(assmt.k$documentation$rationale)){
    assmt.k$documentation$rationale <- NA
  }
  
  if (is_empty(assmt.k$documentation$trend_justification)){
    assmt.k$documentation$trend_justification <- NA
  }
  
  verbose.k <- data.frame(IUCN.name = assmt.k$taxon$scientific_name,
                          assmnt.txt = assmt.k$documentation$rationale,
                          trend.txt = assmt.k$documentation$trend_justification,
                          threat.txt = assmt.k$documentation$threats,
                          use.trade.txt = assmt.k$documentation$use_trade)
  
  
  assmt.k$threats$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$use_and_trade$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$locations$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$habitats$IUCN.name <-assmt.k$taxon$scientific_name
  assmt.k$biogeographical_realms$IUCN.name <-assmt.k$taxon$scientific_name
  
  assmt.k$locations$seasonality <- as.character(assmt.k$locations$seasonality)
  
  order.taxo <- rbind(order.taxo, taxo.k)
  order.threat <- rbind(order.threat, as.data.frame(as.matrix(assmt.k$threats)))
  order.use <- rbind(order.use, as.data.frame(as.matrix(assmt.k$use_and_trade)))
  order.location <- rbind(order.location, as.data.frame(as.matrix(assmt.k$locations)))
  order.habs <- rbind(order.habs, as.data.frame(as.matrix(assmt.k$habitats)))
  order.realms <- rbind(order.realms, as.data.frame(as.matrix(assmt.k$biogeographical_realms)))
  order.txt <- rbind(order.txt, verbose.k)
  
}

write.csv(order.taxo, paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.MAMMALIA.Jul25.csv"))
write.csv(order.threat, paste0(data.path, "Data/IUCN/raw.iucn.threat.MAMMALIA.Jul25.csv"))
write.csv(order.use, paste0(data.path, "Data/IUCN/raw.iucn.use.MAMMALIA.Jul25.csv"))
write.csv(order.location, paste0(data.path, "Data/IUCN/raw.iucn.location.MAMMALIA.Jul25.csv"))
write.csv(order.habs, paste0(data.path, "Data/IUCN/raw.iucn.habitats.MAMMALIA.Jul25.csv"))
write.csv(order.realms, paste0(data.path, "Data/IUCN/raw.iucn.realms.MAMMALIA.Jul25.csv"))
write.csv(order.txt, paste0(data.path, "Data/IUCN/raw.iucn.verbose.txt.MAMMALIA.Jul25.csv"))
