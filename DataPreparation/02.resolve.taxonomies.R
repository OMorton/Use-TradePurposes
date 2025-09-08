
library(tidyverse)

## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## IUCN bird taxonomy Jun 2025, 11031 extant sp
iucn.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Jun25.csv"))
iucn.taxo.short <- iucn.taxo %>% filter(status != "EX") %>% 
  select(IUCN.name, common.name, status)

## IUCN mammal taxonomy Jun 2025, 5813 extant sp
iucn.mam.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.MAMMALIA.Jul25.csv"))
iucn.mam.taxo.short <- iucn.mam.taxo %>% filter(status != "EX") %>% 
  select(IUCN.name, common.name, status)

aves.mam.iucn <- rbind(iucn.taxo.short, iucn.mam.taxo.short)


## Wikipedia species binomials
## Collated using the wikidata query service (SPARQL)
wiki.species <- read.csv(paste0(data.path, "Data/Wikipedia/all.birds.wiki.csv"))
wiki.species <- wiki.species%>% 
  mutate(common.name = gsub("https://en.wikipedia.org/wiki/", "", wikipediaURL),
         common.name = gsub("_", " ", common.name),
         common.name = gsub("%27", "'", common.name),
         common.name = str_replace_all(common.name, 
                                       "(?<=\\s)([a-z])", 
                                       ~ toupper(.x)))

wiki.mammals <- read.csv(paste0(data.path, "Data/Wikipedia/all.mammals.wiki.csv"))
wiki.mammals <- wiki.mammals%>% 
  rename("wikipediaURL" = "article", "taxonName" = "name") %>%
  mutate(common.name = gsub("https://en.wikipedia.org/wiki/", "", wikipediaURL),
         common.name = gsub("_", " ", common.name),
         common.name = gsub("%27", "'", common.name),
         common.name = str_replace_all(common.name, 
                                       "(?<=\\s)([a-z])", 
                                       ~ toupper(.x)))

## Donald et al taxonomy
donald.trade <- read.csv(paste0(data.path, "Data/Donald.et.al.2024/Donald.TableS5.csv"))
# remove entries for three speciens (binomial names) where geographic ssp are included with
# distinct common names (effects Acridotheres melanopterus, Calendulauda africanoides,
# Mirafra somalica)
donald.trade <- donald.trade %>%
  filter(!Common.name %in% c("Grey-rumped Myna", "Grey-backed Myna",
                             "Foxy Lark", "Ash's Lark"))

## IUCN SpUD database
spud.birds <- read.csv(paste0(data.path, "Data/SpUD/iucn.spud.birds.jul25.csv"))
spud.mam <- read.csv(paste0(data.path, "Data/SpUD/iucn.spud.mam.jul25.csv"))

## Benítez-López et al., 2017
# minor pre-processing of the data adding genus names where letter(dot) format was
# used and swapping and for ", ".
BL.species <- read.csv(paste0(data.path,
                              "Data/Benitez-Lopez_2017/Benitez-Lopez_etal_2017_Hunting_Science.csv"))

## WILDMEAT database
WM.species <- read.csv(paste0(data.path,
                              "Data/WILDMEAT/WILDMEAT.species.list.Jul25.csv"))

## Morton et al., 2021
Morton.species <- read.csv(paste0(data.path,
                              "Data/Morton_et_al_2021/Morton2021.csv"))

## New LEMIS (Marshall 2025)
LEMIS.species <- read.csv(paste0(data.path,
                                  "Data/LEMIS/aves.mam.raw.sp.list.csv"))

## New CITES v2025.1
CITES.species <- read.csv(paste0(data.path,
                                 "Data/CITES/aves.mam.raw.sp.list.csv"))

## WiTIS data 
WiTIS.species <- read.csv(paste0(data.path,
                                 "Data/WiTIS/incident-summary-and-species-2995.csv"))

## AVONET trait data
avonet.df <- read.csv(paste0(data.path,
                                  "Data/Tobias.et.al.AVONET/AVONET.data.csv"))

## Bird et al trait data
bird.lh.df <- read.csv(paste0(data.path,
                             "Data/Bird.et.al.2020/GenLengths.Table.S4.csv"))

## Santageli et al Aesthetics data
santageli.df <- read.csv(paste0(data.path,
                              "Data/Santageli_et_al_Aesthetics/Santagelli_2023.csv"))

## Soria et al. Mammal trait database 
soria.df <- read.csv(paste0(data.path,
                             "Data/Soria.et.al.COMBINE/trait_data_imputed.csv"))

## Resolve taxonomy - Donald et al. 2024 ---------------------------------------


## 10999
donald.trade.short <- donald.trade %>% rename("trade.name" = "Scientific.name") %>%
  select(trade.name, Common.name, Trade.Prevalence.Score)

nrow(donald.trade.short) #10999
nrow(filter(donald.trade.short, Trade.Prevalence.Score>0)) #4913

# tr.sp <- donald.trade.short %>% filter(Trade.Prevalence.Score>0) # 4915
# order.use <- read.csv( paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv"))
# uses <- order.use %>% filter(!is.na(description))
# unique(uses$description) # 4839
# length(unique(uses$IUCN.name))


tax.match <- iucn.taxo.short %>%
  left_join(donald.trade.short, by = c("IUCN.name" = "trade.name"))

donald.sp <- tax.match %>% filter(!is.na(Trade.Prevalence.Score)) #10,808



tax.match %>% filter(is.na(Trade.Prevalence.Score))

iucn.taxo.short.upd <- iucn.taxo.short %>%
  mutate(donald.name.upd = case_when(IUCN.name == "Tyto prigoginei" ~ "Phodilus prigoginei",
                                     IUCN.name == "Gymnasio nudipes" ~ "Megascops nudipes",
                                     IUCN.name == "Limnoctites sulphuriferus" ~ "Cranioleuca sulphurifera",
                                     IUCN.name == "Oenanthe lugentoides" ~ "Oenanthe lugens-SPLIT-",
                                     IUCN.name == "Cincloramphus mariae" ~ "Megalurulus mariae",
                                     IUCN.name == "Alaudala cheleensis" ~ "Alaudala rufescens-SPLIT-",
                                     IUCN.name == "Padda fuscata" ~ "Lonchura fuscata",
                                     IUCN.name == "Hylexetastes uniformis" ~ "Hylexetastes perrotii-SPLIT-",
                                     IUCN.name == "Aptenorallus calayanensis" ~ "Gallirallus calayanensis",
                                     IUCN.name == "Saucerottia cupreicauda" ~ "Amazilia cupreicauda",
                                     IUCN.name == "Canachites canadensis" ~ "Falcipennis canadensis",
                                     IUCN.name == "Canachites franklinii" ~ "Falcipennis franklinii",
                                     IUCN.name == "Eumyias stresemanni" ~ "Cyornis stresemanni",
                                     IUCN.name == "Athene granti" ~ "Ninox granti",
                                     IUCN.name == "Pampusana rubescens" ~ "Alopecoenas rubescens",
                                     IUCN.name == "Copsychus albiventris" ~ "Kittacincla albiventris",
                                     IUCN.name == "Chionomesa lactea" ~ "Amazilia lactea",
                                     IUCN.name == "Tetrastes sewerzowi" ~ "Bonasa sewerzowi",
                                     IUCN.name == "Pampusana erythroptera" ~ "Alopecoenas erythropterus",
                                     IUCN.name == "Cincloramphus whitneyi" ~ "Megalurulus whitneyi",
                                     IUCN.name == "Strigops habroptilus" ~ "Strigops habroptila",
                                     IUCN.name == "Myrmothera fulviventris" ~ "Hylopezus fulviventris",
                                     IUCN.name == "Myrmothera berlepschi" ~ "Hylopezus berlepschi",
                                     IUCN.name == "Aethomyias nigrorufus" ~ "Crateroscelis nigrorufa",
                                     IUCN.name == "Aethomyias spilodera" ~ "Sericornis spilodera",
                                     IUCN.name == "Eupherusa ridgwayi" ~ "Thalurania ridgwayi",
                                     IUCN.name == "Pampusana sanctaecrucis" ~ "Alopecoenas sanctaecrucis",
                                     IUCN.name == "Ortygornis pondicerianus" ~ "Francolinus pondicerianus",
                                     IUCN.name == "Artomyias ussheri" ~ "Bradornis ussheri",
                                     IUCN.name == "Artomyias fuliginosa" ~ "Bradornis fuliginosus",
                                     IUCN.name == "Glaucestrilda coerulescens" ~ "Estrilda coerulescens",
                                     IUCN.name == "Empidornis semipartitus" ~ "Melaenornis semipartitus",
                                     IUCN.name == "Copsychus fulicatus" ~ "Saxicoloides fulicatus",
                                     IUCN.name == "Cincloramphus rubiginosus" ~ "Megalurulus rubiginosus",
                                     IUCN.name == "Nannopterum brasilianum" ~ "Nannopterum brasilianus",
                                     IUCN.name == "Nannopterum auritum" ~ "Nannopterum auritus",
                                     IUCN.name == "Luscinia svecica" ~ "Cyanecula svecica",
                                     IUCN.name == "Corthylio calendula" ~ "Regulus calendula",
                                     IUCN.name == "Myrmothera dives" ~ "Hylopezus dives",
                                     IUCN.name == "Pampa rufa" ~ "Campylopterus rufus",
                                     IUCN.name == "Selasphorus ellioti" ~ "Atthis ellioti",
                                     IUCN.name == "Chlorestes eliciae" ~ "Amazilia eliciae",
                                     IUCN.name == "Cincloramphus llaneae" ~ "Megalurulus llaneae",
                                     IUCN.name == "Cincloramphus grosvenori" ~ "Megalurulus grosvenori",
                                     IUCN.name == "Charadrius atrifrons" ~ "Charadrius mongolus-SPLIT-",
                                     IUCN.name == "Philodice mitchellii" ~ "Calliphlox mitchellii",
                                     IUCN.name == "Saucerottia castaneiventris" ~ "Amazilia castaneiventris",
                                     IUCN.name == "Athene roseoaxillaris" ~ "Ninox roseoaxillaris",
                                     IUCN.name == "Philodice bryantae" ~ "Calliphlox bryantae",
                                     IUCN.name == "Selasphorus heloisa" ~ "Atthis heloisa",
                                     IUCN.name == "Cynanthus auriceps" ~ "Chlorostilbon auriceps",
                                     IUCN.name == "Cynanthus forficatus" ~ "Chlorostilbon forficatus",
                                     IUCN.name == "Cynanthus canivetii" ~ "Chlorostilbon canivetii",
                                     IUCN.name == "Phaeoptila sordida" ~ "Cynanthus sordidus",
                                     IUCN.name == "Phaeochroa cuvierii" ~ "Campylopterus cuvierii",
                                     IUCN.name == "Pampa curvipennis" ~ "Campylopterus curvipennis",
                                     IUCN.name == "Microchera chionura" ~ "Elvira chionura",
                                     IUCN.name == "Microchera cupreiceps" ~ "Elvira cupreiceps",
                                     IUCN.name == "Chlorestes candida" ~ "Amazilia candida",
                                     IUCN.name == "Polyerata amabilis" ~ "Amazilia amabilis",
                                     IUCN.name == "Polyerata decora" ~ "Amazilia decora",
                                     IUCN.name == "Saucerottia cyanocephala" ~ "Amazilia cyanocephala",
                                     IUCN.name == "Saucerottia beryllina" ~ "Amazilia beryllina",
                                     IUCN.name == "Saucerottia cyanura" ~ "Amazilia cyanura",
                                     IUCN.name == "Saucerottia saucerottei" ~ "Amazilia saucerottei",
                                     IUCN.name == "Saucerottia edward" ~ "Amazilia edward",
                                     IUCN.name == "Leucolia viridifrons" ~ "Amazilia viridifrons",
                                     IUCN.name == "Leucolia wagneri" ~ "Amazilia wagneri",
                                     IUCN.name == "PChrysuronia coeruleogularis" ~ "Amazilia coeruleogularis",
                                     IUCN.name == "Chlorestes julie" ~ "Amazilia julie",
                                     IUCN.name == "Anthus chii" ~ "Anthus lutescens",
                                     IUCN.name == "Dendroma rufa" ~ "Philydor rufum",
                                     IUCN.name == "Leucolia violiceps" ~ "Amazilia violiceps",
                                     IUCN.name == "Pampusana kubaryi" ~ "Alopecoenas kubaryi",
                                     IUCN.name == "Goldmania bella" ~ "Goethalsia bella",
                                     IUCN.name == "Cincloramphus rufus" ~ "Megalurulus rufus",
                                     IUCN.name == "Melopyrrha nigra" ~ "Pyrrhulagra nigra",
                                     IUCN.name == "Melopyrrha taylori" ~ "Pyrrhulagra taylori",
                                     IUCN.name == "Pampusana hoedtii" ~ "Alopecoenas hoedtii",
                                     IUCN.name == "Lophorina latipennis" ~ "Lophorina superba-SPLIT-",
                                     IUCN.name == "Melopyrrha portoricensis" ~ "Pyrrhulagra portoricensis-SPLIT-",
                                     IUCN.name == "Padda oryzivora" ~ "Lonchura oryzivora",
                                     IUCN.name == "Arizelopsar femoralis" ~ "Poeoptera femoralis",
                                     IUCN.name == "Chrysuronia lilliae" ~ "Amazilia lilliae",
                                     IUCN.name == "Rheinardia nigrescens" ~ "Rheinardia ocellata-SPLIT-",
                                     IUCN.name == "Ducula nicobarica" ~ "Ducula aenea-SPLIT-",
                                     IUCN.name == "Oreotrochilus cyanolaemus" ~ "Oreotrochilus stolzmanni-SPLIT-",
                                     IUCN.name == "Campylopterus calcirupicola" ~ "Campylopterus largipennis-SPLIT-",
                                     IUCN.name == "Cacatua citrinocristata" ~ "Cacatua sulphurea-SPLIT-",
                                     IUCN.name == "Glaucidium sylvaticum" ~ "Glaucidium brodiei-SPLIT-",
                                     IUCN.name == "Cuculus optatus" ~ "Cuculus saturatus-SPLIT-",
                                     IUCN.name == "Cincloramphus turipavae" ~ "Megalurulus turipavae",
                                     IUCN.name == "Athene malaitae" ~ "Ninox malaitae",
                                     IUCN.name == "Eumyias sanfordi" ~ "Cyornis sanfordi",
                                     IUCN.name == "Pampusana canifrons" ~ "Alopecoenas canifrons",
                                     IUCN.name == "Copsychus pyrropygus" ~ "Trichixos pyrropygus",
                                     IUCN.name == "Locustella portenta" ~ "Locustella portenta-NEWLY DESCRIBED-",
                                     IUCN.name == "Phylloscopus nesophilus" ~ "Phylloscopus sarasinorum-SPLIT-",
                                     IUCN.name == "Zosterops flavissimus" ~ "Zosterops chloris-SPLIT-",
                                     IUCN.name == "Pitta vigorsii" ~ "Pitta elegans-SPLIT-",
                                     IUCN.name == "Cyornis kadayangensis" ~ "Cyornis kadayangensis-NEWLY DESCRIBED-",
                                     IUCN.name == "Heliothraupis oneilli" ~ "Heliothraupis oneilli-NEWLY DESCRIBED-",
                                     IUCN.name == "Zosterops meratusensis" ~ "Zosterops meratusensis-NEWLY DESCRIBED-",
                                     IUCN.name == "Cyornis montanus" ~ "Cyornis banyumas-SPLIT-",
                                     IUCN.name == "Oenanthe lugubris" ~ "Oenanthe lugens-SPLIT-",
                                     IUCN.name == "Cyornis whitei" ~ "Cyornis banyumas-SPLIT-",
                                     IUCN.name == "Melanocharis citreola" ~ "Melanocharis citreola-NEWLY DESCRIBED-",
                                     IUCN.name == "Bubo milesi" ~ "Bubo africanus-SPLIT-",
                                     IUCN.name == "Locustella musculus" ~ "Locustella castanea-SPLIT-",
                                     IUCN.name == "Locustella disturbans" ~ "Locustella castanea-SPLIT-",
                                     IUCN.name == "Myzomela prawiradilagae" ~ "Myzomela prawiradilagae-NEWLY DESCRIBED-",
                                     IUCN.name == "Chlorostilbon alice" ~ "Chlorostilbon poortmani-SPLIT-",
                                     IUCN.name == "Pitta concinna" ~ "Pitta elegans-SPLIT-",
                                     IUCN.name == "Dicaeum rhodopygiale" ~ "Dicaeum sanguinolentum-SPLIT-",
                                     IUCN.name == "Rhynchospiza dabbenei" ~ "Rhynchospiza strigiceps-SPLIT-",
                                     IUCN.name == "Eugenes spectabilis" ~ "Eugenes fulgens-SPLIT-",
                                     IUCN.name == "Gallinago magellanica" ~ "Gallinago paraguaiae-SPLIT-",
                                     IUCN.name == "Tunchiornis rubrifrons" ~ "Tunchiornis ochraceiceps-SPLIT-",
                                     IUCN.name == "Melopyrrha grandis" ~ "Pyrrhulagra portoricensis-SPLIT-",
                                     IUCN.name == "Cisticola bakerorum" ~ "Cisticola bakerorum-NEWLY DESCRIBED-",
                                     IUCN.name == "Cisticola anderseni" ~ "Cisticola anderseni-NEWLY DESCRIBED-",
                                     IUCN.name == "Pampusana xanthonura" ~ "Alopecoenas xanthonurus",
                                     IUCN.name == "Melopyrrha violacea" ~ "Pyrrhulagra violacea",
                                     IUCN.name == "Talaphorus chlorocercus" ~ "Leucippus chlorocercus",
                                     IUCN.name == "Thaumasius taczanowskii" ~ "Leucippus taczanowskii",
                                     IUCN.name == "Thaumasius baeri" ~ "Leucippus baeri",
                                     IUCN.name == "Hylocharis sapphirina" ~ "Amazilia sapphirina",
                                     IUCN.name == "Chrysuronia oenone" ~ "Amazilia oenone",
                                     IUCN.name == "Chrysuronia brevirostris" ~ "Amazilia brevirostris",
                                     IUCN.name == "Chrysuronia versicolor" ~ "Amazilia versicolor",
                                     IUCN.name == "Chrysuronia leucogaster" ~ "Amazilia leucogaster",
                                     IUCN.name == "Chrysuronia goudoti" ~ "Amazilia goudoti",
                                     IUCN.name == "Chrysuronia grayi" ~ "Amazilia grayi",
                                     IUCN.name == "Chrysuronia humboldtii" ~ "Amazilia humboldtii",
                                     IUCN.name == "Riccordia bicolor" ~ "Cyanophaia bicolor",
                                     IUCN.name == "Carpodacus lepidus" ~ "Carpodacus sibiricus-SPLIT-",
                                     IUCN.name == "Gracupica floweri" ~ "Gracupica contra-SPLIT-",
                                     IUCN.name == "Alaudala heinei" ~ "Alaudala rufescens-SPLIT-",
                                     IUCN.name == "Gygis candida" ~ "Gygis alba-SPLIT-",
                                     IUCN.name == "Copsychus cebuensis" ~ "Kittacincla cebuensis",
                                     IUCN.name == "Ceyx rufidorsa" ~ "Ceyx erithaca-SPLIT-",
                                     IUCN.name == "Fregetta lineata" ~ "Fregetta lineata-NEWLY DESCRIBED-",
                                     IUCN.name == "Cyornis pelingensis" ~ "Cyornis colonus-SPLIT-",
                                     IUCN.name == "Zosterops paruhbesar" ~ "Zosterops paruhbesar-NEWLY DESCRIBED-",
                                     IUCN.name == "Otus bikegila" ~ "Otus bikegila-NEWLY DESCRIBED-",
                                     IUCN.name == "Zosterops tetiparius" ~ "Zosterops kulambangrae-SPLIT-",
                                     IUCN.name == "Anthus brevirostris" ~ "Anthus furcatus-SPLIT-",
                                     IUCN.name == "Myrmothera subcanescens" ~ "Myrmothera campanisona-SPLIT-",
                                     IUCN.name == "Automolus exsertus" ~ "Automolus ochrolaemus-SPLIT-",
                                     IUCN.name == "Automolus cervinigularis" ~ "Automolus ochrolaemus-SPLIT-",
                                     IUCN.name == "Zosterops dehaani" ~ "Zosterops atriceps-SPLIT-",
                                     IUCN.name == "Basileuterus punctipectus" ~ "Basileuterus tristriatus-SPLIT-",
                                     IUCN.name == "Basileuterus melanotis" ~ "Basileuterus tristriatus-SPLIT-",
                                     IUCN.name == "Basileuterus tacarcunae" ~ "Basileuterus tristriatus-SPLIT-",
                                     IUCN.name == "Ninox fusca" ~ "Ninox boobook-SPLIT-",
                                     IUCN.name == "Ninox rotiensis" ~ "Ninox boobook-SPLIT-",
                                     IUCN.name == "Ninox plesseni" ~ "Ninox boobook-SPLIT-",
                                     IUCN.name == "Ploceus holoxanthus" ~ "Ploceus subaureus-SPLIT-",
                                     IUCN.name == "Oriolus consobrinus" ~ "Oriolus xanthonotus-SPLIT-",
                                     IUCN.name == "Gypsophila annamensis" ~ "Gypsophila crispifrons-SPLIT-",
                                     IUCN.name == "Macropygia cinnamomea" ~ "Macropygia emiliana-SPLIT-",
                                     IUCN.name == "Pomatorhinus bornensis" ~ "Pomatorhinus montanus-SPLIT-",
                                     IUCN.name == "Dicrurus palawanensis" ~ "Dicrurus hottentottus-SPLIT-",
                                     IUCN.name == "Riccordia maugaeus" ~ "Chlorostilbon maugaeus",
                                     IUCN.name == "Polyerata rosenbergi" ~ "Amazilia rosenbergi",
                                     IUCN.name == "Dicrurus sharpei" ~ "Dicrurus ludwigii-SPLIT-",
                                     IUCN.name == "Chionomesa bartletti" ~ "Amazilia bartletti",
                                     IUCN.name == "Ortygornis gularis" ~ "Francolinus gularis",
                                     IUCN.name == "Hylacola cauta" ~ "Calamanthus cautus",
                                     IUCN.name == "Hylacola pyrrhopygia" ~ "Calamanthus pyrrhopygius",
                                     IUCN.name == "Neosericornis citreogularis" ~ "Sericornis citreogularis",
                                     IUCN.name == "Pampusana stairi" ~ "Alopecoenas stairi",
                                     IUCN.name == "Cryptopezus nattereri" ~ "Hylopezus nattereri",
                                     IUCN.name == "Ortygornis sephaena" ~ "Dendroperdix sephaena",
                                     IUCN.name == "Campocolinus albogularis" ~ "Peliperdix albogularis",
                                     IUCN.name == "Campocolinus schlegelii" ~ "Peliperdix schlegelii",
                                     IUCN.name == "Campocolinus coqui" ~ "Peliperdix coqui",
                                     IUCN.name == "Amazilis amazilia" ~ "Amazilia amazilia",
                                     IUCN.name == "Chlorestes notata" ~ "Chlorostilbon notatus",
                                     IUCN.name == "Hylocharis chrysura" ~ "Amazilia chrysura",
                                     IUCN.name == "Helopsaltes fasciolatus" ~ "Helopsaltes fasciolata",
                                     IUCN.name == "Spermestes griseicapilla" ~ "Odontospiza griseicapilla",
                                     IUCN.name == "Dendroma erythroptera" ~ "Philydor erythropterum",
                                     IUCN.name == "Glaucestrilda perreini" ~ "Estrilda perreini-SPLIT-",
                                     IUCN.name == "Glaucestrilda thomensis" ~ "Estrilda thomensis",
                                     IUCN.name == "Myopornis boehmi" ~ "Bradornis boehmi",
                                     IUCN.name == "Brunhilda charmosyna" ~ "Estrilda charmosyna",
                                     IUCN.name == "Brunhilda erythronotos" ~ "Estrilda erythronotos",
                                     IUCN.name == "Delacourella capistrata" ~ "Nesocharis capistrata",
                                     IUCN.name == "Luscinia phaenicuroides" ~ "Hodgsonius phaenicuroides",
                                     IUCN.name == "Sigelus silens" ~ "Melaenornis silens",
                                     IUCN.name == "Cossyphicula isabellae" ~ "Oreocossypha isabellae",
                                     IUCN.name == "Namibornis herero" ~ "Melaenornis herero",
                                     IUCN.name == "Pardipicus nivosus" ~ "Campethera nivosa",
                                     IUCN.name == "Pardipicus caroli" ~ "Campethera caroli",
                                     IUCN.name == "Pampusana jobiensis" ~ "Alopecoenas jobiensis",
                                     IUCN.name == "Chionomesa fimbriata" ~ "Amazilia fimbriata",
                                     IUCN.name == "Athene jacquinoti" ~ "Ninox jacquinoti",
                                     IUCN.name == "Elliotomyia viridicauda" ~ "Amazilia viridicauda",
                                     IUCN.name == "Elliotomyia chionogaster" ~ "Amazilia chionogaster",
                                     IUCN.name == "Eupetomena cirrochloris" ~ "Aphantochroa cirrochloris",
                                     IUCN.name == "Saucerottia viridigaster" ~ "Amazilia viridigaster",
                                     IUCN.name == "Saucerottia tobaci" ~ "Amazilia tobaci",
                                     IUCN.name == "Pampusana beccarii" ~ "Alopecoenas beccarii",
                                     IUCN.name == "Pampusana johannae" ~ "Alopecoenas johannae",
                                     IUCN.name == "Dacelo rex" ~ "Clytoceyx rex",
                                     IUCN.name == "Chlorestes cyanus" ~ "Amazilia cyanus",
                                     IUCN.name == "Riccordia ricordii" ~ "Chlorostilbon ricordii",
                                     IUCN.name == "Riccordia swainsonii" ~ "Chlorostilbon swainsonii",
                                     IUCN.name == "Copsychus luzoniensis" ~ "Kittacincla luzoniensis",
                                     IUCN.name == "Copsychus niger" ~ "Kittacincla nigra",
                                     IUCN.name == "Copsychus superciliaris" ~ "Kittacincla superciliaris",
                                     IUCN.name == "Eumyias hoevelli" ~ "Cyornis hoevelli",
                                     IUCN.name == "Eumyias hyacinthinus" ~ "Cyornis hyacinthinus",
                                     IUCN.name == "Eumyias oscillans" ~ "Cyornis oscillans",
                                     IUCN.name == "Aethomyias rufescens" ~ "Sericornis rufescens",
                                     IUCN.name == "Aethomyias arfakianus" ~ "Sericornis arfakianus",
                                     IUCN.name == "Aethomyias perspicillatus" ~ "Sericornis perspicillatus",
                                     IUCN.name == "Aethomyias papuensis" ~ "Sericornis papuensis",
                                     IUCN.name == "Origma murina" ~ "Crateroscelis murina",
                                     IUCN.name == "Tetrastes bonasia" ~ "Bonasa bonasia",
                                     IUCN.name == "Campylopterus diamantinensis" ~ "Campylopterus largipennis-SPLIT-",
                                     IUCN.name == "Origma robusta" ~ "Crateroscelis robusta",
                                     IUCN.name == "Chrysuronia coeruleogularis" ~ "Amazilia coeruleogularis",
                                     .default = NA)) %>%
         mutate(donald.name.upd.type = ifelse(!is.na(donald.name.upd), "Genus update", NA),
                donald.name.upd.type = ifelse(grepl("-NEWLY DESCRIBED-",donald.name.upd), "-NEWLY DESCRIBED-", donald.name.upd.type),
                donald.name.upd.type = ifelse(grepl("-SPLIT-",donald.name.upd), "-SPLIT-", donald.name.upd.type),
                donald.name.upd = gsub("-NEWLY DESCRIBED-", "", donald.name.upd),
                donald.name.upd = gsub("-SPLIT-", "", donald.name.upd),
                donald.name.upd = ifelse(is.na(donald.name.upd), IUCN.name, donald.name.upd))

tax.match <- iucn.taxo.short.upd %>%
  left_join(donald.trade.short, by = c("donald.name.upd" = "trade.name")) %>%
  group_by(donald.name.upd) %>%
  mutate(donald.name.upd.type = ifelse(is.na(donald.name.upd.type) & n()>1,
                                       "-NOMINATE sp SPLIT-", donald.name.upd.type)) %>%
  select(-Common.name)


write.csv(tax.match, paste0(data.path, "Data/Donald.et.al.2024/IUCN.Donald.taxo.match.csv"))

## Resolve taxonomy - Wikipedia - AVES -----------------------------------------

## Binom match
wiki.binom <- iucn.taxo.short %>%
  left_join(wiki.species, by = c("IUCN.name" = "taxonName")) %>%
  rename("common.name" = "common.name.x")
wiki.binom.match <- wiki.binom %>% filter(!is.na(wikipediaURL)) 

## common name match
wiki.common <- wiki.binom %>% filter(is.na(wikipediaURL)) %>% 
  select(IUCN.name, common.name, status) %>%
  left_join(wiki.species, by = c("common.name"))
wiki.common.match <- wiki.common %>% filter(!is.na(wikipediaURL))

## manual final step
wiki.common %>% filter(is.na(wikipediaURL)) %>% 
  select(IUCN.name, common.name, status) %>%
  write.csv(paste0(data.path, "Data/Wikipedia/missing.links.csv"))
## read in corrected links
wiki.manual <- read.csv(paste0(data.path, "Data/Wikipedia/missing.links.corrected.csv")) %>%
  mutate(wikipediaURL = ifelse(wikipediaURL == "", NA, wikipediaURL))
wiki.manual %>% filter(is.na(wikipediaURL)) # 48 could not be clearly linked to pages

iucn.wiki.tax <- rbind(
  select(wiki.binom.match, IUCN.name, common.name, status, wikipediaURL),
  select(wiki.common.match, IUCN.name, common.name, status, wikipediaURL),
  select(wiki.manual, IUCN.name, common.name, status, wikipediaURL))

length(unique(iucn.wiki.tax$IUCN.name)) # 11031
iucn.wiki.tax %>% filter(is.na(wikipediaURL)) # 48

write.csv(iucn.wiki.tax, paste0(data.path, "Data/Wikipedia/IUCN.Wikipedia.taxo.match.csv"))

## Resolve taxonomy - Wikipedia - MAMMALS --------------------------------------

## Binom match
wiki.binom.mam <- iucn.mam.taxo.short %>%
  left_join(wiki.mammals, by = c("IUCN.name" = "taxonName")) %>%
  rename("common.name" = "common.name.x")
wiki.binom.mam.match <- wiki.binom.mam %>% filter(!is.na(wikipediaURL)) 

## common name match
wiki.mam.common <- wiki.binom.mam %>% filter(is.na(wikipediaURL)) %>% 
  select(IUCN.name, common.name, status) %>%
  left_join(wiki.mammals, by = c("common.name"))
wiki.mam.common.match <- wiki.mam.common %>% filter(!is.na(wikipediaURL))

## manual final step
wiki.mam.common %>% filter(is.na(wikipediaURL)) %>% 
  select(IUCN.name, common.name, status) %>%
  write.csv(paste0(data.path, "Data/Wikipedia/missing.mam.links.csv"))
## read in corrected links
wiki.mam.manual <- read.csv(paste0(data.path, "Data/Wikipedia/missing.mam.links.corrected.csv")) %>%
  mutate(wikipediaURL = ifelse(wikipediaURL == "", NA, wikipediaURL))
wiki.mam.manual %>% filter(is.na(wikipediaURL)) # 73 could not be clearly linked to pages

iucn.wiki.mam.tax <- rbind(
  select(wiki.binom.mam.match, IUCN.name, common.name, status, wikipediaURL),
  select(wiki.mam.common.match, IUCN.name, common.name, status, wikipediaURL),
  select(wiki.mam.manual, IUCN.name, common.name, status, wikipediaURL))

length(unique(iucn.wiki.mam.tax$IUCN.name)) # 5813
iucn.wiki.mam.tax %>% filter(is.na(wikipediaURL)) # 73

write.csv(iucn.wiki.mam.tax, paste0(data.path, "Data/Wikipedia/IUCN.Wikipedia.Mammals.taxo.match.csv"))

       
## Resolve taxonomy - SpUD - AVES ----------------------------------------------

spud.birds.short <- spud.birds %>% 
  filter(Type.of.use == "Extractive") %>%
  select(Scientific.name, Purpose.s..of.end.use, End.Year) %>% 
  rename("Purpose" = "Purpose.s..of.end.use", "SpUD.year" = "End.Year")

check <- spud.birds.short %>% left_join(iucn.taxo.short, by = c("Scientific.name" = "IUCN.name"))
unique((check %>% filter(is.na(status)))$Scientific.name)

spud.birds.match <- spud.birds.short %>% 
  mutate(IUCN.name = case_when(Scientific.name == "Butastur Rufipennis" ~ "Butastur rufipennis",
                               Scientific.name == "Ciconia Nigra" ~ "Ciconia nigra",
                               Scientific.name == "Accipiter Ovampensis" ~ "Accipiter ovampensis",
                               Scientific.name == "Acrocephalus Paludicola" ~ "Acrocephalus paludicola",
                               Scientific.name == "Anas Platyrhynchos" ~ "Anas platyrhynchos",
                               Scientific.name == "Gyps rueppellii" ~ "Gyps rueppelli",
                               Scientific.name == "Coracias garrulus " ~ "Coracias garrulus",
                               Scientific.name == "Eulemur rufrifrons" ~ NA,
                               Scientific.name == "Rallidae\xa0spp. " ~ NA,
                               Scientific.name == "Timaliidae" ~ NA,
                               Scientific.name == "White-headed Vulture" ~ "Trigonoceps occipitalis",
                               Scientific.name == "Haliaeetus leucoryphus " ~ "Haliaeetus leucoryphus",
                               .default = Scientific.name)) %>%
  filter(!is.na(IUCN.name))

test <- spud.birds.match %>% 
  left_join(iucn.taxo.short, by = "IUCN.name")

# three rows that cannot match, 2 at non-species resolution and one is a mammal.

write.csv(spud.birds.match, paste0(data.path, "Data/SpUD/iucn.spud.taxo.match.csv"))

## Resolve taxonomy - SpUD - MAMMALS -------------------------------------------

spud.mam.short <- spud.mam %>% 
  filter(Type.of.use == "Extractive") %>%
  select(Scientific.name, Purpose.s..of.end.use, End.Year) %>% 
  rename("Purpose" = "Purpose.s..of.end.use", "SpUD.year" = "End.Year")

check <- spud.mam.short %>% left_join(iucn.mam.taxo.short, by = c("Scientific.name" = "IUCN.name"))
unique((check %>% filter(is.na(status)))$Scientific.name)

spud.mam.match <- spud.mam.short %>% 
  mutate(IUCN.name = case_when(Scientific.name == "Monodon monoceros " ~ "Monodon monoceros",
                               Scientific.name == "Macropus robustus" ~ "Osphranter robustus",
                               Scientific.name == "Puma concolor " ~ "Puma concolor",
                               Scientific.name == "Ursus maritimus " ~ "Ursus maritimus",
                               Scientific.name == " Capra falconeri " ~ "Capra falconeri",
                               Scientific.name == "Lepus spp." ~ NA,
                               Scientific.name == "Petaurista ssp." ~ NA,
                               Scientific.name == "Sciuridae spp." ~ NA,
                               Scientific.name == "Rhizomys spp." ~ NA,
                               Scientific.name == "Prinodon pardicolor" ~ "Prionodon pardicolor",
                               Scientific.name == "Felis bengalensis" ~ "Prionailurus bengalensis",
                               Scientific.name == "Sus scrofa " ~ "Sus scrofa",
                               Scientific.name == "Muntiacus spp." ~ NA,
                               Scientific.name == "Capricornis sumatrensis" ~ "Capricornis sumatraensis",
                               .default = Scientific.name)) %>%
  filter(!is.na(IUCN.name))

test <- spud.mam.match %>% 
  left_join(iucn.taxo.short, by = "IUCN.name")

# five rows that cannot match, all at non-species resolution.

write.csv(spud.mam.match, paste0(data.path, "Data/SpUD/iucn.spud.mam.taxo.match.csv"))

## Resolve taxonomy - Benítez-López et al., 2017--------------------------------
BL.short <- BL.species %>% 
  mutate(Reference = str_replace_all(Reference, "[^[:print:]]", ""),
         BL.year = str_extract(Reference, "\\d{4}"),
         BL.year = as.integer(BL.year)) %>%
  select(Species, Study, BL.year) %>%
  separate_longer_delim(Species, ", ") %>%
  separate_longer_delim(Species, " and ") %>%
  filter(!grepl("spp", Species))
  

test <- BL.short %>% left_join(aves.mam.iucn, by = c("Species"= "IUCN.name"))
unique(filter(test, is.na(status))$Species)

BL.short.upd <- BL.short %>%
  mutate(IUCN.name = case_when(Species == "Leptotila rufaxila" ~ "Leptotila rufaxilla",
                               Species == "Odontophorus stellarus" ~ "Odontophorus stellatus",
                               Species == "Phalacrocorax brasilianus" ~ "Nannopterum brasilianum",
                               Species == "Aceros undulatus" ~ "Rhyticeros undulatus",
                               Species == "Claravis mondetoura" ~ "Paraclaravis mondetoura",
                               Species == "Penelope purpurascens aequatorialis" ~ "Penelope purpurascens",
                               Species == "Ramphastos ambiguus swainsonii" ~ "Ramphastos ambiguus",
                               Species == "Francolinus lathami" ~ "Peliperdix lathami",
                               Species == "Tockus camurus" ~ "Lophoceros camurus",
                               Species == "Tockus fasciatus" ~ "Lophoceros fasciatus",
                               Species == "Tropicranus albocristatus" ~ "Horizocerus albocristatus",
                               Species == "Crax rubra griscomi" ~ "Crax rubra",
                               Species == "Ara chloroptera" ~ "Ara chloropterus",
                               # mammals
                               Species == "Callicebus torquatus" ~ "Cheracebus torquatus",
                               Species == "Lagothrix lagotricha" ~ "Lagothrix lagothricha",
                               Species == "M. pandora" ~ "Mazama pandora",
                               Species == "C. dorsalis" ~ "Cephalophus dorsalis",
                               Species == "C. leucogaster" ~ "Cephalophus leucogaster",
                               Species == "C. nigrifrons" ~ "Cephalophus nigrifrons",
                               Species == "Neotragus moschatus" ~ "Nesotragus moschatus",
                               Species == "Cebus capuchinus" ~ "Cebus capucinus",
                               Species == "Herpailurus yaguarondi" ~ "Herpailurus yagouaroundi",
                               Species == "Procyon cancrivorous" ~ "Procyon cancrivorus",
                               Species == "P. lotor" ~ "Procyon lotor",
                               Species == "M. gouazoubira" ~ "Mazama gouazoubira",
                               Species == "Puma yagouaroundi" ~ "Herpailurus yagouaroundi",
                               Species == "Procolobus pennantii" ~ "Piliocolobus pennantii",
                               Species == "Pliocolobus badius" ~ "Piliocolobus badius",
                               Species == "Callicebus discolor" ~ "Plecturocebus discolor",
                               Species == "Lagothrix poeppigii" ~ "Lagothrix lagothricha",
                               Species == "Saguinus tripartitus" ~ "Leontocebus tripartitus",
                               Species == "Cephalophus nigrifons" ~ "Cephalophus nigrifrons",
                               Species == "Anomalurus sp" ~ NA,
                               Species == "L. wiedii" ~ "Leopardus wiedii",
                               Species == "M. parvidens" ~ "Marmosops parvidens",
                               Species == "Callicebus brunneus" ~ "Plecturocebus brunneus",
                               Species == "Lagothrix cana" ~ "Lagothrix lagothricha",
                               Species == "Saguinus fuscicollis" ~ "Leontocebus fuscicollis",
                               Species == "S.sanborni." ~ "Sciurus sanborni",
                               Species == "C. monticola" ~ "Philantomba monticola",
                               Species == "C. sylvicultor" ~ "Cephalophus silvicultor",
                               Species == "M. temama" ~ "Mazama temama",
                               Species == "C. weynsi" ~ "Cephalophus weynsi",
                               Species == "Procolobus gordonorum" ~ "Piliocolobus gordonorum",
                               Species == "C. ogilby" ~ "Cephalophus ogilbyi",
                               Species == "Syncerus caffer nanus" ~ "Syncerus caffer",
                               Species == "and C. dorsalis castaneus" ~ "Cephalophus dorsalis",
                               Species == "Galago alleni" ~ "Sciurocheirus alleni",
                               Species == "Galagoides demidovii" ~ "Galagoides demidoff",
                               Species == "C. ogilbyi" ~ "Cephalophus ogilbyi",
                               Species == "Pongo pygmaeus morio" ~ "Pongo pygmaeus",
                               Species == "Callicebus moloch" ~ "Plecturocebus moloch",
                               Species == "Saguinus imperator" ~ "Tamarinus imperator",
                               Species == "D. novemcinctus" ~ "Dasypus novemcinctus",
                               Species == "M. nemorivaga" ~ "Mazama nemorivaga",
                               Species == "Callicebus cupreus" ~ "Plecturocebus cupreus",
                               Species == "C. moloch" ~ "Plecturocebus moloch",
                               Species == "imperator" ~ "Tamarinus imperator",
                               Species == "Pseudalopex culpaeus smithersi" ~ "Lycalopex culpaeus",
                               Species == "C. silvicultor" ~ "Cephalophus silvicultor",
                               Species == "C. nictitans" ~ "Cercopithecus nictitans",
                               Species == "C. pogonias" ~ "Cercopithecus pogonias",
                               Species == "Saguinus geoffroyi" ~ "Oedipomidas geoffroyi",
                               .default = Species)) %>% 
  select(!Study)%>% distinct() %>% filter(!is.na(IUCN.name))
test <- BL.short.upd %>% left_join(aves.mam.iucn, by = c("IUCN.name"))

write.csv(BL.short.upd, paste0(data.path,
                              "Data/Benitez-Lopez_2017/IUCN.BL.taxo.match.csv"))

## Resolve taxonomy - Morton et al., 2021 --------------------------------------

Morton.filt <- Morton.species %>% filter(Class %in% c("Aves", "Mammalia"), Specieslevel == 1) %>%
  select(Class, SpeciesL, TP, Year) %>%
  rename("Mort.year" = "Year") %>%
  distinct()

Morton.corr <- Morton.filt %>% 
  mutate(IUCN.name = case_when(SpeciesL == "Cebus apella" ~ "Sapajus apella",
                               SpeciesL == "Cercopithecus mitis ssp.monoides" ~ "Cercopithecus mitis",
                               SpeciesL == "Tropicranus albocristatus" ~ "Horizocerus albocristatus",
                               SpeciesL == "Saguinus imperator" ~ "Tamarinus imperator",
                               SpeciesL == "Saguinus fuscicollis" ~ "Leontocebus fuscicollis",
                               SpeciesL == "Dasyprocta punctata ssp variegata" ~ "Dasyprocta punctata",
                               SpeciesL == "Lagothrix poeppigii" ~ "Lagothrix lagothricha",
                               .default = SpeciesL)) %>%
  left_join(aves.mam.iucn, by = c("IUCN.name"))

Morton.corr %>% filter(is.na(status))

write.csv(Morton.corr, paste0(data.path,
                          "Data/Morton_et_al_2021/IUCN.Morton.taxo.match.csv"))

## Resolve taxonomy - WILDMEAT -------------------------------------------------

## bird and mammal
## Dates can't be added to this data as functionality doesnt include bulk downloads
WM.filt <- WM.species %>% filter(Specieslevel == 1, Class %in% c("Aves", "Mammalia"))

WM.corr <- WM.filt %>%
  mutate(IUCN.name = case_when(Taxon == "Allochrocebus preussi ssp. insularis" ~ "Allochrocebus preussi",
                               Taxon == "Anas platyrhynchos ssp. domesticus" ~ "REMOVE",
                               Taxon == "Bos taurus ssp. domesticus" ~ "REMOVE",
                               Taxon == "Bos taurus ssp. indicus" ~ "REMOVE",
                               Taxon == "Bos taurus ssp. taurus" ~ "REMOVE",
                               Taxon == "Canis lupus ssp. familiaris" ~ "Canis lupus",
                               Taxon == "Cephalophus ogilbyi ssp. ogilbyi" ~ "Cephalophus ogilbyi",
                               Taxon == "Cercopithecus ascanius ssp. whitesidei" ~ "Cercopithecus ascanius",
                               Taxon == "Cercopithecus dryas" ~ "Chlorocebus dryas",
                               Taxon == "Cercopithecus erythrotis ssp. camerunensis" ~ "Cercopithecus erythrotis",
                               Taxon == "Cercopithecus erythrotis ssp. erythrotis" ~ "Cercopithecus erythrotis",
                               Taxon == "Cercopithecus nictitans ssp. martini" ~ "Cercopithecus nictitans",
                               Taxon == "Cercopithecus pogonias ssp. nigripes" ~ "Cercopithecus pogonias",
                               Taxon == "Cercopithecus pogonias ssp. pogonias" ~ "Cercopithecus pogonias",
                               Taxon == "Cercopithecus solatus" ~ "Allochrocebus solatus",
                               Taxon == "Cercopithecus wolfi ssp. wolfi" ~ "Cercopithecus wolfi",
                               Taxon == "Colobus satanas ssp. anthracinus" ~ "Colobus satanas",
                               Taxon == "Colobus satanas ssp. satanas" ~ "Colobus satanas",
                               Taxon == "Francolinus squamatus" ~ "Pternistis squamatus",
                               Taxon == "Gorilla gorilla ssp. gorilla" ~ "Gorilla gorilla",
                               Taxon == "Gallus gallus ssp. domesticus" ~ "REMOVE",
                               Taxon == "Herpestes naso" ~ "Xenogale naso",
                               Taxon == "Kobus ellipsiprymnus ssp. defassa" ~ "Kobus ellipsiprymnus",
                               Taxon == "Mandrillus leucophaeus ssp. poensis" ~ "Mandrillus leucophaeus",
                               Taxon == "Ovis aries ssp. domesticus" ~ "REMOVE",
                               Taxon == "Pan troglodytes ssp. troglodytes" ~ "Pan troglodytes",
                               Taxon == "Peliperdix coqui" ~ "Campocolinus coqui",
                               Taxon == "Sciurocheirus alleni ssp. alleni" ~ "Sciurocheirus alleni",
                               Taxon == "Sus scrofa ssp. domesticus" ~ "REMOVE",
                               Taxon == "Syncerus caffer ssp. nanus" ~ "Syncerus caffer",
                               Taxon == "Tauraco persa ssp. persa" ~ "Tauraco persa",
                               .default = Taxon)) %>% 
  left_join(aves.mam.iucn, by = c("IUCN.name")) %>%
  filter(IUCN.name != "REMOVE")

write.csv(WM.corr, paste0(data.path,
                               "Data/WILDMEAT/IUCN.WM.taxo.match.csv"))

## Resolve taxonomy - Marshall/New LEMIS ---------------------------------------

mam.syns <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.synonyms.MAMMALIA.Jun25.csv")) %>% filter(!is.na(syn.name))
aves.syns <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.synonyms.Jun25.csv")) %>% filter(!is.na(syn.name))

all.syns <- rbind(aves.syns, mam.syns)
## 7376 lemis sp
lemis.sp.ls <- LEMIS.species %>% 
  #filter(group_ == "Birds") %>%
  select(corrected) %>% distinct()

test <- lemis.sp.ls %>% left_join(aves.mam.iucn, by = c("corrected" = "IUCN.name")) %>%
  filter(is.na(status))

syn.match <- test %>% select(corrected) %>% left_join(all.syns, by = c("corrected" = "syn.name"))

missing <- syn.match %>% filter(is.na(IUCN.name))
syn.dir.cor <- syn.match %>% filter(!is.na(IUCN.name)) %>%
  group_by(corrected) %>% filter(n()==1)
syn.multi.corr <- syn.match %>% filter(!is.na(IUCN.name)) %>%
  group_by(corrected) %>% filter(n()>1) %>%
  filter(!IUCN.name %in% c("Clibanornis rufipectus", "Antrostomus arizonae",
                           "Setophaga flavescens", "Hydrornis irena", 
                           "Melopyrrha grandis"))
## sift manually
write.csv(syn.multi.corr, paste0(data.path,
                          "Data/LEMIS/lemis.to.correct.multi.csv"))
write.csv(missing, paste0(data.path,
                       "Data/LEMIS/lemis.to.correct.csv"))
## read in corrected
syn.mult.corr <- read.csv(paste0(data.path,"Data/LEMIS/lemis.corrected.multi.csv"))
syn.corr <- read.csv(paste0(data.path,"Data/LEMIS/lemis.corrected.csv"))

all.corr <- syn.corr %>% filter(!IUCN.name %in% c("EXTINCT", "DOMESTIC", "Not Assessed")) %>%
  rbind(select(syn.mult.corr, corrected,  IUCN.name)) %>%
  rbind(select(syn.dir.cor, corrected,  IUCN.name))

lemis.iucn.fix <- lemis.sp.ls %>% left_join(aves.mam.iucn, by = c("corrected" = "IUCN.name")) %>%
  filter(!is.na(status)) %>% select(-status, -common.name) %>%
  mutate(IUCN.name = corrected) %>%
  rbind(all.corr)

## 7376 lemis sp matched to 7332 extant IUCN sp 
# 43 were "EXTINCT", "DOMESTIC", "Not Assessed" and one was mislabelled and is a fish.
# add metadata back
lemis.iucn.fix <- lemis.iucn.fix %>% left_join(select(LEMIS.species, -X, -correctedGenus))
write.csv(lemis.iucn.fix, paste0(data.path,
                                "Data/LEMIS/IUCN.LEMIS.taxo.match.csv"))

## Resolve taxonomy - CITES v2025.1 --------------------------------------------

avo.match <- read.csv(paste0(data.path, "Data/Tobias.et.al.AVONET/IUCN.AVONET.taxo.match.csv"))

CITES.ls <- CITES.species %>% distinct(Taxon) %>%
  filter(!grepl("ssp", Taxon), !grepl("spp", Taxon),!grepl("hybrid", Taxon))

intial <- CITES.ls %>% left_join(aves.mam.iucn, by = c("Taxon" = "IUCN.name")) 

intial %>% filter(is.na(common.name)) %>%
  left_join(lemis.iucn.fix, by = c("Taxon" = "corrected")) %>%
  filter(is.na(IUCN.name)) %>%
  left_join(avo.match, by = c("Taxon" = "AVONET.sp")) %>%
  filter(is.na(IUCN.name.y)) %>%
  write.csv(paste0(data.path,"Data/CITES/CITES.name.to.correct.csv"))

cites.corr <- read.csv(paste0(data.path,"Data/CITES/CITES.name.corrected.csv"))

upd.1 <- intial %>% filter(is.na(common.name)) %>%
  left_join(lemis.iucn.fix, by = c("Taxon" = "corrected")) %>% 
  filter(!is.na(IUCN.name)) %>% select(Taxon, IUCN.name)

upd.2 <- intial %>% filter(is.na(common.name)) %>%
  left_join(lemis.iucn.fix, by = c("Taxon" = "corrected")) %>%
  filter(is.na(IUCN.name)) %>%
  left_join(avo.match, by = c("Taxon" = "AVONET.sp")) %>%
  filter(!is.na(IUCN.name.y)) %>%
  select(Taxon, IUCN.name.y) %>% rename("IUCN.name" = "IUCN.name.y")

intial.match <- select(filter(intial, !is.na(common.name)), -status, - common.name) %>%
  mutate(IUCN.name = Taxon)

cites.all.match <- rbind(intial.match,
                         filter(cites.corr, IUCN.name != "NR"), upd.1, upd.2)

## 2048 cites species matched to 2042
# 6 were "EXTINCT", "DOMESTIC", "Not Assessed".
# add metadata
cites.all.match <- cites.all.match %>% distinct() %>% left_join(select(CITES.species, - X))
write.csv(cites.all.match, paste0(data.path,
                                 "Data/CITES/IUCN.CITES.taxo.match.csv"))

## Resolve taxonomy - WiTIS ----------------------------------------------------
WiTIS.clean <- WiTIS.species %>% filter(Class %in% c("Aves", "Mammalia"), Genus != "", Species!= "") %>%
  mutate(Taxon = paste(Genus, Species),
         Witis.Year = year(dmy_hm(Date.of.Incident)))
write.csv(WiTIS.clean, paste0(data.path,
                              "Data/WiTIS/summary.species.clean.csv"))

WiTIS.sp <- WiTIS.clean %>% select(Taxon) %>% distinct(Taxon)
lemis.iucn.fix <- read.csv( paste0(data.path,
                                 "Data/LEMIS/IUCN.LEMIS.taxo.match.csv"))
WiTIS.sp %>% left_join(aves.mam.iucn, by = c("Taxon" = "IUCN.name")) %>%
  filter(is.na(status)) %>%
  left_join(lemis.iucn.fix, by = c("Taxon" = "corrected")) %>% 
  filter(is.na(IUCN.name)) %>%
  write.csv(paste0(data.path,
                   "Data/WiTIS/names.to.correct.csv"))

names.corr <- read.csv(paste0(data.path,
                "Data/WiTIS/names.corrected.csv"))

upd.1 <-WiTIS.sp %>% left_join(aves.mam.iucn, by = c("Taxon" = "IUCN.name")) %>%
  filter(!is.na(status)) %>% select(Taxon) %>%
  mutate(IUCN.name = Taxon)

upd.2 <-WiTIS.sp %>% left_join(aves.mam.iucn, by = c("Taxon" = "IUCN.name")) %>%
  filter(is.na(status)) %>%
  left_join(lemis.iucn.fix, by = c("Taxon" = "corrected")) %>% 
  filter(!is.na(IUCN.name)) %>%
  select(Taxon, IUCN.name)

## 1571 Witis species entries
## 13 are extinct, hybrids or domestic breeds (1558 now)
WiTIS.IUCN.match <- rbind(names.corr, upd.1, upd.2) %>%
  filter(!IUCN.name %in% c("EXTINCT", "DOMESTIC", "HYBRID")) %>% distinct()
WiTIS.IUCN.match <- WiTIS.IUCN.match %>% left_join(select(WiTIS.clean, Taxon, Category.of.Incident,
                                      Item...Commodity.Type, Worked.Product.Type, Witis.Year))
write.csv(WiTIS.IUCN.match, paste0(data.path,
                                  "Data/WiTIS/IUCN.WiTIS.taxo.match.csv"))
## Resolve taxonomy - Tobias/AVONET data ---------------------------------------
avonet.short <- avonet.df %>% select(Species1, Mass)

## Used expanded version of prev key
avonet.key.df <- read.csv(paste0(data.path,
                             "Data/Tobias.et.al.AVONET/MSc.key.csv")) %>% distinct()

# 5 species no traits - poorly known sp.
iucn.taxo.short %>% left_join(avonet.key.df, by = c("IUCN.name"))%>%
  filter(is.na(AVONET.sp))

write.csv(avonet.key.df, paste0(data.path,
                          "Data/Tobias.et.al.AVONET/IUCN.AVONET.taxo.match.csv"))

## Resolve taxonomy - Bird et al., 2020 ----------------------------------------

lh.short <- bird.lh.df %>% select(Scientific.name, Adult.survival)

avonet.key.df <- read.csv(paste0(data.path,
                                 "Data/Tobias.et.al.AVONET/MSc.key.csv")) %>% distinct()

avo.match <- lh.short %>% left_join(avonet.key.df, by = c("Scientific.name"="AVONET.sp")) %>%
  rename("BirdLH.sp" = "Scientific.name")

BirdLH.match <- iucn.taxo.short %>% left_join(avo.match) %>%
  mutate(BirdLH.sp = case_when(IUCN.name == "Nicopsitta calthrapae" ~"Psittacula calthrapae",
                               IUCN.name == "Gallirallus lafresnayanus" ~"MISSING",
                               IUCN.name == "Laterallus spilopterus" ~"Porzana spiloptera",
                               IUCN.name == "Rubigula melanictera" ~"Pycnonotus melanicterus",
                               IUCN.name == "Ixidia squamata" ~"Pycnonotus squamatus",
                               IUCN.name == "Ixidia erythropthalmos" ~"Pycnonotus erythropthalmos",
                               IUCN.name == "Nok hualon" ~"Pycnonotus hualon",
                               IUCN.name == "Palaeornis eupatria" ~"Psittacula eupatria",
                               IUCN.name == "Hypsipetes aureus" ~"Thapsinillas aurea",
                               IUCN.name == "Oroanassa magnifica" ~"Gorsachius magnificus",
                               IUCN.name == "Paraclaravis geoffroyi" ~"Claravis geoffroyi",
                               IUCN.name == "Athene blewitti" ~"Heteroglaux blewitti",
                               IUCN.name == "Hypsipetes platenae" ~"Thapsinillas platenae",
                               IUCN.name == "Laterallus flaviventer" ~"Hapalocrex flaviventer",
                               IUCN.name == "Chlorestes eliciae" ~"Hylocharis eliciae",
                               IUCN.name == "Alexandrinus eques" ~"Psittacula eques",
                               IUCN.name == "Eclectus polychloros" ~"Eclectus roratus",
                               IUCN.name == "Eclectus riedeli" ~"Eclectus roratus",
                               IUCN.name == "Eclectus cornelia" ~"Eclectus roratus",
                               IUCN.name == "Poecile weigoldicus" ~"Poecile montanus",
                               IUCN.name == "Zosterops auriventer" ~"Zosterops palpebrosus",
                               IUCN.name == "Zosterops simplex" ~"Zosterops japonicus",
                               IUCN.name == "Catharus maculatus" ~"Catharus dryas",
                               IUCN.name == "Pelecanoides whenuahouensis" ~"Pelecanoides georgicus",
                               IUCN.name == "Machaeropterus eckelberryi" ~"Machaeropterus striolatus", # closely resembles
                               IUCN.name == "Otus cyprius" ~"Otus scops",
                               IUCN.name == "Haematopus finschi" ~"Haematopus ostralegus",
                               IUCN.name == "Pycnonotus pseudosimplex" ~"Pycnonotus simplex",
                               IUCN.name == "Phylloscopus rotiensis" ~"Phylloscopus presbytes",
                               IUCN.name == "Megascops gilesi" ~"Megascops choliba", # close rel
                               IUCN.name == "Macropygia macassariensis" ~"Macropygia magna",
                               IUCN.name == "Macropygia timorlaoensis" ~"Macropygia magna",
                               IUCN.name == "Malurus assimilis" ~"Malurus lamberti",
                               IUCN.name == "Phaethornis major" ~"Phaethornis bourcieri",
                               IUCN.name == "Pyrrhura chapmani" ~"Pyrrhura melanura",
                               IUCN.name == "Microptilotis imitatrix" ~"Microptilotis gracilis",
                               IUCN.name == "Arremon dorbignii" ~"Arremon flavirostris",
                               IUCN.name == "Cypsiurus gracilis" ~"Cypsiurus parvus",
                               IUCN.name == "Vireo chivi" ~"Vireo olivaceus",
                               IUCN.name == "Paraclaravis mondetoura" ~"Claravis mondetoura",
                               IUCN.name == "Aramides albiventris" ~"Aramides cajaneus",
                               IUCN.name == "Chrysuronia coeruleogularis" ~"Lepidopyga coeruleogularis",
                               IUCN.name == "Chlorestes julie" ~"Juliamyia julie",
                               IUCN.name == "Anthocephala berlepschi" ~"Anthocephala floriceps",
                               IUCN.name == "Celeus galeatus" ~"Hylatomus galeatus",
                               IUCN.name == "Myrmoderus eowilsoni" ~"MISSING",
                               IUCN.name == "Dicaeum dayakorum" ~"MISSING",
                               IUCN.name == "Buccanodon dowsetti" ~"MISSING",
                               IUCN.name == "Cercococcyx lemaireae" ~"MISSING",
                               IUCN.name == "Anas carolinensis" ~"Anas crecca",
                               IUCN.name == "Lophorina minor" ~"Lophorina superba",
                               IUCN.name == "Lophorina latipennis" ~"Lophorina superba",
                               IUCN.name == "Ardea occidentalis" ~"MISSING",
                               IUCN.name == "Chaetura andrei" ~"Chaetura vauxi",
                               IUCN.name == "Tanygnathus everetti" ~"Tanygnathus sumatranus",
                               IUCN.name == "Aplonis circumscripta" ~"Aplonis metallica",
                               IUCN.name == "Scytalopus alvarezlopezi" ~"MISSING",
                               IUCN.name == "Turacoena sulaensis" ~"Turacoena manadensis",
                               IUCN.name == "Rubigula dispar" ~"Pycnonotus dispar",
                               IUCN.name == "Zosterops melanurus" ~"Zosterops palpebrosus",
                               IUCN.name == "Chrysuronia lilliae" ~"Lepidopyga lilliae",
                               IUCN.name == "Laterallus rogersi" ~"Atlantisia rogersi",
                               IUCN.name == "Milvus aegyptius" ~"Milvus migrans",
                               IUCN.name == "Oreotrochilus cyanolaemus" ~"Oreotrochilus stolzmanni",
                               IUCN.name == "Locustella portenta" ~"Locustella castanea",
                               IUCN.name == "Heliothraupis oneilli" ~"MISSING",
                               IUCN.name == "Myzomela prawiradilagae" ~"Myzomela kuehni",
                               IUCN.name == "Guyramemua affinis" ~"Suiriri affinis",
                               IUCN.name == "Chrysuronia oenone" ~"Chrysuronia oenone",
                               IUCN.name == "Chrysuronia goudoti" ~"Lepidopyga goudoti",
                               IUCN.name == "Iole finschii" ~"Alophoixus finschii",
                               IUCN.name == "Fregetta lineata" ~"MISSING",
                               IUCN.name == "Zosterops paruhbesar" ~"MISSING",
                               IUCN.name == "Gypsophila annamensis" ~"MISSING",
                               IUCN.name == "Himalayapsitta roseata " ~"Psittacula roseata",
                               IUCN.name == "Montecincla fairbanki" ~"Trochalopteron fairbanki",
                               IUCN.name == "Sholicola major" ~"MISSING",
                               IUCN.name == "Montecincla jerdoni" ~"Trochalopteron jerdoni",
                               IUCN.name == "Montecincla cachinnans" ~"Trochalopteron cachinnans",
                               IUCN.name == "Montecincla meridionalis" ~"Trochalopteron meridionale",
                               IUCN.name == "Sholicola albiventris" ~"Myiomela albiventris",
                               IUCN.name == "Belocercus longicaudus" ~"Psittacula longicauda",
                               IUCN.name == "Alcurus striatus" ~"Pycnonotus striatus ",
                               IUCN.name == "Himalayapsitta finschii" ~"Psittacula finschii",
                               IUCN.name == "Ixidia cyaniventris" ~"Pycnonotus cyaniventris",
                               IUCN.name == "Trichoglossus iris" ~"Psitteuteles iris",
                               IUCN.name == "Phyllaemulor bracteatus" ~"Nyctibius bracteatus",
                               IUCN.name == "Hylocharis chrysura" ~"Hylocharis chrysura",
                               IUCN.name == "Nicopsitta columboides" ~"Psittacula columboides",
                               IUCN.name == "Himalayapsitta cyanocephala" ~"Psittacula cyanocephala",
                               IUCN.name == "Himalayapsitta himalayana" ~"Psittacula himalayana",
                               IUCN.name == "Alexandrinus krameri" ~"Psittacula krameri",
                               IUCN.name == "Chlorestes cyanus" ~"Hylocharis cyanus",
                               IUCN.name == "Hemixos leucogrammicus" ~"Pycnonotus leucogrammicus",
                               IUCN.name == "Hypsipetes lucasi" ~"Thapsinillas lucasi",
                               IUCN.name == "Rubigula gularis" ~"Pycnonotus gularis",
                               IUCN.name == "Rubigula montis" ~"Pycnonotus montis",
                               IUCN.name == "Rubigula flaviventris" ~"Pycnonotus flaviventris",
                               IUCN.name == "Atronanus fuliginosus" ~"Petrochelidon fuliginosa",
                               IUCN.name == "Hypsipetes affinis" ~"Thapsinillas affinis",
                               IUCN.name == "Hypsipetes longirostris" ~"Thapsinillas longirostris",
                               IUCN.name == "Hypsipetes mysticalis" ~"Thapsinillas mysticalis",
                               IUCN.name == "Hypsipetes harterti" ~"Thapsinillas harterti",
                               IUCN.name == "Hypsipetes thompsoni" ~"Cerasophila thompsoni",
                               IUCN.name == "Zosterops emiliae" ~"Chlorocharis emiliae",
                               IUCN.name == "Hypsipetes chloris" ~"Thapsinillas chloris",
                               IUCN.name == "Kurochkinegramma hypogrammica" ~"Arachnothera hypogrammica",
                               IUCN.name == "Paragallinula angulata" ~"Gallinula angulata",
                               IUCN.name == "Porphyriops melanops" ~"Gallinula melanops",
                               .default = BirdLH.sp)) %>% 
  select(-Adult.survival, - common.name, - status)

write.csv(BirdLH.match, paste0(data.path,
                                "Data/Bird.et.al.2020/IUCN.BirdLH.taxo.match.csv"))

## Resolve taxonomy - Santageli et al., 2023 -----------------------------------
santageli.df %>% select(sciName_HBWBLv5) %>% filter(!is.na(sciName_HBWBLv5)) %>% 
  distinct() %>% mutate(pres = 1) # ~9400 species

## Resolve taxonomy - Soria et al., 2023 ---------------------------------------
mam.syns <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.synonyms.MAMMALIA.Jun25.csv")) %>% 
  filter(!is.na(syn.name)) %>% select(-X)

soria.short <- soria.df %>% select(iucn2020_binomial, phylacine_binomial) 

f <- iucn.mam.taxo.short %>% 
  left_join(soria.short, by = c("IUCN.name" = "iucn2020_binomial")) %>% 
  filter(is.na(phylacine_binomial))

syn.join <- mam.syns %>% filter(IUCN.name %in% f$IUCN.name) %>% 
  left_join(soria.short, by = c("syn.name" = "iucn2020_binomial"))

