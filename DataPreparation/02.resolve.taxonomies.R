
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
spud.species <- read.csv(paste0(data.path, "Data/SpUD/iucn.spud.birds.jul25.csv"))

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

## AVONET trait data
avonet.df <- read.csv(paste0(data.path,
                                  "Data/Tobias.et.al.AVONET/AVONET.data.csv"))

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

       
## Resolve taxonomy - SpUD -----------------------------------------------------

spud.short <- spud.species %>% 
  filter(Type.of.use == "Extractive") %>%
  select(Scientific.name, Purpose.s..of.end.use) %>% 
  rename("Purpose" = "Purpose.s..of.end.use")

check <- spud.short %>% left_join(iucn.taxo.short, by = c("Scientific.name" = "IUCN.name"))
unique((check %>% filter(is.na(status)))$Scientific.name)

spud.match <- spud.short %>% 
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

test <- spud.match %>% 
  left_join(iucn.taxo.short, by = "IUCN.name")

# three rows that cannot match, 2 at non-species resolution and one is a mammal.

write.csv(spud.match, paste0(data.path, "Data/SpUD/iucn.spud.taxo.match.csv"))

## Resolve taxonomy - Benítez-López et al., 2017--------------------------------
BL.short <- BL.species %>% 
  filter(Group == "Birds") %>% select(Species, Study) %>%
  separate_longer_delim(Species, ", ") %>%
  filter(!grepl("spp", Species))
  

test <- BL.short %>% left_join(iucn.taxo.short, by = c("Species"= "IUCN.name"))
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
                               .default = Species))
test <- BL.short.upd %>% left_join(iucn.taxo.short, by = c("IUCN.name"))

write.csv(BL.short.upd, paste0(data.path,
                              "Data/Benitez-Lopez_2017/IUCN.BL.taxo.match.csv"))

## Resolve taxonomy - Morton et al., 2021 --------------------------------------

Morton.filt <- Morton.species %>% filter(Class %in% c("Aves", "Mammalia"), Specieslevel == 1) %>%
  select(Class, SpeciesL, TP) %>%
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

## Resovle taxonomy - Marshall/New LEMIS ---------------------------------------
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
