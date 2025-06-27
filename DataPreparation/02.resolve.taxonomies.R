
library(tidyverse)

## read in data ----------------------------------------------------------------
data.path <- "X:/morton_research/User/bi1om/Research/Wildlife_trade/Morton_et_al_TradePurposes/Analysis/"

## IUCN taxonomy Jun 2025
iucn.taxo <- read.csv(paste0(data.path, "Data/IUCN/raw.iucn.taxonomy.Jun25.csv"))

## Wikipedia species binomials
wiki.species <- read.csv(paste0(data.path, "Data/Wikipedia/all.birds.wiki.csv"))

donald.trade <- read.csv(paste0(data.path, "Data/Donald.et.al.2024/Donald.TableS5.csv"))
# remove entries for three speciens (binomial names) where geographic ssp are included with
# distinct common names (effects Acridotheres melanopterus, Calendulauda africanoides,
# Mirafra somalica)
donald.trade <- donald.trade %>%
  filter(!Common.name %in% c("Grey-rumped Myna", "Grey-backed Myna",
                             "Foxy Lark", "Ash's Lark"))

## Resolve taxonomy - Donald et al. 2024 ---------------------------------------

## 11031
iucn.taxo.short <- iucn.taxo %>% filter(status != "EX") %>% 
  select(IUCN.name, status)
## 10999
donald.trade.short <- donald.trade %>% rename("trade.name" = "Scientific.name") %>%
  select(trade.name, Common.name, Trade.Prevalence.Score)

nrow(donald.trade.short) #10999
nrow(filter(donald.trade.short, Trade.Prevalence.Score>0)) #4915

# tr.sp <- donald.trade.short %>% filter(Trade.Prevalence.Score>0) # 4915
# order.use <- read.csv( paste0(data.path, "Data/IUCN/raw.iucn.use.Jun25.csv"))
# uses <- order.use %>% filter(!is.na(description))
# unique(uses$description) # 4839
# length(unique(uses$IUCN.name))


tax.match <- iucn.taxo.short %>%
  left_join(donald.trade.short, by = c("IUCN.name" = "trade.name"))

donald.sp <- tax.match %>% filter(!is.na(Trade.Prevalence.Score)) #10,812



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
  mutate(donald.name.upd.type = ifelse(is.na(donald.name.upd.type) & n()>1, "-NOMINATE sp SPLIT-", donald.name.upd.type))

## 


## Resolve taxonomy - Wikipedia ------------------------------------------------

wiki.match <- iucn.taxo.short %>%
  left_join(wiki.species, by = c("IUCN.name" = "taxonName"))

wiki.match <-   wiki.species%>%
  left_join(iucn.taxo.short, by = c( "taxonName"="IUCN.name"))

wiki.match %>% filter(is.na(wikipediaURL))
wiki.match %>% filter(is.na(status))

iucn.taxo.short %>% group_by(IUCN.name) %>% filter(n()>1)
f <- wiki.species %>% group_by(taxonName) %>% filter(n()>1)
