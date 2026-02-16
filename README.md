# Morton et al., (2026)

Outputs currently in submission as 'The global footprint of species-use across space and ecology'  
Oscar Morton, Sharon Baruch-Mordo, Chris R. Cooney, & David P. Edwards.

See Methods for details on the data extraction, use classification, modelling approaches and threat assessment framework.

### File summary

#### Data Preparation

***01.get.IUCN.data.R*** - Pull various data from the IUCN API v4.

***02.resolve.taxonomies.R*** - Resolve taxonomies across all key datasets.

***03.collate.wiki.data.R*** - Scrape and tidy all species Wikipedia pages.

***04.species.use.collation.R*** - Collate and apply our use critera to all data sources, creating a final database of species uses.


#### Analysis

***05.used.species.summaries.R*** - Tidy and summarise species uses.

***06.predicting.use.R*** - Collate trait data, train and optimize use specific random forest models. Includes model evaluation.

***07.use.specific.threat.R*** - Build and apply the automated use-threat framework.



***functions. R*** - minimal list of post-processing RF model functions for convenience.