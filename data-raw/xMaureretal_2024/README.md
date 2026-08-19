# Landscape simplification leads to loss of plant-pollinator interaction diversity and flower visitation frequency despite buffering by abundant generalist pollinators

[https://doi.org/10.5061/dryad.1zcrjdg0b](https://doi.org/10.5061/dryad.1zcrjdg0b)

## Description of the data and file structure

The Data.zip folder contains three CSV files:

### LandscapeData.csv

Data on landscape-scale arable crop cover per landscape was calculated using the R package *landscapemetrics* (Hesselbarth, 2019).

*   \[Site] = ID of the study landscape
*   \[arable_PLAND_1000] = Arable crop cover (%) was measured in a 1000 m radius around the center of the study landscapes
*   \[arable_PLAND_500] = Arable crop cover (%) was measured in a 500 m radius  around the center of the study landscapes
*   \[Country] = Country where the landscapes were located (CH = Switzerland; FR = France; GE = Germany)

### InteractionData.csv

Plant-pollinator interactions from transect data of the 24 landscapes (n=8 per country) and 3 sampling rounds.

*   \[Country] = Country where the landscapes were located (CH = Switzerland; FR = France; GE = Germany)
*   \[Sampling.Round] = 3 sampling rounds
*   \[Site] = ID of the study landscape
*   \[Landuse.Type] = major land-use types, very broad categorization
    *   "Intensive.agriculture" and "Rural.habitat.mosaic" are factor levels
    *   Intensive.agriculture = landscapes largely dominated by monocultures, few semi-natural habitat elements
    *   Rural.habitat.mosaic = landscape also dominated by agriculture, but more small scale and with interspersed semi-natural habitat elements
*   \[Best.visited.flower.species] = species of the visited flower
*   \[Best.insect.ID] = species of the flower-visiting bee or hoverfly
*   \[insect.abundance] = the number of times the specific interaction was observed

### FlowerData.csv

Flower survey data from the 24 landscapes (n=8 per country) and 3 sampling rounds.

*   \[Site] = ID of the study landscape
*   \[Sampling.Round] = 3 sampling rounds
*   \[Flower.species] = calculated as flower area, in cm^2, was approximated as circle area with flower diameter/2 = radius, Ammann et al. 2024.
*   \[Flower.abundance.cm2] = Flower abundance of each flowering plant species
*   \[Landuse.Type] = major land-use types, very broad categorization
    *   "Intensive.agriculture" and "Rural.habitat.mosaic" are factor levels
    *   Intensive.agriculture = landscapes largely dominated by monocultures, few semi-natural habitat elements
    *   Rural.habitat.mosaic = landscape also dominated by agriculture, but more small scale and with interspersed semi-natural habitat elements
*   \[Country] = Country where the landscapes were located (CH = Switzerland; FR = France; GE = Germany)

