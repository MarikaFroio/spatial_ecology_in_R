> #### Marika Froio
>> ##### student n.

# São Miguel fish community analysis along the south coast (2026)

---

# Introduction

São Miguel is the largest island of the Azores, located in the North Atlantic Ocean. 
Its surrounding waters are home to a rich variety of marine species and diverse coastal habitats; however, relatively few studies have focused on the fish communities around the island. 

This lack of information makes São Miguel an interesting area for studying fish biodiversity and community structure.

Understanding these patterns is essential for improving knowledge and supporting the conservation of São Miguel’s marine ecosystems.

---

# R project objectives

This study focuses on the southern coast of São Miguel, with the aim of investigating the local fish community. The main analyses focus on:
- **species richness** and **mean abundance** of the recorded species
- **Species distribution**, with a focus on species classified as *Endangered* or *Near Threatened*   according to the IUCN Red List
- **Temporal variation in species abundance** in relation to the time of day and sampling date over   a five-month period
- **Detection patterns**, using kernel density estimation to identify periods with the highest        frequency of fish detections
- **Potential indirect interactions**, exploring competition for space among species in     relation to body size and the spatial area they occupy

---

# Data gathering and Methodology
Data were collected through snorkeling surveys along 15 m transects, located at approximately the same distance from the coastline, at four different sites along the southern coast of São Miguel: Praia da Pedreira, Praia Ribeira das Tainhas, Praia Baixa d’Areia, and Praia do Pópulo.

Sampling was conducted over a five-month period, with a total of five transects for each site.
For each transect, the data recorded included fish species, relative abundance and body size, as well as the date and time of sampling and a set of environmental variables. 

All collected data were organized and stored in an Excel file for subsequent analysis.

---

## language and working directory setup
```md
Sys.setlocale("LC_TIME", "C")
setwd("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project")
```
## packages upload
```md
library("tidyverse")
library("lubridate")
library("vegan")
library(dplyr)
library(ggplot2)
library(mapview)
library(overlap)
library(igraph)
library(terra)
```
## file upload
```md
fish <- read.csv2("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project/sao miguel fish species.csv", fileEncoding = "Windows-1252")
```
## data check
```md
glimpse(fish)
```
## data correction
```md
fish[fish == ""] <- NA
fish<- fish|> fill(site, date, visibility.m, tide, water.T..C, wave.height.m, wave.period.s, wave.power.kW.m, wind.km.h, level, time)
fish$date<-dmy(fish$date)
fish$abundances<-as.numeric(fish$abundances)
fish$size.cm<-as.numeric(fish$size.cm)
fish$visibility.m<-as.numeric(fish$visibility.m)
fish$water.T..C<-as.numeric(fish$water.T..C)
fish$wave.height.m<-as.numeric(fish$wave.height.m)
fish$wave.power.kW.m<-as.numeric(fish$wave.power.kW.m)
fish$wave.period.s<-as.numeric(fish$wave.period.s)
fish$wind.km.h<-as.numeric(fish$wind.km.h)
fish$time <- hm(fish$time)
```
## 1. STUDY OF SPECIES RICHNESS
```md
sc_richness <- fish |>
  summarise(species_richness = n_distinct(fish.species))
sc_richness
# table with species names
names_sc_richness <- fish |>
  distinct(fish.species) |>
  arrange(fish.species)
names_sc_richness
```
|  | fish species|  | fish species|
|---:|---|---:|---|
| 1 |axillary wrasse        | 10 |portuguese blenny      |
| 2 |pufferfish             | 11 |blue damselfish        |
| 3 |rainbow wrasse         | 12 |bogue                  | 
| 4 |saddled seabream       | 13 |dusky grouper juvenile |
| 5 |salema                 | 14 |flounder               |
| 6 |striped red mullet     | 15 |garfish                |
| 7 |thicklip grey mullet   | 16 |madeira rockfish       |
| 8 |two-banded seabream    | 17 |ornate wrasse          |
|9 9|white seabream         | 18 |parrotfish             |

## 2. STUDY OF MEAN ABUNDANCE OF EACH SPECIES
```md
mean_abundance_overall <- fish |>
  group_by(fish.species) |>
  summarise(
    mean_abundance = mean(abundances, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_abundance))
mean_abundance_overall
```
|fish.species           | mean_abundance|
|:----------------------|--------------:|
|salema                 |      38.8|
|white seabream         |      34.2|
|bogue                  |      20|
|two-banded seabream    |       6.1|
|rainbow wrasse         |       3|
|thicklip grey mullet   |       3|
|parrotfish             |       2.7|
|dusky grouper juvenile |       2|
|saddled seabream       |       2|
|striped red mullet     |       1|
|ornate wrasse          |       1.3|
|axillary wrasse        |       1|
|blue damselfish        |       1|
|flounder               |       1|
|garfish                |       1|
|madeira rockfish       |       1|
|portuguese blenny      |       1|
|pufferfish             |       1|

### Mean abundance heatmap
```md
ggplot(mean_abundance_overall,
    aes(x = "abundance",y = reorder(fish.species, mean_abundance),
    fill = mean_abundance)) +
geom_tile(color = "white") +
geom_text(
    aes(label = round(mean_abundance, 2)),
    color = "black",
    size = 3.5) +
scale_fill_gradient(low = "yellow", high = "orange") +
labs(
  x = NULL,
  y = "Species",
  fill = "Mean abundance gradient",
  title = "Mean abundance of fish species") +
theme_minimal() +
theme(
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(size = 9),
  plot.title = element_text(face = "bold", hjust=0.5))
```
![](mean%20abundance%20Rplot.png)

## 3. STUDY OF ENDANGERED (Dusky Grouper) AND NEAR THREATENED (Thicklip Grey Mullet) SPECIES DISTRIBUTION
```md
#adding sites coordinates
coordinates <- data.frame(
  site = c(
    "Praia da Pedreira",
    "Praia Ribeira das Tainhas",
    "Praia Baixa D'Areia",
    "Praia do Populo"),
  latitude = c(
    37.7154764,
    37.7159691,
    37.7165400,
    37.7484241),
  longitude = c(
    -25.4627892,
    -25.4096397,
    -25.5179964,
    -25.6157604))

#combining dataset and coordinates
fish <- merge(fish, coordinates, by = "site")
#check names
names(fish)
#transforming the dataset into a Spatvector
fish_vect <- vect(
  fish,
  geom = c("longitude.y", "latitude.x"),
  crs = "EPSG:4326")

#extracting the 4 sites
sites <- fish_vect[!duplicated(fish_vect$site), ]
#choosing 1st species
EN_sites <- unique(
  fish_vect$site[fish_vect$fish.species == "dusky grouper juvenile "])
#choosing 2nd species
NT_sites <- unique(
  fish_vect$site[fish_vect$fish.species == "thicklip grey mullet"])

#creating the occurrence 1st species
sites$Occurrence_EN <- 0
sites$Occurrence_EN[
  sites$site %in% EN_sites
] <- 1
#check
sites$Occurrence_EN

#creating the occurrence 2nd species
sites$Occurrence_NT <- 0
sites$Occurrence_NT[
  sites$site %in% NT_sites
] <- 1
#check
sites$Occurrence_NT

#data frame dusky grouper/thicklip grey mullet occurrence
EN_occurrence_df <- as.data.frame(sites$Occurrence_EN)
NT_occurrence_df <- as.data.frame(sites$Occurrence_NT)

#presence and absence duskygrouper
pres_EN <- sites[sites$Occurrence_EN == 1,]
abse_EN <- sites[sites$Occurrence_EN == 0,]

#presence and absence thicklip grey mullet
pres_NT <- sites[sites$Occurrence_NT == 1,]
abse_NT <- sites[sites$Occurrence_NT == 0,]
```

### Dusky Grouper (EN) presence-absence map
```md
map_EN <- mapview(
  sites[sites$Occurrence_EN == 1, c("site", "Occurrence_EN")],
  col.regions = "blue",
  cex = 8,
  layer.name = "Dusky grouper juvenile - Presence"
) +
  mapview(
    sites[sites$Occurrence_EN == 0, c("site", "Occurrence_EN") ],
    col.regions = "red",
    cex = 8,
    layer.name = "Dusky grouper juvenile - Absence"
)

map_EN
```

![](duskygrouperRplot.png)


### Thicklip Grey Mullet (NT) presence-absence map
```md
map_NT <- mapview(
  sites[sites$Occurrence_NT == 1, c("site", "Occurrence_NT")],
  col.regions = "green",
  cex = 8,
  layer.name = "Thicklip grey mullet - Presence"
) +
  mapview(
    sites[sites$Occurrence_NT == 0, c("site", "Occurrence_NT") ],
    col.regions = "orange",
    cex = 8,
    layer.name = "Thicklip grey mullet - Absence"
)

map_NT
```
![](thicklipgreymullet3Rplot.png)

## DCA on fish community among sites
```md
# exctract values from spatvector
fish_df <- values(fish_vect)


#  creating transect value
# site + date = transect
fish_df$transect <- paste(
  fish_df$site,
  fish_df$date,
  sep = "_"
)


# 2d matrix
# rows = transect
# columns = specie
# values = abbondanze
dca_matrix <- xtabs(
  abundances ~ transect + fish.species,
  data = fish_df
)


# turn NA into 0
dca_matrix[is.na(dca_matrix)] <- 0

# dimension check
dim(dca_matrix)

#  DCA
dca <- decorana(dca_matrix)

#  eigenvalues: they explain how important each DCA axis is when representing the differences in fish composition among sites
dcal1 <- dca$evals[1]
dcal2 <- dca$evals[2]
dcal3 <- dca$evals[3]
dcal4 <- dca$evals[4]

total <- sum(c(dcal1, dcal2, dcal3, dcal4))

#  DCA1 and DCA2 %
percdca1 <- dcal1 * 100 / total
percdca2 <- dcal2 * 100 / total

#check
percdca1
percdca2

# % DCA1 + DCA2
percdca1 + percdca2

#to see where the transects are in the graph
transect_scores <- scores(
  dca,
  display = "sites")

transect_scores
```
### Plot
```md

#matching the transect to the site
transect_names <- unique(
  fish_df[, c("transect", "site")]
)

# Put transects in the same order as in the DCA
transect_names <- transect_names[
  match(rownames(transect_scores),transect_names$transect),]

#matching color to site
point_colors <- rep("black",nrow(transect_scores))

point_colors[transect_names$site %in% "Praia Baixa D'Areia"] <- "blue"

point_colors[transect_names$site %in% "Praia da Pedreira"] <- "red"

point_colors[transect_names$site %in% "Praia do Populo"] <- "green"

point_colors[transect_names$site %in% "Praia Ribeira das Tainhas"] <- "orange"

# check that there are 5 transects for each site
table(point_colors)

#set axis
xlim <- max(abs(transect_scores[, "DCA1"])) * 1.1

ylim <- max(abs(transect_scores[, "DCA2"])) * 1.1

#set graph and legend area
layout(matrix(c(1, 2),nrow = 2),heights = c(4, 2))

#DCA graph
par(mar = c(5, 4, 2, 2))

plot(
  transect_scores[, "DCA1"],
  transect_scores[, "DCA2"],
  type = "n",
  xlim = c(-xlim, xlim),
  ylim = c(-ylim, ylim),
  xlab = "DCA1",
  ylab = "DCA2"
)
# Lines crossing at zero
abline(
  h = 0,
  v = 0,
  col = "grey80",
  lty = 2
)
# add transects as coloured points
points(
  transect_scores[, "DCA1"],
  transect_scores[, "DCA2"],
  pch = 19,
  col = point_colors,
  cex = 1.2
)

#legend
par(
  mar = c(0, 0, 0, 0)
)

plot.new()

legend(
  "center",
  legend = c(
    "Praia Baixa D'Areia",
    "Praia da Pedreira",
    "Praia do Populo",
    "Praia Ribeira das Tainhas"
  ),
  col = c(
    "blue",
    "red",
    "green",
    "orange"
  ),
  pch = 19,
  bty = "n",
  cex = 0.9
)
```

