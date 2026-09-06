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
- **Species distribution**, with a focus on species classified as *Endangered* or *Near Threatened* according to the IUCN Red List
- **Temporal variation in species abundance** in relation to sampling date over a five-month period
- **Detection patterns**, using kernel density estimation to identify periods with the highest frequency of fish detections
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
# used for data manipulation. it includes "dplyr", also used for data manipulation, and "ggplot2" used for graphs and visualization
library("lubridate")
# used to set dates
library("vegan")
# used for ecological analysis
library(mapview)
# used to create interactive maps
library(overlap)
# used to analyse temporal and density overlap
library(igraph)
# used to analyse species interaction network
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
    mean_abundance = mean(abundances),
    .groups = "drop"
  ) |>
  arrange(desc(mean_abundance))
mean_abundance_overall
```
|fish.species           | mean_abundance|
|:----------------------|--------------:|
|salema                 |      38.85|
|white seabream         |      34.21|
|bogue                  |      20|
|two-banded seabream    |       6.11|
|rainbow wrasse         |       3|
|thicklip grey mullet   |       3|
|parrotfish             |       2.71|
|dusky grouper juvenile |       2|
|saddled seabream       |       2|
|striped red mullet     |       1.75|
|ornate wrasse          |       1.33|
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
  geom = c("longitude", "latitude"),
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

## 4. DCA on fish community among sites
```md
#  creating transect value
# site + date = transect
fish$transect <- paste(
  fish$site,
  fish$date,
  sep = "_"
)


# 2d matrix
# rows = transect
# columns = species
# values = abundances
dca_matrix <- xtabs(
  abundances ~ transect + fish.species,
  data = fish
)


# turn NA into 0
dca_matrix[is.na(dca_matrix)] <- 0

# dimension check
dim(dca_matrix)

#  DCA
dca <- decorana(dca_matrix)

#  eigenvalues: they explain how important each DCA axis is when representing the differences in fish composition among sites
# to check how many values I have:
length(dca$evals)

dcal1 <- dca$evals[1]
dcal2 <- dca$evals[2]
dcal3 <- dca$evals[3]
dcal4 <- dca$evals[4]

total <- sum(c(dcal1, dcal2, dcal3, dcal4))

#  DCA 1-2-3-4 %
percdca1 <- dcal1 * 100 / total
percdca2 <- dcal2 * 100 / total
percdca3 <- dcal3 * 100 / total
percdca4 <- dcal4 * 100 / total

#check
percdca1
percdca2
percdca3
percdca4

# DCA1 & DCA2 have the highest %

#to see where the transects are in the graph
transect_scores <- scores(
  dca,
  display = "sites")

transect_scores
```
### Plot
```md

#matching the transect to the site
transect_names <- fish |>
  distinct(transect, site)

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
  col = "grey",
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
![](dcaRplot.png)

## 5. ABUNDANCE CHANGE OVER 5 MONTHS OF THE 2 MOST ABUNDANT SPECIES
```md
#selection of species, dates and relative abundances - white seabream
white_seabream<-fish[fish$fish.species=="white seabream",]
white_seabream_date<-white_seabream$date
white_seabream_abundance<-white_seabream$abundances

#selection of dates, species and relative abundances - salema
salema<-fish[fish$fish.species=="salema",]
salema_date<-salema$date
salema_abundance<-salema$abundances
```
### plots of abundance change over 5 month in white seabream and salema
```md
par(mfrow = c(1, 2),mar = c(4, 4, 3, 1))

plot(white_seabream_date, white_seabream_abundance, xlab = "time", ylab = "abundance", main = "White Seabream - abundance change with date", cex.main = 0.7, cex.lab = 0.8)

plot(salema_date, salema_abundance, xlab = "date", ylab = "abundance", main = "Salema - abundance change with date", cex.main = 0.7, cex.lab = 0.8)

#to go back to singular plots
par(mfrow = c(1, 1))
```
![](abundancedateRplot.png)
## 6. DETECTION FREQUENCY ANALYSIS 
```md
# White Seabream
#transform date into numeric value
white_seabream_date_numeric<-as.numeric(white_seabream_date)
#kernel density
wsb_linear_kd<-density(white_seabream_date_numeric)

#Salema
salema_date_numeric<-as.numeric(salema_date)
#kernel density
s_linear_kd<-density(salema_date_numeric)

# comparison plot
par(mfrow=c(1,1))

plot(wsb_linear_kd,
      col = "blue",
      xaxt = "n",
      xlab = "date",
      ylab = "density",
      main = "white seabream- salema kernel density overlap")
 
lines(s_linear_kd, col = "red")
 
axis(1, at = pretty(c(wsb_linear_kd$x, s_linear_kd$x)), labels = as.Date(pretty(c(wsb_linear_kd$x, s_linear_kd$x)), origin = "1970-01-01"))
```
![](kerneldensity2Rplot.png)
# Loop to check other species detection frequency
```md
species_list <- unique(fish$fish.species)

#plot
par(mfrow = c(3, 3))
for (species in species_list) {species_data <- fish[
    fish$fish.species == species,]
if (nrow(species_data) >= 2) { species_data_numeric <- as.numeric(
      species_data$date)
    linear_kd <- density(species_data_numeric)
    
    plot(linear_kd,
         main = paste("Kernel density of date for", species),
         xlab = "Date",
         ylab = "density",
         xaxt = "n",
         cex.main = 0.7)
    
    axis(1,
         at = pretty(species_data_numeric),
         labels = as.Date(pretty(species_data_numeric),
                          origin = "1970-01-01"), cex.axis = 0.7)}}
# Return to one plot at a time
par(mfrow = c(1, 1))
```
### linear kernel density plots other species recorded at least twice
![](densityall1Rplot.png)
![](densityall2Rplot.png)

## 7. COMMUNITY GRAPH TO STUDY POTENTIAL SPATIAL/RESOURCE OVERLAP DEPENDING ON FISH SIZE AND ABUNDANCE
```md
#Calculating mean size and mean abundance for each species
species_size <- fish %>%
  select(fish.species, size.cm) |>
  group_by(fish.species) |>
  summarise(
    mean_size = mean(size.cm, na.rm = TRUE))

#Creating big and small species categories based on mean size 
big_species <- species_size |>
  filter(mean_size > 15) |>
  pull(fish.species)

small_species <- species_size |>
  filter(mean_size <= 15) |>
  pull(fish.species)

 #Creating interactions between big and small species 
 interactions <- expand.grid(
  big_species = big_species,
  small_species = small_species)

#Adding the mean size of the species
interactions_big_size <- species_size$mean_size[match(interactions$big_species, species_size$fish.species)]

interactions_small_size <- species_size$mean_size[match(interactions$small_species, species_size$fish.species)]

#Calculating size difference
interactions_size_difference <- abs(interactions_big_size - interactions_small_size)

#Potential spatial/resource overlap
#Smaller size difference = higher potential overlap
interactions$overlap <- 1 / (1 + interactions_size_difference)

# graph all size + all interactions
interactions_graph <- graph_from_data_frame(interactions[,c("big_species","small_species","overlap")],
  vertices = species_size$fish.species,
  directed = FALSE)

#nodes size
V(interactions_graph)$size <- 8

#edge thickness base on overlap
E(interactions_graph)$width <-
  E(interactions_graph)$overlap * 8

#selection of strongest interactions
strong_interactions <- interactions |>
  arrange(desc(overlap)) |>
  slice_head(n = 10)

knitr::kable(strong_interactions)
```
|big_species      |small_species     |   overlap|
|:----------------|:-----------------|---------:|
|axillary wrasse  |portuguese blenny | 0.4444444|
|axillary wrasse  |white seabream    | 0.4198895|
|salema           |portuguese blenny | 0.3939394|
|salema           |white seabream    | 0.3745262|
|saddled seabream |portuguese blenny | 0.2857143|
|saddled seabream |white seabream    | 0.2753623|
|axillary wrasse  |madeira rockfish  | 0.1739130|
|garfish          |portuguese blenny | 0.1666667|
|salema           |madeira rockfish  | 0.1656051|
|garfish          |white seabream    | 0.1630901|

```md
#species involved in strongest interactions
label_species <- unique(c(strong_interactions$big_species,strong_interactions$small_species))

#show labels
V(interactions_graph)$label <- ifelse(
  V(interactions_graph)$name %in% label_species, V(interactions_graph)$name,"")

# plot
plot(
  interactions_graph,
  vertex.color = "lightblue",
  vertex.size = 8,
  vertex.label = V(interactions_graph)$label,
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  edge.color = "grey",
  edge.width = E(interactions_graph)$width,
  main = "Potential spatial/resource overlap among fish species"
)
```
### community interactions based on fish size plot
![](interactionsRplot.png)
