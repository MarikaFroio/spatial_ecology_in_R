## language and working directory setup
```md
Sys.setlocale("LC_TIME", "C")
# Sys.setlocale() means set  local system
# LC_TIME specifies that I want to change the settings related to date and time
# C sets the local standard to english

setwd("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project")
```
## packages upload
```md
library("tidyverse")
# A collection of R packages used for data manipulation, cleaning, visualization, and analysis. It includes packages such as dplyr, ggplot2, tidyr, and readr

library("lubridate")
# Used to work with dates and times. It makes it easier to convert, extract, compare, and manipulate dates

library("vegan")
# Mainly used for ecological and community ecology analyses, such as species diversity, species abundance, ordination, and multivariate analysis

library(dplyr)
#Used for data manipulation. It provides functions such as filter(), select(), group_by(), summarise(), and arrange(). not necessary because I already have tidyverse

library(ggplot2)
#Used to create data visualizations and graphs, based on the Grammar of Graphics. not necessary because I already have tidyverse

library(mapview)
# Used to create interactive maps, especially for visualizing spatial data and geographic objects

library(overlap)
# Used to estimate and compare probability density distributions, particularly useful for measuring overlap between activity or temporal distributions

library(igraph)
#Used to create and analyze networks and graphs. In this project, it represents species as nodes and interactions between species as edges

library(terra)
Used for spatial data analysis, particularly for working with raster and vector geographic data such as maps, coordinates, and spatial objects

# NOT USED
library(knitr)
# knitr is an R package used to combine R code, results, tables, and figures into dynamic reports and documents, such as R Markdown
# NOT USED
```
## file upload
```md
fish <- read.csv2("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project/sao miguel fish species.csv", fileEncoding = "Windows-1252")
# read.csv2() is used to read excel data but in my case it is probably better to use read.cvs(, sep =";") because the decimals are expressed with "." even though the columns are separated with ";"
# fileEncoding tells R how to read the data. in this case we have to put windows-1252 because on the document we see ANSI
```
## data check
```md
glimpse(fish)
```
## data correction
```md
fish[fish == ""] <- NA
# [] selects elements inside fish dataframe and we say that where fish value is an empty string it should be a missing value.

fish<- fish|> fill(site, date, visibility.m, tide, water.T..C, wave.height.m, wave.period.s, wave.power.kW.m, wind.km.h, level, time)
# we update the fish dataframe using the fill() function from the tidyr package. The fill() function replaces missing values (NA) with the previous available value in the column.

fish$date<-dmy(fish$date)
# dmy() lubridate function which transforms a date into an R object (day-month-year)

fish$abundances<-as.numeric(fish$abundances)
fish$size.cm<-as.numeric(fish$size.cm)
fish$visibility.m<-as.numeric(fish$visibility.m)
fish$water.T..C<-as.numeric(fish$water.T..C)
fish$wave.height.m<-as.numeric(fish$wave.height.m)
fish$wave.power.kW.m<-as.numeric(fish$wave.power.kW.m)
fish$wave.period.s<-as.numeric(fish$wave.period.s)
fish$wind.km.h<-as.numeric(fish$wind.km.h)
# as.numeric basic function of R that transforms values into numbers

# in this project specifically
tidyverse / dplyr  → data manipulation
lubridate          → dates
overlap            → temporal/density overlap
igraph             → species interaction network
terra              → spatial data
mapview            → interactive maps
vegan              → ecological analysis
ggplot2             → graphs and visualization

fish$time <- hm(fish$time)
# hm() lubridate package function that converts time in a format that R understands
```
### 1. STUDY OF SPECIES RICHNESS
```md
sc_richness <- fish |>
  summarise(species_richness = n_distinct(fish.species))
#I could also just write
sc_richness <- summarise(fish, species_richness = n_distinct(fish.species))

# this code tells R to summarise the dataset fish, detect all the different values in the column fish.species and create a column called species_richness and then put everything inside an object called sc_richness. all dplyr functions. we use summarise because we want just 1 value

sc_richness

## table with species names
names_sc_richness <- fish |>
  distinct(fish.species) |>
  arrange(fish.species)

# distinc() is to distinguish the different species based on the name and arrange() puts them in alphabetical order. they're all dplyr functions

#or I could also write
names_sc_richness <- arrange(
  distinct(fish, fish.species),
  fish.species
)

names_sc_richness

knitr::kable(names_sc_richness)
# kable() is a function from the knitr package used to format data as a clean, readable table
# :: is used to access a function or object from a specific package without loading the whole package with library()
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

### 2. STUDY OF MEAN ABUNDANCE OF EACH SPECIES
```md
mean_abundance_overall <- fish |>
  group_by(fish.species) |>
  summarise(mean_abundance = mean(abundances, na.rm = TRUE),
    .groups = "drop") |>
  arrange(desc(mean_abundance))

# group_by() is a dplyr function that creates a groups the data based on the species, then summarise() is a dplyr function that creates a result containing mean_abundance which is the mean value of all abundances for each species and then arrange() (dplyr function) orders the result from species with highest to lowest abundance. (MAYBE NA.RM = TRUE COULD BE REMOVED)

mean_abundance_overall

knitr::kable(mean_abundance_overall)
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

## Mean abundance heatmap
```md
ggplot(mean_abundance_overall,
    aes(x = "abundance",y = reorder(fish.species, mean_abundance),
# y=rorder... is a basic R function that puts species on the y axis and orders them based on their mean abundance
    fill = mean_abundance))
# fill means that the color of the tile depends on the mean abundance (and creates the gradient on the side)
 +
geom_tile(color = "white")
# geom_tile creates rectangular tiles with a white line between them COULD AVOID IT
+
# geom_text writes the text on the graph. aes is aesthetic mappings and controls which variables are used to create the graph. color and size are fixed settings so they are not part of aes
geom_text(
    aes(label = round(mean_abundance, 2)),
# round (mean...) is a basic R function that rounds to 2 decimals, could be label=mean_abundance
    color = "black",
    size = 3.5) +
scale_fill_gradient(low = "yellow", high = "orange")
# scale_fill-gradient is a ggplot2 function that assigns a color to low and high values +
labs(
  x = NULL,
  y = "Species",
  fill = "Mean abundance gradient",
  title = "Mean abundance of fish species")
# labs is a ggplot2 function used for setting labels and heads of graphs. x:null means no label for x axis, fill: is the head of legend for color gradient
+
theme_minimal()
# minimalist style
+
theme(
  axis.text.x = element_text(face = "bold"),
  axis.text.y = element_text(size = 9),
  plot.title = element_text(face = "bold", hjust=0.5))
# to adjust the aesthetic of the graph, like bold letters, size and centered text--> hjust=0.5
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
# all basic R functions. we create a data frame so we can have an organized table with associations already made and it will be easier to merge it with the fish data frame. c() creates a vector of values. latitude and longitude values are taken from google earth. 

#combining dataset and coordinates
fish <- merge(fish, coordinates, by = "site")
# merge() is a basic R function that puts together the 2 dataframes fish and coordinates using the common column "site", meaning that now in fish dataframe we will also have the coordinates associated to the sites.

#check names
names(fish)
# basic R function used to check the names of the column because now the coordinates could have a different name. .x and .y doesn't mean latitude or longitude, it's just a random letter used to change the name.

#transforming the dataset into a Spatvector
fish_vect <- vect(
  fish,
  geom = c("longitude.y", "latitude.x"),
  crs = "EPSG:4326")
# vect() is a terra function that transforms the dataframe fish into a SpatVector, which is still a dataframe but with spatial characteristics.
# geom() is an argument of vect (to check I could do args(vect)), it stands for geometry and is used to tell vect() which columns to use as coordinates
# crs is another vect() argument which specifies the geographic reference system. it corresponds to WGS 84 coordinates (World Geodetic System 1984), used to express a location on earth in latitude and longitude. I know that i have to use "EPSG:4326" because it is the code associated for WGS 84 and google earth uses this kind.

#extracting the 4 sites
sites <- fish_vect[!duplicated(fish_vect$site), ]
# the syntax [] is a basic R object that allows to select certains values.
# duplicated() is also a basic R function that finds duplicated values. by putting ! in front which means not we are choosing only non duplicated values
#I DIDN'T USE UNIQUE() BECAUSE THIS FUNCTION GIVES BACK DIRECTLY THE VALUES WITHOUT DUPLICATES, SO JUST THE SINGLE SITE NAMES. DUPLICATED() INSTEAD WORKS BETTER WITH ROWS AND ALLOWS ME TO KEEP ALL THE INFO ASSOCIATED TO THE ROW.
# $site selects the column site
# the ored is [rows, columns] so we choose which rows we want but we keep columns "empty" so that we keep all columns associated to those rows (like the coordinates)

#choosing 1st species
EN_sites <- unique(
  fish_vect$site[fish_vect$fish.species == "dusky grouper juvenile "])
# i am creating just a vector with the names of the sites where I recored the species so I can use the basic function of R unique() which gives me the different names jsut 1 time
# inside unique I put only the column site of the spatvector fish_vect
# and I select only the data related to the species I want in the column fish.species of the spatvector fish_vect

#if I wanted to create directly a spatial object I could have used:
EN_sites <- fish_vect[
  fish_vect$fish.species == "dusky grouper juvenile " &
  !duplicated(fish_vect$site),
]

sites$Occurrence_EN <- 0
sites$Occurrence_EN[sites$site %in% EN_sites$site] <- 1

#choosing 2nd species
NT_sites <- unique(
  fish_vect$site[fish_vect$fish.species == "thicklip grey mullet"])

#creating the occurrence 1st species
sites$Occurrence_EN <- 0
sites$Occurrence_EN[
  sites$site %in% EN_sites
] <- 1
# I am assigning a 0 or 1 value to certain elements of a column
# I am creating the column Occurrence_EN and first everything has value 0
# then in that column I select all the objects associates to the column sites and if they appear in the vector EN_sites as well then the value is 1. %in% is an R operator that tells if a value is part of a group of values

#check
sites$Occurrence_EN

#creating the occurrence 2nd species
sites$Occurrence_NT <- 0
sites$Occurrence_NT[
  sites$site %in% NT_sites
] <- 1
#check
sites$Occurrence_NT

# NOT NECESSARY
#data frame dusky grouper/thicklip grey mullet occurrence
EN_occurrence_df <- as.data.frame(sites$Occurrence_EN)
NT_occurrence_df <- as.data.frame(sites$Occurrence_NT)
# NOT NECESSARY

#presence and absence duskygrouper
pres_EN <- sites[sites$Occurrence_EN == 1,]
abse_EN <- sites[sites$Occurrence_EN == 0,]

# selecting only values 1 for presence and values 0 for absence in sites spatvector

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
# mapview() is a function of the mapview package that represents an object on a map.
# we are representing sites: the rows are the presences and absences and the columns are sites and occurrence
# col.regions is a mapview argument that assigns a color to certain point or regions on the map
# layer.name is a mapview argument that gives a name to the layer of the map

# OPPURE
mapview(
  pres_EN[, c("site", "Occurrence_EN")],
  col.regions = "blue",
  cex = 8,
  layer.name = "Dusky grouper juvenile - Presence"
)+
mapview(
  abse_EN[, c("site", "Occurrence_EN")],
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
# exctract values from spatvector
fish_df <- values(fish_vect)
# to create a DCA is better to work with a dataframe and not a spatvector because it doesn't use spatial data. so we extract the values from fish_vect and create fish_df
# values() is a terra package function

#creating transect value
#site + date = transect
fish_df$transect <- paste(
  fish_df$site,
  fish_df$date,
  sep = "_"
)
# creating the column transect
# we use paste() (basic R function) to create a singular object with both information about site and date. If I had used c() it would have created a vector with separated info.
# I could avoid using sep = "_"


# 2d matrix
#rows = transect
#columns = species
#values = abundances

dca_matrix <- xtabs(
  abundances ~ transect + fish.species,
  data = fish_df
)
# I am creating the matrix for the DCA using the R base function xtabs(). The matrix is structured with transects as rows, fish species as columns, and their corresponding abundances as values. All the information is extracted from fish_df.

# turn NA into 0
dca_matrix[is.na(dca_matrix)] <- 0
# in some cases there might not be a value recorded for a combination of transect and species so we are using is.na() R basic function to find those missing values and turn them into 0

# dimension check
dim(dca_matrix)
# dim() basic R function that tells the number of rows and columns for the dataframe

#  DCA
dca <- decorana(dca_matrix)
# decorana() is a vegan package function that executes the detrended correspondence analysis on the matrix that I created

#  eigenvalues: they explain how important each DCA axis is when representing the differences in fish composition among sites
dcal1 <- dca$evals[1]
dcal2 <- dca$evals[2]
dcal3 <- dca$evals[3]
dcal4 <- dca$evals[4]

total <- sum(c(dcal1, dcal2, dcal3, dcal4))
# I am extracting the eingenvalues for 4 axis and then I consider the sum as 100%

# to check how many values I have I could do:
length(dca$evals)
# and if I want to use them all in my calculation:
total <- sum(dca$evals)

#  DCA1 and DCA2 %
percdca1 <- dcal1 * 100 / total
percdca2 <- dcal2 * 100 / total
# to know what percentage of the total value is represented by dca1 or dca2. it might be possible that the first 2 are the most representative and important, they might show the highest difference between the data

#check
percdca1
percdca2

# % DCA1 + DCA2
percdca1 + percdca2
# it is the sum of the 2 percentages. I could also name it as sum_percdca, if I don't give it a name R doesn't save it

#to see where the transects are in the graph
transect_scores <- scores(
  dca,
  display = "sites")
# scores() is a fucntion of the vegan package that extracts the coordinates of my dca
# display = sites doesn't refer to my sites but is a vegan argument that displays the coordinats of my transects
# so in transect_scores I am saving the dca coordinates (on the graph) of my transects

transect_scores
```
### Plot
```md

#matching the transect to the site
transect_names <- unique(
  fish_df[, c("transect", "site")]
)
# I could also write
transect_names<- fish_df |>
  distinct(transect, site)

# in both cases I'm taking only different objects 1 time and extracting from fish_df the columns transect and site. i want to know which site each transect belongs to

# Put transects in the same order as in the DCA
transect_names <- transect_names[
  match(rownames(transect_scores),transect_names$transect),]
# rownames(transect_scores) has the transect names in the order they show up in the dca
# match() looks for the names in transect_names$transect column and correlates them to transect_scores

#matching color to site
point_colors <- rep("black",nrow(transect_scores))
# first we create a vector with all points colored in black
# nrow() counts how many transects we have
# rep(black) creates a vector with as many black as the transects

point_colors[transect_names$site %in% "Praia Baixa D'Areia"] <- "blue"
# then we change the color based on the name of the site (point_colors and transect_names were not correlated before)

point_colors[transect_names$site %in% "Praia da Pedreira"] <- "red"

point_colors[transect_names$site %in% "Praia do Populo"] <- "green"

point_colors[transect_names$site %in% "Praia Ribeira das Tainhas"] <- "orange"

# check that there are 5 transects for each site
table(point_colors)
# function that counts how many times a value is repeated

#set axis
xlim <- max(abs(transect_scores[, "DCA1"])) * 1.1

ylim <- max(abs(transect_scores[, "DCA2"])) * 1.1
# from the table of transect_scores, we are taking the values of the columns DCA1 and DCA2
# we transform the values in all positive with abs() because then we will find the farthest value from 0 with max()
# *1.1 is to implement the value of 10% so we have more space in the graph
# Take all the DCA coordinates of the transects → find the one farthest from 0 → increase that distance by 10% → use the result to set the axis limits.

#set graph and legend area
layout(matrix(c(1, 2),nrow = 2),heights = c(4, 2))
# layout() creates different areas in the plot area. basic R function
# matrix (c(1,2), nrow=2) creates a matrix with 2 rows corresponding to the 2 areas, the first one for the graph and the second for the legend
# heights=c(4,2) sets the height of the area, the first one is higher than the second

#DCA graph
par(mar = c(5, 4, 2, 2))
# par() is a basic R function that modifies the graph settings
# mar = c() is the function used to set the margins of the graph in the order: bottom, left, top, right. so we're leaveing more space on the bottom and on the left

plot(
  transect_scores[, "DCA1"],
  transect_scores[, "DCA2"],
  type = "n",
  xlim = c(-xlim, xlim),
  ylim = c(-ylim, ylim),
  xlab = "DCA1",
  ylab = "DCA2"
)
# we're creating the structure of the graph without putting the points yet. type="n" means without points
# the axes are set with both negative and positive values
# then we add the lables to the axes

# Lines crossing at zero
abline(
  h = 0,
  v = 0,
  col = "grey80",
  lty = 2
)
# abline() is a basic R function that adds lines to the graph: h is the orizontal and v the vertical
# lty= 2 sets the scattered line

# add transects as coloured points
points(
  transect_scores[, "DCA1"],
  transect_scores[, "DCA2"],
  pch = 19,
  col = point_colors,
  cex = 1.2
)
# pch = 19 selects a full point
# to check other symbols
plot(
  1:25,
  rep(1, 25),
  pch = 1:25,
  cex = 2
)

text(
  1:25,
  rep(0.8, 25),
  labels = 1:25
)
# or
?points


#legend
par(mar = c(0, 0, 0, 0))
# all margins are at 0

plot.new()
# creates an empty working space in the second area

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
  cex = 0.9 (character expansion)
)
# center: is the position of the legend
# legend = c() adds the name of the sites
# col=c() adds the color of each site
# pch= 19 adds the symbol (plotting character)
# bty="n" erases the outline of the legend (box type)
```
![](dcaRplot.png)

## 5. ABUNDANCE CHANGE OVER 5 MONTHS OF THE 2 MOST ABUNDANT SPECIES
```md
#selection of species, dates and relative abundances - white seabream
white_seabream<-fish[fish$fish.species=="white seabream",]
# selection of all columns only related to rows with white seabream

white_seabream_date<-white_seabream$date
# selection of the column date inside the dataframe with only white seabream data

white_seabream_abundance<-white_seabream$abundances
# selection of the column abundances inside the dataframe with only white seabream data

#selection of dates, species and relative abundances - salema
salema<-fish[fish$fish.species=="salema",]
salema_date<-salema$date
salema_abundance<-salema$abundances
```
### plots of abundance change over 5 month in white seabream and salema
```md
par(mfrow = c(1, 2),mar = c(4, 4, 3, 1))
# par () to set the graph
# mfrow() is the funcion multi-frame by rows and is used to create a graph with 1 row and 2 columns which means to put 2 graphs next to each other.

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
#as.numeric is an R function that transforms a value into a number

#kernel density
wsb_linear_kd<-density(white_seabream_date_numeric)
# density() is used to estimate the probability distribution of a numeric variable. In practice, it creates an estimate of the density the data, which can then be represented graphically.

#Salema
salema_date_numeric<-as.numeric(salema_date)
#kernel density
s_linear_kd<-density(salema_date_numeric)

# comparison plot
par(mfrow=c(1,1))
# (1,1) means that I want just 1 graph

plot(wsb_linear_kd,
      col = "blue",
      xaxt = "n",
      xlab = "date",
      ylab = "density",
      main = "white seabream- salema kernel density overlap")
 # xaxt="n" means that I don't want the x axis to be automatically represented

lines(s_linear_kd, col = "red")
# lines() add a line on the already existing graph
 
axis(1, at = pretty(c(wsb_linear_kd$x, s_linear_kd$x)), labels = as.Date(pretty(c(wsb_linear_kd$x, s_linear_kd$x)), origin = "1970-01-01"))
# axis(1) means inferior axis
# pretty() is used to select the values where to put the lines on the graph
# c(...$x, ...$x) means that i want the x values of both densities
# as.Date(....) we basically take the same values from the densities we are representing but they have to be written in the format origin = .... because we converted them in numbers previously

```
![](kerneldensity2Rplot.png)
# Loop to check other species detection frequency
```md
# NOT NECESSARY
fish_date_numeric <- as.numeric(fish$date)
# NOT NECESSARY

species_list <- unique(fish$fish.species)
# we transform all dates into numebers and select the species only once

#plot
par(mfrow = c(3, 3))
for (species in species_list)
# this means for each species contained in species_list do the following things
{species_data <- fish[
    fish$fish.species == species,]
# create the object species_data which has all the info related to the species of the dataframe, one after the other

if (nrow(species_data) >= 2)
# this means only if I have at least 2 observations for species_data

{ species_data$date_numeric <- as.numeric(
      species_data$date)
# transforming the species_data date in a numeric value

    linear_kd <- density(species_data$date_numeric)
# linear density for each species data numeric date
    
    plot(linear_kd,
         main = paste("Kernel density of date for", species),
# paste() puts together different text elements, we are putting the same title to each graph with onlt the name of the species changing

         xlab = "Date",
         ylab = "density",
         xaxt = "n",
         cex.main = 0.7)
    
    axis(1,
         at = pretty(species_data$date_numeric),
         labels = as.Date(pretty(species_data$date_numeric),
                          origin = "1970-01-01"), cex.axis = 0.7)}}
# Return to one plot at a time
par(mfrow = c(1, 1))
```
### linear kernel density plots other speacies recorded at least twice
![](densityall1Rplot.png)
![](densityall2Rplot.png)

## 7. COMMUNITY GRAPH TO STUDY POTENTIAL SPATIAL/RESOURCE OVERLAP DEPENDING ON FISH SIZE AND ABUNDANCE
```md
#Calculating mean size and mean abundance for each species
species_size <- fish %>% # i can also use |>
  select(fish.species, size.cm, abundances) %>%
# I'm selecting only the columns I'm interested in
  group_by(fish.species) %>%
# I'm dividing the data into groups based on the species
  summarise(
    mean_size = mean(size.cm, na.rm = TRUE),
    mean_abundance = mean(abundances, na.rm = TRUE))
# summarise() sums up the data creating 1 line per group generally where we put the mean size and abundance
#  na.rm = TRUE to ignore missing values

#Creating big and small species categories based on mean size 
big_species <- species_size %>% # I could use |>
# we start from the species_size object (which also contains species abundances)
  filter(mean_size > 15) %>%
# filter() selects certain rows. in this case the rows with size bigger than 15 cm
  pull(fish.species)
# pull() extract a single column from a table and transforms it into a vector. in this case it is the species column

small_species <- species_size %>%
  filter(mean_size <= 15) %>%
  pull(fish.species)

 #Creating interactions between big and small species 
 interactions <- expand.grid(
  big_species = big_species,
  small_species = small_species)
# expand.grid() creates all possible combinations between the given objects. in this case we want all possible combinations between big and small species. we are creating 2 columns with the same name as the object we created and selected

#Adding the mean size of the species
interactions_big_size <- species_size$mean_size[match(interactions$big_species, species_size$fish.species)]
# we take the column mean_size from species_size
# match() for each species of the column big_species from the interactions object, select the same species in species_size (and give me its mean size)

interactions_small_size <- species_size$mean_size[match(interactions$small_species, species_size$fish.species)]

#Calculating size difference
interactions_size_difference <- abs(interactions_big_size - interactions_small_size)
# abs() means absolute value and erases the negative value (might not be necessary and i can just write the difference without () )
# this code calculates the difference of mean size between big and small species for each interaction

#Potential spatial/resource overlap
#Smaller size difference = higher potential overlap
interactions$overlap <- 1 / (1 + interactions_size_difference)
# I'm creating a new column called overlap inside interactions
# we add 1 to avoid a division with 0 in case there is no difference
# 1/ creates a value that decreases with the increase of size difference
# so we have less overlap for species with high size difference

# graph all size + all interactions
interactions_graph <- graph_from_data_frame(
  interactions[
    ,
    c(
      "big_species",
      "small_species",
      "overlap"
    )
  ],
  vertices = species_size$fish.species,
  directed = FALSE)
# graph_from_data_frame() is a function from the igraph package that creates a graph starting from a dataframe. the graphs usually has vertices (nodes) and edges (connections)
# I'm using the interactions dataframe and I'm selecting the columns big_specie, small_species and overlap
# the vertices are the species names from species_size vector
# directed=FALSE means that there is no specific direction in the interaction, it goes both ways

#nodes size
V(interactions_graph)$size <- 8
# V() is an igraph function that refers to nodes

#edge thickness base on overlap
E(interactions_graph)$width <-
  E(interactions_graph)$overlap * 8
# E() is an igraph function that refers to edges
# we are selecting the width of the edge representing the interaction and saying that it should be the value of the overlap associated to that interaction multiplied by 8

#selection of strongest interactions
strong_interactions <- interactions %>%
  arrange(desc(overlap)) %>%
  slice_head(n = 10)
# to select the 10 strongest interactions
# arrange() orders the rows of the dataframe
# desc() means in decreasing order
# so  arrange(desc(overlap)) orders interactions from the one with the highest overlap to the one with the lowest
# slice_head(10) selects the first 10 rows of the dataframe

#species involved in strongest interactions
label_species <- unique(
  c(strong_interactions$big_species,
    strong_interactions$small_species))
# we take the big and small species from the strong interactions and eliminate the duplicates with the function unique ()

#show labels
V(interactions_graph)$label <- ifelse(
  V(interactions_graph)$name %in% label_species,
  V(interactions_graph)$name, "")
# ifelse() puts a condition. if the name of the species is in label_species then show the name, otherwise keep it empty ""
# we created the $label attribute/column

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
  main = "Potential spatial/resource overlap among fish species")
```
### community interactions based on fish size plot
![](interactionsRplot.png)

