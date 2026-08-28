> #### Marika Froio
>> ##### student n.






#to set in english
Sys.setlocale("LC_TIME", "C")
#to set directory
setwd("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project")
#upload file
fish <- read.csv2("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project/sao miguel fish species.csv", fileEncoding = "Windows-1252")
install.packages(c("tidyverse", "lubridate", "vegan"))
install.packages(c("dplyr", "ggplot2"))
install.packages("mapview")
install.packages("overlap")
install.packages("igraph")
install.packages("terra")
#tidyverse: to visualize, clean and change data
#lubridate: to work with date and time
#vegan: to make ecological analyses 
#mapview to create a map
library("tidyverse")
library("lubridate")
library("vegan")
library(dplyr)
library(ggplot2)
library(mapview)
library(overlap)
library(igraph)
library(terra)
#check data nature
glimpse(fish)

#FIX DATASET
#count empty spaces as NA
fish[fish == ""] <- NA
# [] selects a part of the file. i want to change empty strings into null data
fish<- fish|> fill(site, date, visibility.m, tide, water.T..C, wave.height.m, wave.period.s, wave.power.kW.m, wind.km.h, level, time)
#to apply the function to the following elements
#transform data from chr
fish$date<-dmy(fish$date)
#dmy transforms a character date into a date recognized by R
fish$abundances<-as.numeric(fish$abundances)
#as.numeric transforms a character into a number
fish$size.cm<-as.numeric(fish$size.cm)
fish$visibility.m<-as.numeric(fish$visibility.m)
fish$water.T..C<-as.numeric(fish$water.T..C)
fish$wave.height.m<-as.numeric(fish$wave.height.m)
fish$wave.power.kW.m<-as.numeric(fish$wave.power.kW.m)
fish$wave.period.s<-as.numeric(fish$wave.period.s)
fish$wind.km.h<-as.numeric(fish$wind.km.h)
fish$time <- hm(fish$time)
#hm transforms time into time recognized by R
#check
glimpse(fish)

# OVERALL SPECIES RICHNESS
sc_richness <- fish |>
  summarise(species_richness = n_distinct(fish.species))
#n_distinct counts unique/different values, so how many different species we have. it gives a NUMBER
sc_richness

# table with species names (overall)
names_sc_richness <- fish |>
  distinct(fish.species) |>
  arrange(fish.species)
#distinct removes duplicates
#arrange orders species, usually in alphabetic order
names_sc_richness

# MEAN OVERALL ABUNDANCE
mean_abundance_overall <- fish |>
  group_by(fish.species) |>
  summarise(
    mean_abundance = mean(abundances, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_abundance)) 
#desc() arranges in descendant order the species, from most to least abundant

mean_abundance_overall

# ABUNDANCE TABLE OVERALL
abundance_table_overall <- mean_abundance_overall |>
  select(species = fish.species,mean_abundance)

abundance_table_overall


# HEATMAP OVERALL ABUNDANCE
ggplot(mean_abundance_overall,
  aes(x = "Southern coast",y = reorder(fish.species, mean_abundance), 
#reorder orders species on y axis based on mean abundance from most to least abundant
    fill = mean_abundance)) 
+geom_tile(color = "white")+
#geom_tile creates colored tiles in the heatmap
geom_text(
#geom_text writes a numeric value inside the tiles
    aes(label = round(mean_abundance, 2)),
    color = "black",
    size = 3.5
  ) +
  scale_fill_gradient(low = "yellow", high = "orange") +
  labs(
    x = NULL,
    y = "Species",
    fill = "Mean abundance",
    title = "Mean abundance of fish species along the southern coast") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold"))

#STUDY OF DUSKY ENDANGERED (DUSKY GROUPER) AND THREATENED (THICKLIP GREY MULLET) SPECIES POPULATION DISTRIBUTION
#adding sites coordinates
coordinates <- data.frame(
  site = c(
    "Praia da Pedreira",
    "Praia Ribeira das Tainhas",
    "Praia Baixa D'Areia",
    "Praia do Populo"
  ),
  latitude = c(
    37.7154764,
    37.7159691,
    37.7165400,
    37.7484241
  ),
  longitude = c(
    -25.4627892,
    -25.4096397,
    -25.5179964,
    -25.6157604
  )
)
#combining dataset and coordinates
fish <- merge(fish, coordinates, by = "site")
#transforming fish into a spatvector
library(terra)

fish_vect <- vect(
  fish,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)
#extracting the 4 sites
sites <- fish_vect[!duplicated(fish_vect$site), ]
#choosing 1st species
EN_sites <- unique(
  fish_vect$site[fish_vect$fish.species == "dusky grouper"])
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
EN_occurrence_df <- as.data.frame(sites$Occurrence_EN$Occurrence)
NT_occurrence_df <- as.data.frame(sites$Occurrence_NT$Occurrence)

#presence and absence duskygrouper
pres_EN <- sites[sites$Occurrence_EN == 1,]
abse_EN <- sites[sites$Occurrence_EN == 0,]

#presence and absence thicklip grey mullet
pres_NT <- sites[sites$Occurrence_NT == 1,]
abse_NT <- sites[sites$Occurrence_NT == 0,]

#normal plot dusky grouper/thicklip grey mullet
plot(pres_EN, 
     col = "blue",
     pch = 19,
     main = "dusky grouper/thicklip grey mullet presence - absence")
points(abse_EN, 
       col = "red",
       pch = 19)
points(pres_NT, 
       col = "green",
       pch = 19)
points(abse_NT, 
       col = "orange",
       pch = 19)
text(pres_EN, 
     labels = pres_EN$site,
     pos = 3,
     cex = 0.7)
text(abse_EN, 
     labels = abse_EN$site,
     pos = 3,
     cex = 0.7)
text(pres_NT, 
     labels = pres_NT$site,
     pos = 3,
     cex = 0.7)
text(abse_NT, 
     labels = abse_NT$site,
     pos = 3,
     cex = 0.7)
legend("bottomright",
       legend = c("EN - Presence",
                  "EN - Absence",
                  "NT - Presence",
                  "NT - Absence"),
       col = c("blue",
               "red",
               "green",
               "orange"),
       pch = 19)
#map
map_EN_NT<- mapview(
  pres_EN,
  col.regions = "blue",
  cex = 8,
  layer.name = "EN - Presence") +
  mapview(
    abse_EN,
    col.regions = "red",
    cex = 8,
    layer.name = "EN - Absence") +
  mapview(
    pres_NT,
    col.regions = "green",
    cex = 8,
    layer.name = "NT - Presence") +
  mapview(
    abse_NT,
    col.regions = "orange",
    cex = 8,
    layer.name = "NT - Absence")

map_EN_NT

#DCA TO STUDY OF FISH COMMUNITY AMONG SITES TO SEE IF THERE'S ANY DIFFERENCE
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


#to see where the species are in the graph
species_scores <- scores(
  dca,
  display = "species")

species_scores

#plot
plot(
  dca,
  display = c("sites", "species"),
  type = "text")

#STUDY OF OVERLAP BETWEEN THE 2 MOST ABUNDANT SPECIES TO SEE IF THEIR ABUNDANCES CHANGE WITH TIME DURING THE DAY OR OVER A 5 MONTHS PERIOD
#ABUNDANCE CHANGE WITH TIME
#To change time into decimal hours
fish$time_dh<-hour(fish$time)+(minute(fish$time)/60)
#to create circular time
fish$time_dh_circ<-fish$time_dh*2*pi/24
#to select the species
white_seabream<-fish[fish$fish.species=="white seabream",]
#to select the time during which white seabream was observed
white_seabream_time<-white_seabream$time_dh_circ
#to select the abundances of white seabream
white_seabream_abundance<-white_seabream$abundances
#density plot does not work because it would tell me only at what time I did the survey, so I will compare abundances and time in a graph
plot(white_seabream$time_dh, white_seabream_abundance, xlab="time of day", ylab="abundance", main="white seabream - abundance change with time")

#with date
white_seabream_date<-white_seabream$date
plot(white_seabream_date, white_seabream_abundance, xlab="date", ylab="abundance", main="white seabream - abundance change with date")

#KERNEL DENSITY TO CHECK WHEN THE SPECIES ARE RECORDED THE MOST
#CIRCULAR KERNEL DENSITY WHITE SEABREAM - TIME
densityPlot(white_seabream_time)

#LINEAR KERNEL DENSITY WHITE SEABREAM - DATE
white_seabream_date_numeric<-as.numeric(white_seabream_date)
wsb_linear_kd<-density(white_seabream_date_numeric)
plot(wsb_linear_kd,
     xlab = "date",
     ylab = "density",
     main = "white seabream - kernel density over time",
     xaxt = "n")

axis(1,
     at = pretty(white_seabream_date_numeric),
     labels = as.Date(pretty(white_seabream_date_numeric),
                      origin = "1970-01-01"))

#COMPARISON WITH SALEMA
#salema data
salema<-fish[fish$fish.species=="salema",]

#time
salema_time<-salema$time_dh_circ
salema_abundance<-salema$abundances
plot(salema$time_dh, salema_abundance, xlab="time", ylab="abundance", main="salema - abundance change with time")
#circular kernel density
densityPlot(salema_time)

#date
salema_date<-salema$date
plot(salema_date, salema_abundance, xlab = "date", ylab="abundance", main="salema - abundance change with date")
#linear kernel density
salema_date_numeric<-as.numeric(salema_date)
s_linear_kd<-density(salema_date_numeric)
plot(s_linear_kd,
      xlab = "date",
      ylab = "density",
      main = "salema - kernel density over time",
      xaxt = "n")

axis(1,
      at = pretty(salema_date_numeric),
      labels = as.Date(pretty(salema_date_numeric),
                       origin = "1970-01-01"))
#COMPARISON CIRCULAR TIME WHITE SEABREAM-SALEMA
overlapPlot(white_seabream_time, salema_time)

#COMPARISON LINEAR DATE WHITE SEABREAM-SALEMA
plot(wsb_linear_kd,
      col = "blue",
      xaxt = "n",
      xlab = "date",
      ylab = "density",
      main = "white seabream- salema kernel density overlap")
 
lines(s_linear_kd, col = "red")
 
axis(1,
      at = pretty(c(wsb_linear_kd$x, s_linear_kd$x)),
      labels = as.Date(pretty(c(wsb_linear_kd$x, s_linear_kd$x)),
                       origin = "1970-01-01")) 
#LOOP TO CHECK ALL OTHER SPECIES IN THE DATASET
#we will analyze the study of abundance change with date because it is the most representative
#LOOP DATE-ABUNDANCE ALL SPECIES
species_list <- unique(fish$fish.species)

par(mfrow = c(3, 3))

for (species in species_list) {
  
  species_data <- fish[fish$fish.species == species, ]
  
  if (nrow(species_data) >= 2) {
    
    plot(species_data$date,
         species_data$abundances,
         xlab = "Date",
         ylab = "Abundance",
         main = paste(species, "- abundance change with date"),
         xaxt = "n",
         cex.main = 0.8)
    
    axis(1,
         at = pretty(species_data$date),
         labels = as.Date(pretty(species_data$date),
                          origin = "1970-01-01"))}}

#LOOP LINEAR KERNEL DENSITY DATE to check during which time each species is recorded the most (higher recording frequency)
fish$date_numeric <- as.numeric(fish$date)
species_list <- unique(fish$fish.species)

par(mfrow = c(3, 3))

for (species in species_list) {
  
  species_data <- fish[fish$fish.species == species, ]
  
  if (nrow(species_data) >= 2) {
    
    linear_kd <- density(species_data$date_numeric)
    
    plot(linear_kd,
         main = paste("Kernel density of date for", species),
         xlab = "Date",
         xaxt = "n",
         cex.main = 0.8)
    
    axis(1,
         at = pretty(species_data$date_numeric),
         labels = as.Date(pretty(species_data$date_numeric),
                          origin = "1970-01-01"))}}

#COMMUNITY GRAPH TO STUDY INDIRECT INTERACTIONS AMONG FISH SPECIES DEPENDING ON THEIR SIZE (amount of space they require in the same habitat)
# Calculating mean size for each species
species_size <- fish %>%
  group_by(fish.species) %>%
  summarise(mean_size = mean(size.cm, na.rm = TRUE))

# Creatting big and small species categories based on mean size
big_species <- species_size %>%
  filter(mean_size > 15) %>%
  pull(fish.species)

small_species <- species_size %>%
  filter(mean_size <= 15) %>%
  pull(fish.species)

# Creating interactions between big and small species
interactions <- expand.grid(
  big_species = big_species,
  small_species = small_species
)

#directed graph
interactions_graph <- graph_from_data_frame(
  interactions,
  vertices = species_size$fish.species,
  directed = F
)

plot(interactions_graph)

#!!!!!!ALTERNATIVA!!!!!!!
#COMMUNITY GRAPH TO STUDY POTENTIAL SPATIAL/RESOURCE OVERLAP
#DEPENDING ON FISH SIZE AND ABUNDANCE

# Calculating mean size for each species
species_size <- fish %>%
  group_by(fish.species) %>%
  summarise(
    mean_size = mean(size.cm, na.rm = TRUE),
    mean_abundance = mean(abundances, na.rm = TRUE)
  )

# Creating big and small species categories based on mean size
big_species <- species_size %>%
  filter(mean_size > 15) %>%
  pull(fish.species)

small_species <- species_size %>%
  filter(mean_size <= 15) %>%
  pull(fish.species)

# Creating interactions between big and small species
interactions <- expand.grid(
  big_species = big_species,
  small_species = small_species
)

# Adding the mean size of the species
interactions$big_size <- species_size$mean_size[
  match(interactions$big_species, species_size$fish.species)
]

interactions$small_size <- species_size$mean_size[
  match(interactions$small_species, species_size$fish.species)
]

# Calculating size difference
interactions$size_difference <- abs(
  interactions$big_size - interactions$small_size
)

# Potential spatial/resource overlap
# Smaller size difference = higher potential overlap
interactions$overlap <- 1 / (1 + interactions$size_difference)

# Directed graph
interactions_graph <- graph_from_data_frame(
  interactions[, c("big_species", "small_species", "overlap")],
  vertices = species_size$fish.species,
  directed = F
)

# Node size based on mean abundance
V(interactions_graph)$size <- species_size$mean_abundance[
  match(
    V(interactions_graph)$name,
    species_size$fish.species
  )
] * 3

# Edge thickness based on potential overlap
E(interactions_graph)$width <- E(interactions_graph)$overlap * 5

# Plot
plot(
  interactions_graph,
  vertex.color = "lightblue",
  edge.color = "grey",
  main = "Potential spatial/resource overlap among fish species"
)

