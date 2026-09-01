# to set directory
setwd("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project")
# upload file
fish <- read.csv2("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project/sao miguel fish species.csv", fileEncoding = "Windows-1252")
# install packages that I might need (check if I need them)
```md
install.packages(c("tidyverse", "lubridate", "vegan"))
# tidyverse: to visualize, clean and change data
# lubridate: to work with date and time
# vegan: to make ecological analyses 
library("tidyverse")
library("lubridate")
library("vegan")
```
# check data nature
```md
glimpse(fish)
```md
# count empty spaces as NA
fish[fish == ""] <- NA
#[] selects a part of the file. i want to change empty strings into null data
```

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
fish$wave.height.m<-as.numeric(fish$wave.height.m)
fish$wave.power.kW.m<-as.numeric(fish$wave.power.kW.m)
fish$wave.period.s<-as.numeric(fish$wave.period.s)
fish$wind.km.h<-as.numeric(fish$wind.km.h)
fish$time <- hm(fish$time)
#hm transforms time into time recognized by R
#check
glimpse(fish)
#remove sea urchin, juveniles and count different kinds of axillary wrasse as same species
fish <- fish |>
  mutate(
#mutate is used to create e new column or modify an existing one like in this case
    species_official = case_when(
      
      # Unire le diverse categorie di axillary wrasse
      fish.species %in% c(
#%in% checks if fish.species is the category associated to the following elements
        "axillary wrasse",
        "axillary wrasse (blue)",
        "axillary wrasse (orange/brown)"
      ) ~ "axillary wrasse",
      
      # Unire le diverse categorie di parrotfish
      fish.species %in% c(
        "parrotfish",
        "parrotfish ",
        "parrot fish"
      ) ~ "parrotfish",
      
      # Escludere juveniles e sea urchin
      fish.species %in% c(
        "juveniles",
        "sea urchin"
      ) ~ NA_character_,
      
      # Mantenere tutte le altre specie
      TRUE ~ fish.species
    )
  )
#coordinates
library(terra)
library(sdm)
library(viridis)
#adding coordinates to dataset
coordinates<-data.frame(site=c("Praia da Pedreira", "Praia Ribeira das Tainhas", "Praia Baixa D'Areia", "Praia do Populo"), latitude= c(37,7154764, 37,7159691, 37,7165400, 37,7484241), longitude=c(-25,4627892, -25,4096397, -25,5179964, -25,6157604))
fish<-merge(fish, coordinates, by = "site")

#OBJECTIVE: STUDY OF FISH COMMUNITY ACROSS THE SOUTHERN COAST OF SAO MIGUEL ISLAND

#1 SPECIES RICHNESS: OVERALL SPECIES RICHNESS AND SITE SPECIFIC SPECIES RICHNESS
#OVERALL SPECIES RICHNESS

sc_richness <- fish |>summarise
#summarise gives the result as a table
(species_richness = n_distinct(species_official, na.rm = TRUE))
#n_distinct counts unique/different values, so how many different species we have. it gives a NUMBER
#na.rm = TRUE tells R to ignore all missing values
sc_richness
#table with species names (overall)
names_sc_richness <- fish |> filter(!is.na(species_official)) |>
#filter selects only specific rows with certain conditions
#! means NOT and is.na selects null values. so filter(!is.na(species_official)) means select only rows were species_official is not null.
distinct(species_official) |>
#distinct removes duplicates. it gives 1 or more NAMES
arrange(species_official) |>
#arrange orders species, usually in alphabetic order
names_sc_richnes

#SITE SPECIES RICHNESS (number)

site_richness <- fish |>filter(!is.na(species_official)) |>
group_by(site) |>
#dplyr function that groups data based on column site
summarise(
species_richness = n_distinct(species_official),
.groups = "drop")
#function that gives the result in the original form
site_richness
#SITE SPECIES RICHNESS (names)
species_by_site <- fish |>
filter(!is.na(species_official)) |>
group_by(site) |>
summarise(
species_richness = n_distinct(species_official),
species = paste(sort(unique(species_official)), collapse = ", "),
#paste puts data in the same string
#sort orders data, usually in alphabetical order
#unique removes duplicates
.groups = "drop")
species_by_site

#2 FREQUENCY

#OVERALL FREQUENCY
#to identify all the transects (sampling occasions)
sampling_occasions <- fish |>
distinct(site, date) 
total_occasions <- nrow(sampling_occasions)
total_occasions
#to calculate overall species frequency
species_frequency_overall <- fish |>
filter(!is.na(species_clean)) |>
distinct(site, date, species_clean) |>
count(species_clean, name = "occurrences") |>
mutate(total_sampling_occasions = total_occasions,
frequency_percent = occurrences / total_occasions * 100) |> arrange(desc(frequency_percent))
species_frequency_overall
#to make the table
frequency_table_overall <- species_frequency_overall |>
select(species = species_clean, frequency_percent)
frequency_table_overall

#SITE FREQUENCY
#to identify the 5 transect per each site
site_sampling_occasions <- fish |>
  distinct(site, date) |>
  count(site, name = "total_sampling_dates")
site_sampling_occasions
#to calculate species fequency for each site
species_frequency_site <- fish |>
  filter(!is.na(species_clean)) |>
  distinct(site, date, species_clean) |>
  count(site, species_clean, name = "occurrences") |>
  left_join(
    site_sampling_occasions,
    by = "site"
  ) |>
  mutate(
    frequency_percent = occurrences / total_sampling_dates * 100
  ) |>
  arrange(site, desc(frequency_percent))
#to make the matrix table
frequency_matrix <- species_frequency_site |>
  select(site, species_clean, frequency_percent) |>
  pivot_wider(
    names_from = site,
    values_from = frequency_percent,
    values_fill = 0
  )

frequency_matrix

#3 MEAN ABUNDANCE

#SITE MEAN ABUNDANCE
#to select the abundances and count 0 when species are not registered
species_sampling <- fish |> filter(!is.na(species_clean)) |> group_by(site, date, species_clean) |> summarise(
abundance = sum(abundances, na.rm = TRUE),
.groups = "drop")

all_species <- fish |> filter(!is.na(species_clean)) |>
distinct(species_clean)
species_sampling_complete <- merge(
sampling_occasions,
all_species,
by = NULL)

#to check the total samplings
nrow(species_sampling_complete)
#to calculate the mean abundance per site
species_sampling_complete <- species_sampling_complete |>
left_join(species_sampling,
by = c("site", "date", "species_clean")) |>
mutate(abundance = replace_na(abundance, 0))
mean_abundance_site

#NUOVO CODICE MEAN ABUNDANCE PER SITE CHE MANCAVA
mean_abundance_site <- species_sampling_complete |>
  group_by(site, species_clean) |>
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    .groups = "drop"
  )
mean_abundance_site

#MEAN OVERALL ABUNDANCE
mean_abundance_overall <- species_sampling_complete |>
  group_by(species_clean) |>
  summarise(
    mean_abundance = mean(abundance),
    .groups = "drop"
  ) |>
  arrange(desc(mean_abundance))
mean_abundance_overall
#tabella totale
abundance_table_overall <- mean_abundance_overall |>
  select(
    species = species_clean,
    mean_abundance
  )

abundance_table_overall
#abundance matrix table
abundance_matrix <- mean_abundance_site |>
  select(site, species_clean, mean_abundance) |>
  pivot_wider(
    names_from = site,
    values_from = mean_abundance,
    values_fill = 0
  )

abundance_matrix
#heatmap overall abundance
ggplot(
  mean_abundance_overall,
  aes(
    x = "Southern coast",
    y = reorder(species_clean, mean_abundance),
    fill = mean_abundance
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(mean_abundance, 2)),
    color = "black",
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "yellow",
    high = "orange"
  ) +
  labs(
    x = NULL,
    y = "Species",
    fill = "Mean abundance",
    title = "Mean abundance of fish species along the southern coast"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold")
  )
#heatmap site abundance
ggplot(
  abundance_matrix |>
    tidyr::pivot_longer(
      cols = -species_clean,
      names_to = "site",
      values_to = "mean_abundance"
    ),
  aes(
    x = site,
    y = reorder(species_clean, mean_abundance),
    fill = mean_abundance
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(mean_abundance, 2)),
    size = 3
  ) +
  scale_fill_gradient(
    low = "yellow",
    high = "orange"
  ) +
  labs(
    x = "Site",
    y = "Species",
    fill = "Mean abundance",
    title = "Mean abundance of fish species across sites"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold"
    ),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

#4 SHANNON DIVERSITY

#SHANNON DIVERSITY OVERALL
# sommare l'abbondanza di ogni specie su tutta la costa
abundance_overall <- species_sampling_complete |>
  group_by(species_clean) |>
  summarise(
    total_abundance = sum(abundance, na.rm = TRUE),
    .groups = "drop"
  )

# calcolare indice di Shannon
shannon_overall <- diversity(
  abundance_overall$total_abundance,
  index = "shannon"
)
shannon_overall

#SHANNON DIVERSITY PER SITE
abundance_site <- species_sampling_complete |>
  group_by(site, species_clean) |>
  summarise(
    total_abundance = sum(abundance, na.rm = TRUE),
    .groups = "drop"
  )

# calcolare Shannon per ogni sito
shannon_site <- abundance_site |>
  group_by(site) |>
  summarise(
    shannon = diversity(
      total_abundance,
      index = "shannon"
    ),
    .groups = "drop"
  ) |>
  arrange(desc(shannon))

shannon_site
#plot shannon per site
ggplot(
  shannon_site,
  aes(
    x = reorder(site, shannon),
    y = shannon
  )
) +
  geom_col(
    fill = "steelblue"
  ) +
  geom_text(
    aes(label = round(shannon, 2)),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    x = "Site",
    y = "Shannon diversity index (H')",
    title = "Fish diversity across sites"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold"
    ),
    plot.title = element_text(face = "bold")
  )


#5 SPECIES ABUNDANCE SIMILARITY 
#matrix species x site

abundance_species_site <- mean_abundance_site |>
  select(species_clean, site, mean_abundance) |>
  pivot_wider(
    names_from = site,
    values_from = mean_abundance,
    values_fill = 0
  )

abundance_species_site
#species correlation matrix
species_correlation <- abundance_species_site |>
  column_to_rownames("species_clean") |>
  t() |>
  cor(
    method = "spearman"
  )
species_correlation

#heatmap
correlation_long <- species_correlation |>
  as.data.frame() |>
  rownames_to_column("species_1") |>
  pivot_longer(
    cols = -species_1,
    names_to = "species_2",
    values_to = "correlation"
  )

ggplot(
  correlation_long,
  aes(
    x = species_1,
    y = species_2,
    fill = correlation
  )
) +
  geom_tile(
    color = "white"
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Spearman\ncorrelation",
    title = "Similarity in species abundance patterns across sites"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
    axis.text.y = element_text(
      size = 8
    ),
    plot.title = element_text(
      face = "bold"
    )
  )

#5 BRAY CURTIS-CLUSTERING

#BRAY CURTIS-CLUSTERING for species >20%
#selection of common species
common_species <- species_frequency_overall |>
  filter(frequency_percent >= 20) |>
  pull(species_clean)

common_species
#matrix
species_matrix_common <- species_sampling_complete |>
  filter(species_clean %in% common_species) |>
  mutate(
    sampling = paste(site, date, sep = "_")
  ) |>
  select(sampling, species_clean, abundance) |>
  pivot_wider(
    names_from = species_clean,
    values_from = abundance,
    values_fill = 0
  )
species_matrix_common_t <- species_matrix_common |>
  column_to_rownames("sampling") |>
  t()
#bray-curtis
bray_species_common <- vegdist(
  species_matrix_common_t,
  method = "bray"
)

bray_species_common
#clustering
cluster_species_common <- hclust(
  bray_species_common,
  method = "average"
)

plot(
  cluster_species_common,
  main = "Clustering of fish species based on Bray-Curtis dissimilarity",
  xlab = "Species",
  ylab = "Bray-Curtis dissimilarity",
  sub = "",
  cex = 0.9
)

#6NMDS

# NMDS - COMMUNITY COMPOSITION

community_matrix <- species_sampling_complete |>
  mutate(
    sampling = paste(site, date, sep = "_")
  ) |>
  select(sampling, species_clean, abundance) |>
  pivot_wider(
    names_from = species_clean,
    values_from = abundance,
    values_fill = 0
  )
#matrix for vegan
community_matrix
community_matrix_nmds <- community_matrix |>
  column_to_rownames("sampling")
#NMDS
set.seed(123)

nmds <- metaMDS(
  community_matrix_nmds,
  distance = "bray",
  k = 2,
  trymax = 100,
  autotransform = FALSE
)

nmds
#check stress
nmds$stress
#(accettabile)
#dataset for plot
nmds_points <- as.data.frame(scores(nmds, display = "sites"))

nmds_points$sampling <- rownames(nmds_points)

nmds_points <- nmds_points |>
  mutate(
    site = sub("_.*", "", sampling),
    date = sub("^[^_]*_", "", sampling)
  )

nmds_points
#plot
ggplot(
  nmds_points,
  aes(
    x = NMDS1,
    y = NMDS2,
    color = site
  )
) +
  geom_point(
    size = 4,
    alpha = 0.8
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "Site",
    title = "NMDS of fish community composition",
    subtitle = paste("Bray-Curtis dissimilarity | Stress =", round(nmds$stress, 3))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )



