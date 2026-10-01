setwd("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project")
install.packages(c("tidyverse", "lubridate", "vegan"))
library("tidyverse")
library("lubridate")
library("vegan")
fish <- read.csv2("C:/Users/froio/OneDrive/Desktop/GCE &SDG/R project/sao miguel fish species.csv", fileEncoding = "Windows-1252")
glimpse(fish)
head(fish)
fish[fish == ""] <- NA
names(fish)
fish<- fish|> fill(site, date, visibility.m, tide, water.T..C, wave.height.m, wave.period.s, wave.power.kW.m, wind.km.h, level, time)
fish$date<-dmy(fish$date)
fish$abundances<-as.numeric(fish$abundances)
fish$size.cm<-as.numeric(fish$size.cm)
fish$visibility.m<-as.numeric(fish$visibility.m)
fish$water.T..C<-as.numeric(fish$water.T..C)
fish$wave.height.m<-as.numeric(fish$wave.height.m)
fish$wave.height.m<-as.numeric(fish$wave.height.m)
fish$wave.power.kW.m<-as.numeric(fish$wave.power.kW.m)
fish$wave.period.s<-as.numeric(fish$wave.period.s)
fish$wind.km.h<-as.numeric(fish$wind.km.h)
fish$time <- hm(fish$time)
unique(fish$fish.species)
install.packages("dplyr")
library(dplyr)
#to create a new column named "transect" where site and date are correlated
fish<-fish|> mutate(transect=paste(site,date,sep ="_"))
#to count transects among the 4 sites
fish |>
  distinct(site, transect) |>
  count(site)
#to calculate the total abundance of each species per site, per transect
species_transect <- fish |>
  group_by(site, transect, fish.species) |>
  summarise(
    transects_abundances = sum(abundances, na.rm = TRUE),
    .groups = "drop"
  )
#(change name transect_abundances)
#abundance of each species per each transect
all_species_transects <- fish |>
distinct(site, transect) |>
     cross_join(
         fish |>
             distinct(fish.species)
     )
#SPECIES RICHNESS OF EACH SITE
site_species_richness<-fish|> group_by(site)|>summarise(richness=n_distinct(fish.species))
#to see the table
site_species_richness
#plot
install.packages("ggplot2")
library(ggplot2)
ggplot(site_species_richness, aes(x = site, y = richness)) +
     geom_col(fill = "lightblue") +
     labs(
         x = "Site",
         y = "Species richness",
         title = "Species richness by site"
     )
#SHANNON INDEX to study the diversity of each transect
community_matrix <- species_transect |>
     tidyr::pivot_wider(
         names_from = fish.species,
         values_from = transects_abundances,
         values_fill = 0
     )
community_matrix
 shannon <- community_matrix |>
     dplyr::select(-site, -transect) |>
     vegan::diversity(index = "shannon")
 shannon_results <- community_matrix |>
     dplyr::select(site, transect) |>
     mutate(
         shannon = vegan::diversity(
             community_matrix |> dplyr::select(-site, -transect),
             index = "shannon"
         )
     )
 shannon_results
# A tibble: 20 × 3
   site                      transect shannon
   <chr>                     <chr>      <dbl>
 1 Praia Baixa D'Areia       Praia B…   0.573
 2 Praia Baixa D'Areia       Praia B…   0.799
 3 Praia Baixa D'Areia       Praia B…   1.07 
 4 Praia Baixa D'Areia       Praia B…   0.500
 5 Praia Baixa D'Areia       Praia B…   0    
 6 Praia Ribeira das Tainhas Praia R…   1.10 
 7 Praia Ribeira das Tainhas Praia R…   0.560
 8 Praia Ribeira das Tainhas Praia R…   0.841
 9 Praia Ribeira das Tainhas Praia R…   0.943
10 Praia Ribeira das Tainhas Praia R…   1.27 
11 Praia da Pedreira         Praia d…   0.564
12 Praia da Pedreira         Praia d…   0.669
13 Praia da Pedreira         Praia d…   1.15 
14 Praia da Pedreira         Praia d…   1.03 
15 Praia da Pedreira         Praia d…   0.455
16 Praia do Populo           Praia d…   1.04 
17 Praia do Populo           Praia d…   0.829
18 Praia do Populo           Praia d…   0.823
19 Praia do Populo           Praia d…   1.19 
20 Praia do Populo           Praia d…   0.912
#plot
ggplot(shannon_results, aes(x = site, y = shannon)) +
     geom_boxplot(fill = "lightgreen") +
     stat_summary(
         fun = mean,
         geom = "point",
         color = "red",
         size = 3
     ) +
     labs(
         x = "Site",
         y = "Shannon diversity",
         title = "Shannon diversity by site"
     )
#SPECIES FREQUENCY
species_frequency <- species_transect |>
     group_by(site, fish.species) |>
     summarise(
         frequency = n_distinct(transect) / 5,
         .groups = "drop"
     ) |>
     mutate(
         frequency_percent = frequency * 100
     ) NO!!!
species_frequency_plot <- species_frequency |>
  group_by(site) |>
  arrange(frequency_percent) |>
  mutate(fish.species = factor(fish.species, levels = unique(fish.species))) |>
  ungroup()

#plot
ggplot(
     species_frequency,
     aes(
         x = frequency_percent,
         y = reorder_within(fish.species, frequency_percent, site)
     )
 ) +
     geom_col(fill = "steelblue") +
     facet_wrap(~ site, scales = "free_y") +
     scale_y_reordered() +
     scale_x_continuous(
         limits = c(0, 100),
         breaks = seq(0, 100, 20)
     ) +
     labs(
         x = "Frequency (%)",
         y = "Species",
        title = "Frequency of fish species by site"
     ) +
     theme_minimal() +
     theme(
         axis.text.y = element_text(size = 7),
         strip.text = element_text(face = "bold")
     ) NO!!!
ggplot(
  species_frequency_plot,
  aes(
    x = frequency_percent,
    y = fish.species
  )
) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ site, scales = "free_y") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Frequency (%)",
    y = "Species",
    title = "Frequency of fish species by site"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold")
  )

#TOTAL MEAN ABUNDANCE OF EACH RECORDED SPECIES IN EACH SITE
species_abundance
ggplot(
+     species_abundance,
+     aes(
+         x = mean_abundance,
+         y = reorder(fish.species, mean_abundance)
+     )
+ ) +
+     geom_col(fill = "steelblue") +
+     facet_wrap(~ site, scales = "free_y") +
+     labs(
+         x = "Mean abundance",
+         y = "Species",
+         title = "Mean abundance of recorded species by site"
+     ) +
+     theme_minimal() +
+     theme(
+         axis.text.y = element_text(size = 7),
+         strip.text = element_text(face = "bold")
+     )

