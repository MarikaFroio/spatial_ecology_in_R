#how to repeat commands
#we can now compare the temporal relationship. we can create a loop to repeat the same action for every species

#unique is a function that selects each species
#select the species
#assign the time
#create the graph





#multiframe
species_list <- unique(kerinci$Sps)

# Set up a multiframe: choose layout based on number of species
n <- length(species_list)
rows <- ceiling(sqrt(n))
cols <- ceiling(n / rows)

par(mfrow = c(rows, cols))   # create multiframe layout

# Loop to plot each species
for(sp in species_list){
  
  temp <- kerinci[kerinci$Sps == sp, ]
  temp_time <- temp$Timecirc
  
  densityPlot(temp_time,
              main = paste("Density plot:", sp))
}


