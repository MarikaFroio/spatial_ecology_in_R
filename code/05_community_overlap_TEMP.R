#this code is analysing the temporal overlap between species

#install.packages("overlap")
library(overlap)

data(kerinci)
#we have to translate the linear time into circular time
circulartime<-kerinci$time*2*pi
#circ is the name for the column (below)
kerinci$Timecirc<-kerinci$time*2*pi

#we select tiger into the dataset
tiger<-kerinci[kerinci$Sps=="tiger",]
#, is used to stop the comand 

#to build the density plot

densityPlot(tiger$Timecirc)
tigertime<-tiger$Timecirc
densityPlot(tigertime) #check the code

#exercise: create a kernel density plot for the species called macaque
macaque<-kerinci[kerinci$Sps=="macaque",]
macaquetime<-macaque$Timecirc
densityPlot(macaque$Timecirc)

#the tiger had 2 peaks, 1 in the morning and 1 in the evening while the macaque are shifted towards the morning. why??

#we can see the overlpa over the 2 graphs. we can use the function overlapPlot
overlapPlot(tigertime, macaquetime)
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

# Reset plotting layout
par(mfrow = c(1,1))



timetiger<tiger$circ
#now I have all the times of the tiger ????

denistyPlot(timetiger)




#20/11
