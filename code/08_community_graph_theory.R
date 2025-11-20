#codes for performing graph theory analysis on community data
install.packages("igraph") # run once
library(igraph)


#let's create a dataset called species
species<-("Algae", "Zooplankton", "Small Fish", "Large Fish", "bird")
#we can concatenate them with the function c
species<-c("Algae", "Zooplankton", "Small Fish", "Large Fish", "bird")

predator<-c("Zooplankton", "Small Fish", "Large Fish","Birds", "Birds")
prey<-c("Algae", "Zooplankton", "Small Fish", "Small Fish", "Large Fish")
#now that we have a list of predators and preys we can create a dataframe
interactions<-data.frame(predator, prey)
