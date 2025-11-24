# install.packages("sdm")
# install.packages("terra")
# terra → used for spatial data (rasters, vectors, maps), sdm → a package for species distribution modeling

library(terra)
library(sdm)

file <- system.file("external/species.shp", package="sdm")
#This line: looks inside the sdm package folder, finds the example file "species.shp", returns its full path and saves it in the variable file.
#species.shp is the name of a shapefile that contains species occurrence data. A shapefile is a common GIS file format for storing vector spatial data (points, lines, or polygons).
It is used to store geographic features along with their attributes.

rana <- vect(file)
#vect() is a terra function. It reads the shapefile and converts it into a SpatVector object.
#A SpatVector is a data object used in the terra package in R to store vector spatial data.
#rana now contains the spatial points from the shapefile. The shapefile contains species occurrence points (locations where a species was observed). 

plot(rana)
#In short: You load spatial packages, You locate an example shapefile included with sdm, You read it as a vector object, You plot the species occurrence points

# Assuming 'rana' is your SpatVector with points and attributes
# Extract coordinates using the geom() function
coordinates <- geom(rana)
#In the context of terra (the package you are using), geom() is a function used with a SpatVector to extract its geometry.
#For a SpatVector, geom() returns the coordinates of the geometry (points, lines, polygons), usually in a tabular form.
#This will give you a data frame-like object containing: the x and y coordinates, geometry IDs, part IDs (for polygons or multi-part features)
#Even though geom(rana) looks like a data frame when printed, it is actually a matrix or terra-specific object, not a genuine R data.frame.

# Convert the coordinates to a data frame
coordinates_df <- as.data.frame(coordinates)
#to convert it into a real data frame, especially if you want to use functions that require a true data.frame

# Extract the 'Occurrence' attribute from the SpatVector
occurrence_df <- as.data.frame(rana$Occurrence)

# Combine the coordinates and the occurrence data into one data frame
final_df <- cbind(coordinates_df, occurrence_df)






# install.packages("sdm")
# install.packages("terra")
##install.packages("viridis") ##to change the color palette more easily
library(sdm)
library(terra)
library(viridis)

file <- system.file("external/species.shp", package="sdm")
# [1] "/usr/local/lib/R/site-library/sdm/external/species.shp"

rana <- vect(file)
rana

rana$Occurrence

plot(rana)

pres <- rana[rana$Occurrence==1]
#pres = presence-only data
#the command is used to subset the dataset and keep only the points where the species is present.
#1 = species present, (sometimes 0 = species absent, depending on dataset)
#in short: This command filters the dataset, keeping only the spatial records where the species was observed (Occurrence = 1).

# Exercise: plot in a multiframe the rana dataset beside the pres dataset
par(mfrow=c(1,2))
plot(rana)
plot(pres)

# Exercise: select data from rana with only absences
abse <- rana[rana$Occurrence==0]
plot(abse)

# Exercise: plot in a multiframe presences beside absences
par(mfrow=c(1,2))
plot(pres)
plot(abse)

# Exercise: plot in a multiframe presences on top of absences
par(mfrow=c(2,1))
plot(pres)
plot(abse)

# Excercise: plot the presences in blue together with absences in red
dev.off() #???
plot(pres, col="blue", pch=19, cex=2)
points(abse, col="red", pch=19, cex=2)

# Covariates
elev <- system.file("external/elevation.asc", package="sdm")
# [1] "/usr/local/lib/R/site-library/sdm/external/elevation.asc"

elevmap <- rast(elev)
#In R, the function rast() from the terra package is used to read or create raster objects.
#rast() reads the file and converts it into a SpatRaster object.
#A SpatRaster is terra’s data structure for raster data.
#Once it is a SpatRaster, you can: plot it, extract values, manipulate the raster, overlay points, compute statistics, etc.

# Exercise: change the colors of the elevation map by the colorRampPalette function
cl <- colorRampPalette(c("green","hotpink","mediumpurple"))(100)
plot(elevmap, col=cl)

# Exercise: plot the presnces together with elevation map 
##to see the points on the map: where we have high elevation we have less animals???
points(pres, pch=19)
# Export the final data frame to a CSV file
write.csv(final_df, "coordinates_with_occurrence.csv", row.names = FALSE)

##excercise: import temperature and plot the precences vs temperature
temp<-system.file("external/temperature.asc",package="sdm")
##converting path to file
tempmap<-rast(temp)
plot(tempmap)
plot(tempmap, col=mako(100))

##excercise: plot elevation and temperature with presences one beside the other
par(mfrow=c(1,2))
plot(elevmap, col=mako(100))
points(pres)
plot(tempmap, col=mako(100))
points(pres)


##precipitation
prec<-system.file("external/precipitation.asc", package="sdm")

precmap<-rast(prec)
plot(precmap)
points(pres)


##let's import the amount of vegetation
vege<-system.file("external/vegetation.asc", package="sdm")
vegemap<-rast(vege)
plot(vegemap)
points(pres)
## if there is a higher vegetation it is worse for predators??

##excersice: plot all the ancillary variables in multiframe
par(mfrow=c(2,2))
plot(elevmap)
plot(tempmap)
plot(precmap)
plot(vegemap)




## we can create an array of map stacked
anci<-c(elevmap, tempmap, precmap, vegemap)
##this requires the package terra
##to create a multiframe
plot(anci, col=magma(100))





# View the first few rows of the final table (optional)
head(final_df)

# Add the attribute column (e.g., Occurrence) to the data frame
coordinates_df$Occurrence <- rana$Occurrence

# Export the data frame to a CSV file
write.csv(coordinates_df, "coordinates_with_occurrence.csv", row.names = FALSE)

# Occurrences
rana$Occurrence

# Selection of presences
pres <- rana[rana$Occurrence==1]

# Exercise: select all absences
abse <- rana[rana$Occurrence==0]
# or
abse <- rana[rana$Occurrence!=1]

# Ecercise: plot the presences with a color together with the absences with another color
plot(pres, col="#76EE00") # chartreuse1
# or
plot(pres, col="chartreuse1") 

plot(abse, col="#FF1493") # deeppink
# I cannot use plot but, rather, I would prefer using points()

plot(pres, col="chartreuse1") 
points(abse, col="#FF1493") # deeppink

 


par(mfrow=c(2,1))
plot(pres)
plot(abse)





##how to monitor communities with R
imagine we are going to the field to sample some sites. we are going to measure some organisms.
we should create an abundance matrix with the different plots for the different species.
then i can construct a graph

##we can use the package vegan
