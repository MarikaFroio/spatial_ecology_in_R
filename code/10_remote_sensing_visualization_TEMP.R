# Code to visualize remote sensing data

# Zenodo set: https://zenodo.org/records/15645465
# install.packages("imageRy")

library(imageRy)
library(terra)

# Listing data inside imageRy
#all functions from imageRy start with im.
im.list()

#sentinel program from ESA: european space agency or ecological society of america. mission sentinel: a satellite covering the planet every 10 days we get an image. among such images the spatial resolution is 10 meters. we are going to use this data.
#sentinel satellite is composed by several different sensors each of which is recording the landscape in a different wavelenght
#the layers are called bands
#let's improt the first band
# Sentinel-2 bands
# https://gisgeography.com/sentinel-2-bands-combinations/
b2<-im.import("sentinel.dolomites.b2.tif")
#now we can plot it
plot(b2)
#the image is representing the reflectance of objects. the reflectance is the capacity of objects to reflect energy
#the reflectance ranges from 0 to 1
#in the image we have the blue wavelength, 490 nm which means that the wavelength is very small
#everything reflecting the blue is yellow
#everything which is dark means that the blu wavelength energy is being absorbed
# importing the data
b2 <- im.import("sentinel.dolomites.b2.tif")
library(viridis)
cl <- colorRampPalette(c("black", "grey", "light grey")) (100)
plot(b2, col=cl)

#exercise: import and plot band 3 with thr legend mako from the viridis package
b3<-im.import("sentinel.dolomites.b2.tif)
library(viridis)
plot(b3, col=mako (100))

cl <- colorRampPalette(c("black", "grey", "white")(100))
plot(b2, col=cl)

plot(b2, col=cl)
plot(b3, col=cl)

par(mfrow=c(1,2))
plot(b2, col=cl)
plot(b3, col=cl)
#how to build our own functions: there is a function
multiframe<-function(x,y){
par(mfrow=c(x,y))
}
multiframe(1,2)
plot(b2, col=cl)
plot(b3, col=cl)

#exercise: using the function that you developed plot:
#band 2
#band 3
#their relationship
#one beside the other

multiframe(1,3)
#1 row and 2 columns
plot(b2, col=cl)
plot(b3, col=cl)
plot(b2,b3)
#they are really correlated with each other

#import band 4 (red)
b4<-im.import("sentinel.dolomites.b4.tif")

dev.off() #??
plot(b4)
#can be seen by colorblind people

#import b8. now we are in the infrared. NIR infrared (b8). NIR stands for near infrared- it is the nearest region of the infrared to the visible pattern
#huge wavelenght
b8<-im.import("sentinel.dolomites.b8.tif")
plot(b8)
#the infrared provides the possibility to distinguish several objects that we would not see otherwise. 

#exercise: using your function, plot the 4 bands:
#b2 b3
#b4 b8

multiframe (2,2)
plot(b2, col=inferno(100))
plot(b3, col=inferno(100))
plot(b4, col=inferno(100))
plot(b8, col=inferno(100))

#exercise: with your function, plot the following statistical relationships beside the other
#b2 b3
#b2 b8

multiframe (1,2)
plot(b2, col=inferno(100))
plot(b3, col=inferno(100))
plot(b4, col=inferno(100))
plot(b8, col=inferno(100))
plot(b2,b3, xlab="b2-blue", ylab="b3-green", main="relationship b2 vs. b3")
plot(b2,b8, xlab="b2-blue", ylab="b8-NIR", main="relationship b2 vs. b8")
#in the second case the correlation is not direct as in the first case. every single part of the graph repsents a different part of the story. those that have small blue and high NIR (top left) represents the vegetation since it is high reflecting in the infrared and low in the blue. this is becasue they absorb blue and red light to do the photosynthesis
#vegetation reflects a lot in the NIR thank to a tissue that is a series of vertical cells. the radiation catches evert single cell and is reflected a lot. another radiation that is reflected a lot is the green one.



