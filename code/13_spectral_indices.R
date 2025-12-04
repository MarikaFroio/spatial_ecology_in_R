#this script helps calculating spectral indices

library(terra)
library(imageRy)

#list files
im.list()

im.import("matogrosso_l5_1992219_lrg.jpg")
#layer 1=NIR, layer 2=red. layer3=green
im.plotRGB(m1992, r=1, g=2, b=3)
im.plotRGB(m1992, r=2, g=1, b=3)
im.plotRGB(m1992, r=2, g=2, b=1)

m2006<-im.import("matogrosso_l5_1992219_lrg.jpg")


#spectral indices
#NDVI different vegetation index
#0-100
#healthy vegetation
#NIR: 90, red: 10 
#DVI=NIR-red=90-10=80

#stressed vegetatiion
#NIR: 60, red: 20
#DVI=NIR-red=60-20=40

#calculating DVI 
dvi1992=m1992[[1]]-m1992[[2]]
dvi2006=m2006[[1]]-m2006[[2]]

par(mfrow=c(1,2))
plot(dvi1992)
plot(dvi2006)


ndvi1992<-im.ndvi(m1992, 1, 2)
ndvi2006<-im.ndvi(m2006, 1, 2)
plot(ndvi1992)
plot(ndvi2006)

#range of ndvi
#0-100
#ndvi=nir-red/(nir+red) = (100-0)/(100+0)=1
#ndvi=nir-red/(nir+red) = (0-100)/(0+100)=-1
#ndvi will always range from -1 to 1



