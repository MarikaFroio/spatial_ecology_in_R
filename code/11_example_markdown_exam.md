# example markdown code for the exam 
# how to import external data in R

the packages needed are the following:
```r
library(terra)
library(imageRy)
```
first of all, it is important to set the working directory by:
```r
set("C://Users/froio/Desktop/GCE &SDG/spatial ecology in R")
```
R doesn't read the backslash, so we have to change them
to check if the working directory has been properly sest, you can make use of:
```r
getwd()
```
once the data to be used has been put on the working directory, we can upload it to R by:
```r
group<-rast("image.JPG")
```
## visualizing the data
in order to visualize data we will use the RGB scheme
```r
im.plotRGB(group, r=1, g=2, b=3)
```

the image is flipped, hence let's act!
```r
group<-flip(group)
```

exporting the result of the previous code is done by:
```r
png("groupphoto.png")
im.plotRGB(group, r=1, g=2, b=3)
dev.off()
```

the output is then:
![image](https://github.com/user-attachments/assets/b31bb614-2d04-40c8-b12f-f46ac5c256c0)


changing the layers inside the RGB scheme is done by:
```r
png("groupphotonew.png")
im.plotRGB(group, r=1, g=2, b=3)
dev.off()
```

the results of layer inversion will be a false color like:
drag and drop the new photo

