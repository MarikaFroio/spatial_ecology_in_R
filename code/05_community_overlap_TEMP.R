#this code is analysing the temporal overlap between species

#install.packages("overlap")
library(overlap)

data(kerinci)
circulartime<-kerinci$time*2*pi
kerinci$circ<-kerinci$time*2*pi

tiger<-kerinci[kerinci$Sps=="tiger",]
#, is used to stop the comand 

timetiger<tiger$circ
#now I have all the times of the tiger ????

denistyPlot(timetiger)
