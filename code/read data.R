setwd('D:/columbia/research/nydata2/')


library(tidyverse)
library(data.table)


tbl <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.,skip = 2))

filenames<-list.files(pattern = "*.csv")

#as.numeric(substr(filenames[1],1,4))
#as.numeric(substr(filenames[1],7,10))

tbl$lon<-1
tbl$lat<-1

numberoffiles<-length(filenames)
lengthforeachfile<-nrow(tbl)/numberoffiles

#strsplit(filenames[i], "[_]")[[1]][1]
for(i in 1:numberoffiles){
  tbl[(1+lengthforeachfile*(i-1)):(lengthforeachfile+lengthforeachfile*(i-1)),"lon"]<-as.numeric(strsplit(filenames[i], "[_]")[[1]][1])
  tbl[(1+lengthforeachfile*(i-1)):(lengthforeachfile+lengthforeachfile*(i-1)),"lat"]<-as.numeric(strsplit(filenames[i], "[_]")[[1]][2])
  print(i)
}

#write.csv(tbl, file = "D:/columbia/research/ny energy/data.csv")
tbl<-read.csv(file = "D:/columbia/research/ny energy/data.csv")
tbl <- tbl[,-1]

DHIDNI<-tbl[,6:7]
library(data.table)
DHIDNI2<-setDT(DHIDNI)[, as.list(colSums(.SD)), by = gl(ceiling(58030920/24), 24, 58030920)]
DHIcol2<-DHIDNI2[,2]
DNIcol2<-DHIDNI2[,3]
#DNIcol<-setDT(DHI)[, as.list(colSums(.SD)), by = gl(ceiling(58030920/24), 24, 58030920)]
#DHIcol2<-DHIcol[,2]
#colnames(DHIcol2) <- c("DHI")
#DNIcol<-setDT(DNI)[, as.list(colSums(.SD)), by = gl(ceiling(58030920/24), 24, 58030920)]
#DNIcol2<-DNIcol[,2]
#colnames(DNIcol2) <- c("DNI")

newdf = tbl[seq(1, nrow(tbl), 24), ]
newdf$DHI<-DHIcol2
newdf$DNI<-DNIcol2
newdf <- newdf[,-c(4,5)]
#write.csv(newdf, file = "D:/columbia/research/ny energy/datasummed.csv", row.names=FALSE)
#save(newdf,file="data.Rda")
load("data.Rda")

load("data.Rda")
library(reshape2)
T1<-reshape(newdf[,c(1:4,6:7)], idvar = c("Year","Month","Day","lon"), timevar = "lat", direction = "wide")
T2<-reshape(T1, idvar = c("Year","Month","Day"), timevar = "lon", direction = "wide")
T3<-T2[ , colSums(is.na(T2)) == 0]

T3<-T3[!(T3$Month==2 & T3$Day==29),]
#save(T3,file="data2.Rda")
T3[2915:2925,4:334]<-T3[3280:3290,4:334]
