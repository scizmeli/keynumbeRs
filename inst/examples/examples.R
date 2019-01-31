library(keynumbers)
Sydney <-kn.getModel(modelname="sydney-water-usuage-1879")

collnames <- kn.modelSegmentColNames(Sydney)

coll2 <- kn.getCollection("3019")
coll3 <- kn.getCollection("3023")
coll2df2 <- kn.coll2df(coll2)
coll2df3 <- kn.coll2df(coll3)

kn.modelExec(Sydney)

seg_num = 2
Models <- kn.modelRep(Sydney, seg_num)
sapply(1:length(Models), function(x) kn.modelExec(Models[[x]]))


if (! file.exists("AU.zip")) {
  download.file("http://download.geonames.org/export/dump/AU.zip","AU.zip")
  unzip("AU.zip")
}
AU <- read.delim("~/keynumbers/AU.txt", header=FALSE)

seg_num = 3
Models <- kn.modelRep(Sydney, seg_num)
coll2df3[,"model1879"] <- sapply(1:length(Models), function(x) kn.modelExec(Models[[x]]))

library(ggplot2)
ggplot(data = coll2df3, aes(x = date, y = model1879))+
  geom_line(color = "#00AFBB", size = 2)

library(xts)
library(dygraphs)
library(dplyr)
coll2df3 %>%
  select(model1879) %>%
  xts(coll2df3$date) %>%
  dygraph() %>%
  dySeries("model1879",strokeWidth = 4)

library(leaflet)
pal <- colorQuantile("YlOrRd", NULL, n = 8)
leaflet(orstationc) %>%
  addTiles() %>%
  addCircleMarkers(color = ~pal(tann))
