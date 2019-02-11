library(keynumbers)

Sydney <-kn.getModel(modelname="sydney-water-usuage-1879")

collnames <- kn.modelSegmentColNames(Sydney)

coll2 <- kn.getCollection("3019")
coll3 <- kn.getCollection("3023")
coll2df2 <- kn.coll2df(coll2[[1]])
coll3df3 <- kn.coll2df(coll3[[1]])

kn.modelExecLocal(Sydney)

seg_num = 2
m <- kn.modelExecRemote("sydney-water-usuage-1879")
m <- kn.modelExecRemote("sydney-water-usuage-1879", seg_nb=seg_num, list(dividend=838625.5, divisor=1))
M <- kn.modelExecRemoteDF("sydney-water-usuage-1879", seg_nb=seg_num, coll2df2)
sapply(M, function(x) x$result$number)
sapply(M, function(x) x$segments[[2]]$dividend$number)

Models <- kn.modelRep(Sydney, seg_num)
coll2df2[,"model1879"] <- sapply(1:length(Models), function(x) kn.modelExec(Models[[x]]))

if (! file.exists("AU.zip")) {
  download.file("http://download.geonames.org/export/dump/AU.zip","AU.zip")
  unzip("AU.zip")
}

if (!("AU" %in% ls()))
  AU <- read.delim("~/keynumbers/AU.txt", header=FALSE)

colNames <- c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature_class", "feature_code", "country_code",
              "cc2", "admin1_code", "admin2_code", "admin3_code", "admin4_code", "population", "elevation", "dem", "timezone", "modification_date")
colnames(AU) <- colNames
AU$name <- as.character(AU$name)
mylocs <- as.character(coll2df2$location)
mylocs[8] <- "Wingecarribee Dam"
mylocs[9] <- "Fitzroy Falls Dam"

loc_idx <- sapply(mylocs, function(x) {
  idx <- grep(x, AU$name, ignore.case = T)
  if (length(idx) > 0)
    idx[1] #c(x, idx[1], AU$V2[idx[1]])
  else
    NA #c(x, NA)
  })

coll2df2$latitude <- as.numeric(AU$latitude[loc_idx])
coll2df2$longitude <- as.numeric(as.character(AU$longitude[loc_idx]))

labs <- lapply(1:nrow(coll2df2), function(x)
  paste0(coll2df2$location[x],": ", coll2df2$number[x], " ", coll2df2$unit[x]))

library(leaflet)
(coll2df2 %>%
  mutate(number.scaled = 10*(2+((number - mean(number)) / sd(number)))) %>%
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(
    label = labs,
    radius = ~number.scaled,
    lat = ~latitude, lng = ~longitude,
    stroke = FALSE, fillOpacity = 0.7
  ) -> m )

seg_num = 3
m <- kn.modelExecRemote("sydney-water-usuage-1879", seg_nb=seg_num, list(dividend=838625.5, divisor=1))
M <- kn.modelExecRemoteDF("sydney-water-usuage-1879", seg_nb=seg_num, coll3df3)
sapply(M, function(x) x$result$number)
sapply(M, function(x) x$segments[[3]]$dividend$number)

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
