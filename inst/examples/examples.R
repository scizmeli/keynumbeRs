library(keynumbers)
a<-kn.getModel(modelname="sydney-water-usuage-1879")

collnames <- kn.modelSegmentColNames(a)

coll2 <- kn.getCollection("3019")
coll3 <- kn.getCollection("3023")
coll2df2 <- kn.coll2df(coll2)
coll2df3 <- kn.coll2df(coll3)

kn.modelExec(a)

seg_num = 2
Models <- kn.modelRep(a, seg_num)
sapply(1:length(Models), function(x) kn.modelExec(Models[[x]]))

seg_num = 3
Models <- kn.modelRep(a, seg_num)
sapply(1:length(Models), function(x) kn.modelExec(Models[[x]]))


