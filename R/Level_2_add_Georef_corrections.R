### ADV Level 2 - add found places (previously multiple and no unique) 

# load packages
require(openxlsx)
require(rgdal)
require(raster)
require(mapview)
require(rgeos)

# set up folders
wd <-"C:/Envimaster/ADV"
org <- file.path(wd,"Data/org/")
out <- file.path(wd,"Data/output")
map <- file.path(wd,"Data/MapDATA")

# load datasets
res1 <-read.csv(file.path(out,"Level_1/ADV_automatch_level_1.csv"))
res2 <-read.csv(file.path(out,"Level_2/ADV_level_1_corrections_clean.csv"))

# check
head(res1)
head(res2)

# reorder for mathcing IDs
res2 <-res2[order(res2$X.1),]
res1 <-res1[order(res1$X.1),]

# check for equal
all.equal(res1$X.1,res2$X.1)
all.equal(res1$Quadrant,res2$Quadrant)
which(res1$Quadrant!=res2$Quadrant)
res1$Quadrant[237]
res2$Quadrant[237]

# new df and add columns
res3 <- res1
res3$prev <-res2$prev
res3$x_new <- res2$x
res3$y_new <- res2$y
res3$cor <- res2$new_res

# one entire has far too long x coordinate 
which(res3$x_new==max(res3$x_new))
res3[254,]
res3$x_new[254] <- 999
res3$y_new[254] <- 999
res3$cor[254] <- "corrupted"

res3[254,]

# 3 entires have x == y coordinates
which(res3$x_new==res3$y_new)
which(res3$x_new==res3$y_new & res3$x_new!=999)

res3[c(284, 301, 318),]

res3$x_new[c(284, 301, 318)] <-999
res3$y_new[c(284, 301, 318)] <-999
res3$cor[c(284, 301, 318)] <- "corrupted"
# reorder columns change colnames
head(res3)

# set ID name
colnames(res3)
colnames(res3)[2] <- "ID"
colnames(res3)[14] <- "comment"

res3 <-res3[order(res3$ID),]

# clean up false detected
which(res3$cor=="false detected")
res3$x_new[which(res3$cor=="false detected")] <-999
res3$y_new[which(res3$cor=="false detected")] <-999

# clip unused second ID col
res3 <-res3[,2:ncol(res3)]

head(res3)

# write data table
write.csv(res3,file.path(out,"Level_2/ADV_level_2.csv"))


# get spatial object from table
names(res3)
names(res3[,21:22])
all_sp1 <- SpatialPointsDataFrame(res3[,21:22],res3)
any(all_sp1$x==999)
length(which(all_sp1$x==999))

# delete rows without coordinates
all_sp_c1 <-subset(all_sp1,all_sp1$x_new!=999)
#set proj
proj4string(all_sp_c1) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad
mapview(all_sp_c1,zcol="Quadrant")

# check if shp is equal in lentgh than detected
length(all_sp_c1)
length(which(res3$x_new!=999))
length(which(res1$res=="detected"))

# detected check
1085/1270
length(which(res1$res=="detected"))/1270

# write shp for spatial validation
writeOGR(all_sp_c1,file.path(out,"Level_2/ADV_Points_Level_2.shp"),driver="ESRI Shapefile",layer="ADV_Points_l2.shp")

head(res3)
