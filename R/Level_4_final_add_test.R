### ADV Level 3 - add found (previously not detected)

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
res1 <-read.csv(file.path(out,"/Level_4/ADV_level_3_add_test.csv"),encoding = "UTF-8")

head(res1)

# get spatial object from table
names(res1)
names(res1[,21:22])
test <- res1[which(is.na(res1$x_new)==F),]
all_sp1 <- SpatialPointsDataFrame(test[,21:22],test)
any(all_sp1$x_new==999)
length(which(all_sp1$x_new==999))

# delete rows without coordinates
all_sp_c1 <-subset(all_sp1,all_sp1$x_new!=999)
#set proj
proj4string(all_sp_c1) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad
pal = mapviewPalette("mapviewRasterColors")
mapview(all_sp_c1,col.regions = pal(100),zcol="Quadrant")

# check problems

any(res1$x_new==res1$y_new)
which(res1$x_new==res1$y_new&res1$x_new==999)

res1[12,]

# vidsule check up in very close neighborhood

# ID 861, 1183
# ID 750, 753
# ID 967, 713

# visual validation false deteced

# ID 839

# ID 1235 maybe wrong ?

# assign Tag for visual false entries
res1$final_check <- 999
res1[which(res1$ID==861),]

res1$final_check[which(res1$ID==861)] <- "corrupted"
res1$final_check[which(res1$ID==1183)] <- "corrupted"
res1$final_check[which(res1$ID==750)] <- "corrupted"
res1$final_check[which(res1$ID==753)] <- "corrupted"
res1$final_check[which(res1$ID==967)] <- "corrupted"
res1$final_check[which(res1$ID==713)] <- "corrupted"
res1$final_check[which(res1$ID==839)] <- "false sector"
res1$final_check[which(res1$ID==1235)] <- "corrupted"
res1$final_check[which(res1$ID==318)] <- "false sector"

# rewrite points
# get spatial object from table
names(res1)
names(res1[,21:22])
test <- res1[which(is.na(res1$x_new)==F),]
all_sp1 <- SpatialPointsDataFrame(test[,21:22],test)
any(all_sp1$x_new==999)
length(which(all_sp1$x_new==999))

# delete rows without coordinates
all_sp_c1 <-subset(all_sp1,all_sp1$x_new!=999)
#set proj
proj4string(all_sp_c1) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad
pal = mapviewPalette("mapviewRasterColors")
mapview(all_sp_c1,col.regions = pal(100),zcol="Quadrant")
# write data
write.csv(res1,file.path(out,"/Level_4/ADV_Level_4_v1.csv"),row.names = F)
writeOGR(all_sp_c1,file.path(out,"Level_4/ADV_Validation_Level_4.shp"),driver="ESRI Shapefile",layer="ADV_Points_l4.shp")
