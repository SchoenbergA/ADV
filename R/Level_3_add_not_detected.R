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
res1 <-read.csv(file.path(out,"Level_2/ADV_level_2.csv"))
res2 <-read.csv(file.path(out,"Level_3/ADV_Level_1_not_detected.csv"),encoding = "UTF-8")

head(res1)
head(res2)

colnames(res1)
colnames(res2)


# delete unneded second ID
res1<-res1[,2:ncol(res1)]

# check colnames
all.equal(colnames(res1),colnames(res2))
# correct ID colname
colnames(res2)[1] <- "ID"
colnames(res2) [13]<- "comment"
colnames(res2) [20]<- "cor"
all.equal(colnames(res1),colnames(res2))

# add dummy cols for rbind
res2$prev <-"NANA"
res2$xx <- 99999
res2$yy <- 99999
colnames(res1)
colnames(res2)  
  
# order
res2 <- res2[,c(1:16,22,23,19,21,17,18,20)]

colnames(res2)[17]<-"x"
colnames(res2)[18]<-"y"
colnames(res2)[21]<-"x_new"
colnames(res2)[22]<-"y_new"

all.equal(colnames(res1),colnames(res2))


# subset found "Not deteced" and new entires
res3 <- res2[1:150,]
res4 <- res2[151:nrow(res2),]

# check if "Not deteced" amount in Level 2 is equal to res3
length(which(res1$res=="not detected")) # works
length(which(res1$x_new==999)) # 15 more entires have no coordinates
res1[which(res1$x_new==999 & res1$res=="not detected"),] # comes from "no unique and false deteced

# check if names are equal
all.equal(res1$Ortsname[which(res1$res=="not detected")],res3$Ortsname) # problöem with tailing whitesapces
all.equal(res1$ID[which(res1$res=="not detected")],res3$ID) # works

# subset all not "not detected" and r bind found "not detected"
length(which(res1$res=="not detected"))

cords <- subset(res1,res1$res!="not detected")
l3 <- rbind(cords,res3)

# check equal

l3 <-l3[order(l3$ID),]
all.equal(res1$Ortsname,l3$Ortsname)# tailing whitesapces

all.equal(res1$ID,l3$ID)

# trimm tailing whitesapces
l3$Ortsname <-trimws(l3$Ortsname, which = "right")


# check result
length(which(l3$x_new!=999))/1270

# add new entries
l3f <- rbind(l3,res4)

head(l3f)
length(which(l3f$x_new!=999))/1270
length(which(l3f$x_new==999))

# !!! ID 490 is missing. This causes Problem when selecting by []
# is missing since "corrected table"
nrow(l3f)
l3f[1296,]
length(unique(l3f$ID))
max(l3f$ID)
min(l3f$ID)
length(1:1297)

l3f$ID[!(l3f$ID %in% c(1:1297))]
l3f$ID[1]
l3f$ID[460]
l3f$ID[490]
l3f$ID[500]


# add dummy row for 490
newrow <- l3f[123,]# take random row
newrow[,2:ncol(newrow)] <-999 # set all values to 999
newrow$ID <- 490 # set ID

# rbind ID 490 and reorder by ID
nrow(l3f)
l3f <- rbind(l3f,newrow)
l3f <-l3f[order(l3f$ID),]
# rownames are not equla to ID due to rbind (so row number is not ID)
rownames(l3f) <- l3f$ID
### visual validation
# get spatial object from table
names(l3f)
names(l3f[,21:22])
test <- l3f[which(is.na(l3f$x_new)==F),]
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

# false detected
# ID c(1183,318,850,1226)
which(l3f$ID==c(1183,318,850,1226))
l3f[1183,]

l3f$x_new[c(1183,318,850,1226)] <- 999
l3f$y_new[c(1183,318,850,1226)] <- 999
l3f$cor[c(1183,318,850,1226)] <- "false detected 2nd"

which(is.na(l3f$x_new))
l3f$x_new[c(1269 ,1271)] <- 999
l3f$y_new[c(1269 ,1271)] <- 999
l3f$cor[c(1269 ,1271)] <- "NANA"

which(l3f$cor=="corrupted 2nd")
l3f[318,]
# get spatial object from table
names(l3f)
l3f[1296,]
nrow(l3f)
names(l3f[,21:22])
sp2 <- SpatialPointsDataFrame(l3f[,21:22],l3f)
length(which(sp2$x_new==999))

# delete rows without coordinates
sp2 <-subset(sp2,sp2$x_new!=999)
l3f$Ortsname[1183]
sp2$Ortsname[1183]
length(sp2)
nrow(l3f)
#set proj
proj4string(sp2) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad

mapview(sp2,col.regions=pal,zcol="Quadrant")

### visual validation ready. Data set clean
length(which(l3f$x_new!=999))/nrow(l3f)
length(which(l3f$x_new==999))
which(l3f$x_new==999)
#l3f[which(l3f$x_new==999),]

# write  csv
write.csv(l3f,file.path(out,"/Level_3/ADV_level_3.csv"),row.names = F)
