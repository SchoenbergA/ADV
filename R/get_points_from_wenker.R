# setup ####

# load packages
require(openxlsx)
require(geojsonR)
require(rgdal)
require(raster)

# set up folders
wd <-getwd()
org <- file.path(wd,"Data/org/")
out <- file.path(wd,"Data/output")

# load data ####
# avd as xslx
avd <-openxlsx::read.xlsx(xlsxFile =file.path(org,"ADV_Ortstabelle_Hessen.xlsx"))
# wenker places from shapefile
wen <- rgdal::readOGR(file.path(org,"Wenker_places_wgs.shp"))

# prepare data ####

# add geometry to wenker
geo <- geom(wen)
# write UTM geometry
wen$x <- geo[,2]
wen$y <- geo[,3]

# add columns for coordinates and results
avd$x <-999
avd$y <-999
avd$res <- "no res"
head(avd)

# test the logic
# any(wen$ort %in% avd$Ortsname)
#which(wen$ort %in% avd$Ortsname)
# 
#wen$ort[1074]
#which(avd$Ortsname==wen$ort[1074])
#avd$Ortsname[852]#
#
#avd$Ortsname[1]%in%wen$ort
#which(wen$ort==avd$Ortsname[10])
#wen$x[which(wen$ort==avd$Ortsname[10])]

#avd$x[1] <- 888


# function to receive coordinates from wenker places if "ortsname" is equal to adv
  for(i in 1:nrow(avd)){
  # check if ortsname avd is in Wenkerorte
  if(avd$Ortsname[i]%in%wen$ort==T){
    
    if(length(which(wen$ort==avd$Ortsname[i]))>1){
      cat(paste0("multiple entries for ",avd$Ortsname[i]," detected. Skipping",sep="\n"))
      avd$res[i] <- "multiple entires detected"
    } else {
    # write coords from wenker to avd
    avd$res[i] <- "detected"
    avd$x[i] <- wen$x[which(wen$ort==avd$Ortsname[i])]
    avd$y[i] <- wen$y[which(wen$ort==avd$Ortsname[i])]
    }
  } else {
    cat(paste0("AVD place ",avd$Ortsname[i], " not detected in Wenker Places",sep="\n"))
    avd$res[i] <- "not detected"
  }
  }

head(avd)

# results
nrow(avd)
length(which(avd$res=="detected"))
length(which(avd$res=="multiple entires detected"))
length(which(avd$res=="not detected"))

# write to shp
# subset only places which have coordinates
avd_dec <- subset(avd,avd$res=="detected")
# convert to spatial points
wgs <- SpatialPointsDataFrame(coords = avd_dec[,13:14],avd_dec)
# set crs
proj4string(wgs) <- "+proj=longlat +datum=WGS84 +no_defs"

plot(wgs)
# write
head(wgs)
writeOGR(wgs,file.path(out,"adv_wgs2.shp"),driver = "ESRI Shapefile",layer="adv_wgs") 
# warning column names will be shortend due to ersi format

### example for mulitple
which(wen$ort==avd$Ortsname[10])
as.data.frame(wen[which(wen$ort==avd$Ortsname[10]),])
# solution: extract "bundesland and kreis" admin levels for wenker and reduce search area on hessen.
