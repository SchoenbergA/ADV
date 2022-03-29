
# Automated check for best reuslt in extraction of places using buffer for hessia
# bnufer requires projection !

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
# load and prepare data ####

# wenker places from shapefile
wen_wgs <- rgdal::readOGR(file.path(org,"Wenker_places_wgs.shp"))
wen <- spTransform(wen_wgs,CRS("+init=epsg:25832"))

# add geometry to wenker
geo <- geom(wen)
# write UTM geometry
wen$utm_x <- geo[,2]
wen$utm_y <- geo[,3]

head(wen)
wen_shp <-wen

#alternative #################################################################################
wen <- read.table(file.path(org,"alle_wenkerorte.txt"),sep="\t",fileEncoding = "utf-8",header = T)
head(wen)
which(colnames(wen)=="rede_name") 
names(wen)[3] <-"ort"

# weitere alternatve durch type name
#which(colnames(wen)=="org_name") 
#names(wen)[7] <-"ort"
class(wen)
head(wen)

wen$x <- as.numeric(wen$x)
wen$y <- as.numeric(wen$y)

which(is.na(wen$x))
wen[46991,]
wen <- wen[which(is.na(wen$x)==F),]
wenp <- SpatialPointsDataFrame(wen[,which(colnames(wen)=="x"):which(colnames(wen)=="y")],wen)
proj4string(wenp) <- "+proj=longlat +datum=WGS84 +no_defs"
#plot(wenp)
wenp
# project to utm32
wenutm <- spTransform(wenp,"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#plot(wenutm)

wenutm
# add geometry to wenker
geo <- geom(wenutm)
# write UTM geometry
wenutm$utm_x <- geo[,2]
wenutm$utm_y <- geo[,3]

head(wenutm)
####################################################################################################

# load hessen masks
mhe_wgs <- rgdal::readOGR(file.path(map,"Mask_Hessen.shp"))
mask <- spTransform(mhe_wgs,CRS("+init=epsg:25832"))

# load ADV table
# adv as xslx
adv <-openxlsx::read.xlsx(xlsxFile =file.path(org,"ADV_Ortstabelle_Hessen.xlsx"))

# add columns for coordinates and results
adv$x <-999
adv$y <-999
adv$res <- "no res"
head(adv)


## 1 Core function ####################################################################
# function to receive coordinates from wenker places if "ortsname" is equal to adv
extrt_wen_cords <- function(adv,wen,buffer,nobj){  
  for(i in 1:nrow(adv)){
    # check if ortsname adv is in Wenkerorte
    if(adv$Ortsname[i]%in%wen$ort==T){
      
      if(length(which(wen$ort==adv$Ortsname[i]))>1){
        #cat(paste0("multiple entries for ",adv$Ortsname[i]," detected. Skipping",sep="\n"))
        adv$res[i] <- "multiple entires detected"
      } else {
        # write coords from wenker to adv
        adv$res[i] <- "detected"
        adv$x[i] <- wen$utm_x[which(wen$ort==adv$Ortsname[i])]
        adv$y[i] <- wen$utm_y[which(wen$ort==adv$Ortsname[i])]
      }
    } else {
      #cat(paste0("adv place ",adv$Ortsname[i], " not detected in Wenker Places",sep="\n"))
      adv$res[i] <- "not detected"
    }
  }
  # adv to spatialpoints
  
  # trim all rows without coordinates
  advc <-subset(adv,adv$x!=999)
  advnew <-subset(adv,adv$x==999)
  # convert to spatial object
  if(nrow(advc>0)){
  advp <- SpatialPointsDataFrame(advc[,which(colnames(advc)=="x"):which(colnames(advc)=="y")],advc)
  plot(advp)
  # project to utm32
  proj4string(advp) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    advp <- "missing"
  }
  # Print results
  cat(" ",sep="\n")
  cat("##############################################",sep="\n")
  cat(paste0("Total ADV places ",nobj),sep="\n")
  cat(paste0(length(which(adv$res=="detected"))," detected (",round(length(which(adv$res=="detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(adv$res=="multiple entires detected"))," multiple entires detected (",round(length(which(adv$res=="multiple entires detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(adv$res=="not detected"))," not detected (",round(length(which(adv$res=="not detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  # Store results
  res <- c(paste0(buffer),
           length(which(adv$res=="detected")),
           length(which(adv$res=="multiple entires detected")),
           length(which(adv$res=="not detected")),
           (length(which(adv$res=="multiple entires detected"))+length(which(adv$res=="not detected"))),
           format(round(length(which(adv$res=="detected"))/nobj*100,digits = 2),nsmall = 2),
           format(round(length(which(adv$res=="multiple entires detected"))/nobj*100,digits = 2),nsmall = 2),
           format(round(length(which(adv$res=="not detected"))/nobj*100,digits = 2),nsmall = 2),
           as.numeric(format(round(length(which(adv$res=="multiple entires detected"))/nobj*100,digits = 2),nsmall = 2))+
           as.numeric(format(round(length(which(adv$res=="not detected"))/nobj*100,digits = 2),nsmall = 2)),
           100-(as.numeric(format(round(length(which(adv$res=="multiple entires detected"))/nobj*100,digits = 2),nsmall = 2))+
                  as.numeric(format(round(length(which(adv$res=="not detected"))/nobj*100,digits = 2),nsmall = 2))))
  
  # return
  res <- as.numeric(res)
  res_ls <-list("res"=res,"adv"=advp,"new"=advnew)
  return(res_ls)
}# end function #######################################################################
## 2 wrapper #########################################################################
wenCordsBuffer <- function(adv,wen,mask,buf_list){
  nobj <-nrow(adv)
# loop for buffer
  # compute list obj
  ls_df <- vector("list", length(buf_list))
  for (j in 1:length(buf_list)) {
    cat(" ",sep="\n")
  cat(paste0("Start ",j," / ",length(buf_list)),sep="\n")
  # compute buffer
  buf <-gBuffer(mask,byid = T,width = buf_list[j]*1000)
  # plot to see expoanding buffer
  plot(buf,axes=F,las=2,ylim=c(5200000,6000000),xlim=c(200000,800000))
  # crop point dataset to buffer
  buf_wen <- crop(wen,buf)
  
  # run function and store results
  if(j==1){
  ls2 <-extrt_wen_cords(adv,buf_wen,buffer=buf_list[j],nobj)
  ls3 <-ls2$res
  ls_df[[j]] <-ls2$adv
  new <- ls2$new
  } else {
    ls <-extrt_wen_cords(new,buf_wen,buffer=buf_list[j],nobj)
    new <- ls$new
    ls3 <- rbind(ls3,ls$res)
    ls_df[[j]] <-ls$adv
  }
  
}# end loop j
  # return
  colnames(ls3)<-c("Buffer","detected","multiple matches","not detected","total not detected","detected in %","multiple matches in %","not detected in %","total not detected in %","Total detected in %")
  rownames(ls3)<-1:nrow(ls3)
  ls3 <- as.data.frame(ls3)
  str(ls3)
  cat(" ",sep="\n")
  cat(sum(ls3$detected)/nrow(adv),sep="\n")
  cat("##############################################",sep="\n")
  cat("Finished",sep="\n")
  cat("##############################################",sep="\n")
  return(list("res"=ls3,"adv"=ls_df,"rest"=new))
}# end function #######################################################################
#  3 plot results ######################################################################
plotRES <- function(res){
 # res$total_detected <- 100 -as.numeric(res$`total not detected in %`)
  plot(res$Buffer,res$`Total detected in %`,xaxs="i",ylim=c(0,100),
       main=(paste0("best fit @buffer: ",res$Buffer[which(res$`Total detected in %`==max(res$`Total detected in %`))[1]], " with ",max(res$`Total detected in %`),"% detected")))
  lines(res$Buffer,res$`Total detected in %`,col="green",lwd=3)
  lines(res$Buffer,res$`not detected in %`,col="gray",lwd=3)
  lines(res$Buffer,res$`multiple matches in %`,col="blue",lwd=3)
  lines(res$Buffer,res2$`not detected in %`,col="red",lwd=3)
  abline(v=res$Buffer[which(res$`Total detected in %`==max(res$`Total detected in %`))],col="darkgreen",lwd=3)
  
  
  legend("topright", legend=c("detected", "total not assigned","multiple matches","not detected","best fit"),
         col=c("green","grey","blue","red","darkgreen"), lty=1, cex=0.8,lwd=3)
} # end of function ###################################################################


# run
wcb <-wenCordsBuffer(adv = adv,wen = wen_shp,mask = mask,buf_list =seq(0,25,1)) #0.6490952
wcb <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,25,5)) #0.7261998  923
# best with wenker from tsv (wenker utm)

# fine tuning
wcb1 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,25,1))   #  0.7269866
wcb2 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,25,0.5))  # 0.7269866
wcb3 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,25,0.25)) # 0.7269866
wcb4 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,25,25))  #  0.7191188
wcb4 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(25,50,25))  #  0.7191188
wcb4 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(300,400,100)) 
wcb4 <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,0.001,0.001)) 
wcb <-wenCordsBuffer(adv = adv,wen = wenutm,mask = mask,buf_list =seq(0,25,1))

############################
res1 <- wcb1$res 
res2 <- wcb2$res 
res3 <- wcb3$res 
res4 <- wcb4$res 
sum(res1$detected)/nrow(adv)
sum(res1$detected)
sum(res2$detected)/nrow(adv)
sum(res2$detected)
sum(res3$detected)/nrow(adv)
sum(res3$detected)
###############################

# unlist
advls <-wcb$adv
which(advls!="missing")
advls <- advls[which(advls!="missing")]
res2 <- wcb$res 
rest <- wcb$rest

# get total detetced result
sum(res2$detected)/nrow(adv)
sum(res2$detected)
# plot results
plotRES(res2)

# mapview all pointslayer
pal = mapviewPalette("mapviewTopoColors")
mapview(advls,col.regions = pal(length(advls)))

# merge all points and rest

# rbind all obj in list
all_pt <-as.data.frame(do.call("rbind",advls))
head(all_pt)
# clean unneded cols (checken woher die bei as.spatial kommen)
all_pt_c <-all_pt[,1:(ncol(all_pt)-2)]
head(all_pt_c)

# rbind letzte nicht treffer aus loop
all_rest <-rbind(all_pt_c,rest)
View(all_rest)

# check if equal to original
all.equal(all_rest,adv)

all.equal(length(unique(all_rest$Ortsname)),length(unique(adv$Ortsname))   )
all.equal(table(all_rest$Ortsname),table(adv$Ortsname)) #TRUE, just other sorted!

# get spatial object from table
all_sp <- SpatialPointsDataFrame(all_rest[,13:14],all_rest)
any(all_sp$x==999)
# delete rows without coordinates
all_sp_c <-subset(all_sp,all_sp$x!=999)
#set proj
proj4string(all_sp_c) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad
mapview(all_sp_c,zcol="Quadrant")
mapview(all_sp_c,zcol="Teilquadrant")

# output

# write full data table
write.csv(all_rest,file.path(out,"ADV_output_0_25_1.csv"))

# write shp for spatial validation
writeOGR(all_sp_c,file.path(out,"pt_0_25_1.shp"),driver="ESRI Shapefile",layer="val")
