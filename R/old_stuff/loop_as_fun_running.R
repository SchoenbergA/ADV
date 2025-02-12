
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

## 1 Core function ####################################################################

## 2 wrapper #########################################################################
GetCrdbyTab <- function(tab_in,tab_crd,crd_x=NULL,crd_y=NULL,mask,buf_list){
  ## load core function ####################################################################
  # function to receive coordinates from tab_crd places if "ortsname" is equal to tab_in
  extrt_tab_crd_cords <- function(tab_in,tab_crd,buffer,nobj){  
    for(i in 1:nrow(tab_in)){
      # check if ortsname tab_in is in tab_crdkerorte
      if(tab_in$Ortsname[i]%in%tab_crd$ort==T){
        
        if(length(which(tab_in$Ortsname==tab_in$Ortsname[i]))>1){
          tab_in$res[i] <- "not an unique name"
        } else {
          if(length(which(tab_crd$ort==tab_in$Ortsname[i]))>1){
            #cat(paste0("multiple entries for ",tab_in$Ortsname[i]," detected. Skipping",sep="\n"))
            tab_in$res[i] <- "multiple entires detected"
          } else {
            # write coords from tab_crdker to tab_in
            tab_in$res[i] <- "detected"
            tab_in$x[i] <- tab_crd$crd_x[which(tab_crd$ort==tab_in$Ortsname[i])]
            tab_in$y[i] <- tab_crd$crd_y[which(tab_crd$ort==tab_in$Ortsname[i])]
          }
        }
      } else {
        #cat(paste0("tab_in place ",tab_in$Ortsname[i], " not detected in tab_crdker Places",sep="\n"))
        tab_in$res[i] <- "not detected"
      }
    }
    # tab_in to spatialpoints
    
    # trim all rows without coordinates
    tab_inc <-subset(tab_in,tab_in$x!=999)
    tab_innew <-subset(tab_in,tab_in$x==999)
    # convert to spatial object
    if(nrow(tab_inc>0)){
      tab_inp <- SpatialPointsDataFrame(tab_inc[,which(colnames(tab_inc)=="x"):which(colnames(tab_inc)=="y")],tab_inc)
      plot(tab_inp)
      # project to utm32
      proj4string(tab_inp) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    } else {
      tab_inp <- "missing"
    }
    # Print results
    cat(" ",sep="\n")
    cat("##############################################",sep="\n")
    cat(paste0("Total tab_in places ",nobj),sep="\n")
    cat(paste0(length(which(tab_in$res=="detected"))," detected (",round(length(which(tab_in$res=="detected"))/nobj*100,digits = 2),"%)"),sep="\n")
    cat(paste0(length(which(tab_in$res=="multiple entires detected"))," multiple entires detected (",round(length(which(tab_in$res=="multiple entires detected"))/nobj*100,digits = 2),"%)"),sep="\n")
    cat(paste0(length(which(tab_in$res=="not an unique name"))," not an unique name (",round(length(which(tab_in$res=="not an unique name"))/nobj*100,digits = 2),"%)"),sep="\n")
    cat(paste0(length(which(tab_in$res=="not detected"))," not detected (",round(length(which(tab_in$res=="not detected"))/nobj*100,digits = 2),"%)"),sep="\n")
    # Store results
    res <- c(paste0(buffer),
             ### absolute values
             length(which(tab_in$res=="detected")),                   # detected              
             length(which(tab_in$res=="not an unique name")),         # not unqiue
             length(which(tab_in$res=="multiple entires detected")),  # multis
             length(which(tab_in$res=="not detected")),               # not detected
             (length(which(tab_in$res=="multiple entires detected"))+length(which(tab_in$res=="not detected"))+length(which(tab_in$res=="not an unique name"))),
             ### in percent
             format(round(length(which(tab_in$res=="detected"))/nobj*100,digits = 2),nsmall = 2),
             format(round(length(which(tab_in$res=="not an unique name"))/nobj*100,digits = 2),nsmall = 2),
             format(round(length(which(tab_in$res=="multiple entires detected"))/nobj*100,digits = 2),nsmall = 2),
             format(round(length(which(tab_in$res=="not detected"))/nobj*100,digits = 2),nsmall = 2),
             
             as.numeric(format(round(length(which(tab_in$res=="multiple entires detected"))/nobj*100,digits = 2),nsmall = 2))+
               as.numeric(format(round(length(which(tab_in$res=="not detected"))/nobj*100,digits = 2),nsmall = 2))+
               as.numeric(format(round(length(which(tab_in$res=="not an unique name"))/nobj*100,digits = 2),nsmall = 2)),
             
             100-(as.numeric(format(round(length(which(tab_in$res=="multiple entires detected"))/nobj*100,digits = 2),nsmall = 2))+
                    as.numeric(format(round(length(which(tab_in$res=="not detected"))/nobj*100,digits = 2),nsmall = 2))+
                    as.numeric(format(round(length(which(tab_in$res=="not an unique name"))/nobj*100,digits = 2),nsmall = 2)))    )
    
    # return
    res <- as.numeric(res)
    res_ls <-list("res"=res,"tab_in"=tab_inp,"new"=tab_innew)
    return(res_ls)
  }# end function 
  
  # set column names to access via $ by variable
  names(tab_crd)[which(names(tab_crd)==crd_x)] <-"crd_x"
  names(tab_crd)[which(names(tab_crd)==crd_y)] <-"crd_y"
  
  # add dummy calumns to tab_in
  tab_in$x <-999
  tab_in$y <-999
  tab_in$res <- "no res"
  
  # calculate amount of objects in tab_in for % calculation
  nobj <-nrow(tab_in)
  plot(nobj)
  
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
    buf_tab_crd <- crop(tab_crd,buf)
    
    # run function and store results
    if(j==1){
      ls2 <-extrt_tab_crd_cords(tab_in,buf_tab_crd,buffer=buf_list[j],nobj)
      ls3 <-ls2$res
      ls_df[[j]] <-ls2$tab_in
      new <- ls2$new
    } else {
      ls <-extrt_tab_crd_cords(new,buf_tab_crd,buffer=buf_list[j],nobj)
      new <- ls$new
      ls3 <- rbind(ls3,ls$res)
      ls_df[[j]] <-ls$tab_in
    }
    
  }# end loop j
  
  # return
  colnames(ls3)<-c("Buffer","detected","not an unique name","multiple matches","not detected","total not detected","detected in %","no unqie in %","multiple matches in %","not detected in %","total not detected in %","Total detected in %")
  rownames(ls3)<-1:nrow(ls3)
  ls3 <- as.data.frame(ls3)
  
  cat(" ",sep="\n")
  cat("Total detected in % ",sep="\n")
  cat(sum(ls3$detected)/nrow(tab_in),sep="\n")
  cat("##############################################",sep="\n")
  cat("Finished",sep="\n")
  cat("##############################################",sep="\n")
  return(list("res"=ls3,"ls_sP"=ls_df,"rest"=new))
}# end function #######################################################################

### load and preapre data #######################
# load hessen masks
mhe_wgs <- rgdal::readOGR(file.path(map,"Mask_Hessen.shp"))
maskh <- spTransform(mhe_wgs,CRS("+init=epsg:25832"))

# load ADV org
adv <-openxlsx::read.xlsx(xlsxFile =file.path(org,"ADV_Ortstabelle_Hessen.xlsx"))

# load coordinates table via WENKER shp
# wenker places from shapefile 
wen_wgs <- rgdal::readOGR(file.path(org,"Wenker_places_wgs.shp"))
wen <- spTransform(wen_wgs,CRS("+init=epsg:25832"))
# add geometry to wenker
geo <- geom(wen)
# write UTM geometry
wen$wenutm_x <- geo[,2]
wen$wenutm_y <- geo[,3]
head(wen)
###
# wenker by tsv
wen2 <- read.table(file.path(org,"alle_wenkerorte.txt"),sep="\t",fileEncoding = "utf-8",header = T)
head(wen2)
which(colnames(wen2)=="rede_name") 
names(wen2)[3] <-"ort"

# weitere alternatve durch type name
#which(colnames(wen)=="org_name") 
#names(wen)[7] <-"ort"
class(wen2)
head(wen2)

wen2$x <- as.numeric(wen2$x)
wen2$y <- as.numeric(wen2$y)

which(is.na(wen2$x))
wen2[46991,]
wen2 <- wen2[which(is.na(wen2$x)==F),]
wenp <- SpatialPointsDataFrame(wen2[,which(colnames(wen2)=="x"):which(colnames(wen2)=="y")],wen2)
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
###
# # load osm
osm <- rgdal::readOGR(file.path(org,"hessen-latest-free.shp/gis_osm_places_free_1.shp"),encoding = "UTF-8")
osm2 <- spTransform(osm,CRS("+init=epsg:25832"))

head(osm2)
names(osm2)[5] <- "ort"

geo <- geom(osm2)
osm2$utm_x <- geo[,2]
osm2$utm_y <- geo[,3]

which(is.na(osm2$ort))
osm2 <- osm2[which(is.na(osm2$ort)==F),]

head(osm2)
####################################################################################

### run full ####

### 1. Run with Wenker only 
names(wenutm)
wen_run <-GetCrdbyTab(tab_in = adv,tab_crd = wenutm,crd_x ="utm_x" ,crd_y = "utm_y",mask = maskh,buf_list = seq(0,25,1))

# unlist results and rest
res1 <- wen_run$res
rest1 <- wen_run$rest

# unlist result points
pts1 <- wen_run$ls_sP
any(pts1=="missing")
pts1 <- pts1[which(pts1!="missing")]

# rbind all obj in list
all_pts1 <-as.data.frame(do.call("rbind",pts1))
head(all_pts1)
# clean unneded cols (checken woher die bei as.spatial kommen)
all_pts_c1 <-all_pts1[,1:(ncol(all_pts1)-2)]
head(all_pts_c1)

# rbind points from first run and rest
all_rest1 <-rbind(all_pts_c1,rest1)
View(all_rest1)

# check if equal to original
all.equal(all_rest1,adv)

all.equal(length(unique(all_rest1$Ortsname)),length(unique(adv$Ortsname))   )
all.equal(table(all_rest1$Ortsname),table(adv$Ortsname)) #TRUE, just other sorted!

# get spatial object from table
names(all_rest1[,13:14])
all_sp1 <- SpatialPointsDataFrame(all_rest1[,13:14],all_rest1)
any(all_sp1$x==999)
# delete rows without coordinates
all_sp_c1 <-subset(all_sp1,all_sp1$x!=999)
#set proj
proj4string(all_sp_c1) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad
mapview(all_sp_c1,zcol="Quadrant")
mapview(all_sp_c1,zcol="Teilquadrant")

# output

# write full data table
write.csv(all_rest1,file.path(out,"ADV_Result_v2.csv"))

# write shp for spatial validation
writeOGR(all_sp_c1,file.path(out,"ADV_Result_v2.shp"),driver="ESRI Shapefile",layer="ADV_Result_v2")


### 2. Run with Wenker and OSM in second run only ####


# add results from first run for compare to rest1
rest1$old <- rest1$res

# run rest from wenker with osm (using minimal buffer due to only data for hessen)
sec_run <-GetCrdbyTab(tab_in = rest1,tab_crd = osm2,crd_x ="utm_x" ,crd_y = "utm_y",mask = maskh,buf_list = c(0,0.5,1))
res2 <-sec_run$res

# unlist results and rest
res2 <- sec_run$res
rest2 <- sec_run$rest

# unlist result points
pts2 <- sec_run$ls_sP
any(pts2=="missing")
pts2 <- pts2[which(pts2!="missing")]

# merge all points and rest for second run ####


# add "old" column for first run results
all_pts_c1$old <- " first run "

# rbind all obj in list for second run
all_pts2 <-as.data.frame(do.call("rbind",pts2))
head(all_pts2)
# clean unneded cols (checken woher die bei as.spatial kommen)
all_pts2_c <-all_pts2[,1:(ncol(all_pts2)-2)]
head(all_pts2_c)

# get rest of second run and add "old" column
rest2 <-sec_run$rest

# rbind results of first and second run plus rest of second run
all_rest2 <-rbind(all_pts_c1,all_pts2_c,rest2)
View(all_rest2)

# check if equal to original
all.equal(all_rest2,adv)

all.equal(length(unique(all_rest2$Ortsname)),length(unique(adv$Ortsname))   )
all.equal(table(all_rest2$Ortsname),table(adv$Ortsname)) #TRUE, just other sorted!

# get spatial object from table
names(all_rest2[,13:14])
all_sp2 <- SpatialPointsDataFrame(all_rest2[,13:14],all_rest2)
any(all_sp2$x==999)

# delete rows without coordinates
all_sp_c2 <-subset(all_sp2,all_sp2$x!=999)
#set proj
proj4string(all_sp_c2) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot validation by quandrant or teilquad
mapview(all_sp_c2,zcol="Quadrant")
mapview(all_sp_c2,zcol="Teilquadrant")

unique(all_sp2$res)
length(which(all_sp2$res=="detected"))/nrow(adv) # 0.7356412
length(which(all_sp2$res=="not detected"))/nrow(adv) # 0.1652242
length(which(all_sp2$res=="multiple entires detected"))/nrow(adv) # 0.03619197
length(which(all_sp2$res=="not an unique name"))/nrow(adv) # 0.06294256

unique(all_sp1$res)
length(which(all_sp1$res=="detected"))/nrow(adv) # 0.7010228
length(which(all_sp1$res=="not detected"))/nrow(adv) # 0.1888277
length(which(all_sp1$res=="multiple entires detected"))/nrow(adv) # 0.04248623
length(which(all_sp1$res=="not an unique name"))/nrow(adv) # 0.06766326 (those lesser value in sp2 comes from the fact, that those names were not detected in osm)

# output

# write full data table
write.csv(all_rest2,file.path(out,"ADV_Result_v3.csv"))

# write shp for spatial validation
writeOGR(all_sp_c2,file.path(out,"ADV_Result_v3.shp"),driver="ESRI Shapefile",layer="ADV_Result_v3")
