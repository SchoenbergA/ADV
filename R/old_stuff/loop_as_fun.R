
in_tab=adv
cord_tab <- as.data.frame(wen)

in_col = "Ortsname"
cord_col = "ort"
cord_x = "utm_x"
cord_y= "utm_y"
cord_proj = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
nobj = 1271
buffer=2019


# out of loops prepare data
in_tab$x <- 999
in_tab$y <- 999
in_tab$res <- "no_res"
test <-extrct_coords(in_tab = adv,in_col = "Ortsname",cord_tab = cord_tab,cord_col = "ort",cord_x = "utm_x",
              cord_y = "utm_y",cord_proj ="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
              nobj = 1271,buffer=2019)
res <-test$res
res
####################################################################################
teat2 <- Buff_extrct_coords(in_tab = adv,in_col = "Ortsname",cord_tab = (wen),cord_col = "ort",cord_x = "utm_x",
                   cord_y = "utm_y",cord_proj ="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                   buf_list = c(1,5,10),
                   mask=mask)

## 1 Core function ####################################################################
# function to receive coordinates for an input table with names of places by a given dataset with known coordinates and names 
extrct_coords <- function(in_tab,in_col,cord_tab,cord_col,cord_x,cord_y,cord_proj,buffer,nobj){  
  # get columns
  col_in <- which(colnames(in_tab)==in_col)
  col_cord <- which(colnames(as.data.frame(cord_tab))==cord_col)
  col_x <- which(colnames(as.data.frame(cord_tab))==cord_x)
  col_y <- which(colnames(as.data.frame(cord_tab))==cord_y)
  
  # loop over all rows in input table
   for(i in 1:nrow(in_tab)){
print(i)
    # check if input place is in coordinate table
      if(in_tab[,col_in][i]%in%as.data.frame(cord_tab[,col_cord])==T){
        # check if more than exact one entires matches
        if (length(which(as.data.frame(cord_tab[,col_cord])==in_tab[,col_in][i]))>1){
          in_tab$res[i] <- "multiple_entires_detected"
        } else {
          # write coordinates to input table
          in_tab$res[i] <- "detected"
          in_tab$x[i] <- as.data.frame(cord_tab)[which(as.data.frame(cord_tab[,col_cord])==in_tab[,col_in][i]),cord_x]
          in_tab$y[i] <- as.data.frame(cord_tab)[which(as.data.frame(cord_tab[,col_cord])==in_tab[,col_in][i]),cord_y]
        }
      } else {
        in_tab$res[i] <- "not_detected"
      }

  } # end loop
  
  # get subset with detected places
  in_dec <-subset(in_tab,in_tab$x!=999)
  # get subset with places not detected or with multiple matches
  in_new <-subset(in_tab,in_tab$x==999)

  # convert detected places to spatial object (will be stored in list)
  if(nrow(in_dec>0)){
    in_dec_p <- SpatialPointsDataFrame(in_dec[,which(colnames(in_dec)=="x"):which(colnames(in_dec)=="y")],in_dec)
    plot(in_dec_p)
    # project to utm32
    proj4string(in_dec_p) <- cord_proj
  } else {
    in_dec_p <- "missing" # for storing to list in the loop
  }
  # Print results
  cat(" ",sep="\n")
  cat("##############################################",sep="\n")
  cat(paste0("Total Places to Detect ",nobj),sep="\n")
  cat(paste0(length(which(in_tab$res=="detected"))," detected (",round(length(which(in_tab$res=="detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(in_tab$res=="multiple_entires_detected"))," multiple_entires_detected (",round(length(which(in_tab$res=="multiple_entires_detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  cat(paste0(length(which(in_tab$res=="not_detected"))," not_detected (",round(length(which(in_tab$res=="not_detected"))/nobj*100,digits = 2),"%)"),sep="\n")
  # Store results
  res <- c(paste0(buffer),
           length(which(in_tab$res=="detected")),
           length(which(in_tab$res=="multiple_entires_detected")),
           length(which(in_tab$res=="not_detected")),
           (length(which(in_tab$res=="multiple_entires_detected"))+length(which(in_tab$res=="not_detected"))),
           format(round(length(which(in_tab$res=="detected"))/nobj*100,digits = 2),nsmall = 2),
           format(round(length(which(in_tab$res=="multiple_entires_detected"))/nobj*100,digits = 2),nsmall = 2),
           format(round(length(which(in_tab$res=="not_detected"))/nobj*100,digits = 2),nsmall = 2),
           as.numeric(format(round(length(which(in_tab$res=="multiple_entires_detected"))/nobj*100,digits = 2),nsmall = 2))+
             as.numeric(format(round(length(which(in_tab$res=="not_detected"))/nobj*100,digits = 2),nsmall = 2)))
  
  # return
  res_ls <-list("res"=res,"p_dec"=in_dec,"new"=in_new)
  return(res_ls)
}# end function #######################################################################

## 2 wrapper #########################################################################

Buff_extrct_coords <- function(in_tab,in_col,cord_tab,cord_col,cord_x,cord_y,cord_proj,mask,buf_list){

  # get nrow of input table (for later calculation of results)
  nobj <-nrow(in_tab)

  # compute list obj (for saving results)
  ls_df <- vector("list", length(buf_list))
  
  # loop coordinate extraction by name over buffer
  for (j in 1:length(buf_list)) {
    cat(" ",sep="\n")
    cat(paste0("Start ",j," / ",length(buf_list)),sep="\n")
    # compute buffer for mask layer
    buf <-gBuffer(mask,byid = T,width = buf_list[j]*1000)
                # plot to see expoanding buffer (extemnt is hardcoded. Plot xy lim has problem with coordinates)
                plot(buf,axes=F,las=2,ylim=c(5200000,6000000),xlim=c(200000,800000))
    # crop point dataset to buffer
    buf_wen <- crop(cord_tab,buf)
    
    # run extrct_coords function and store results
    # first run with full input dataset
    if(j==1){
      ls2 <-extrct_coords(in_tab,in_col,cord_tab,cord_col,cord_x,cord_y,cord_proj,buffer=buf_list[j],nobj)
      ls3 <-ls2$res
      # store resulting spatial points to list
      ls_df[[j]] <-ls2$p_dec
      # get input dataset for next run (without detected entries)
      new <- ls2$new
      # further runs with only those entries of input dataset which are not detected in runs before.
    } else {
      ls <-extrct_coords(in_tab = new,in_col,cord_tab,cord_col,cord_x,cord_y,cord_proj,buffer=buf_list[j],nobj)
      # get input dataset for next run (without detected entries)
      new <- ls$new
      # store results
      ls3 <- rbind(ls3,ls$res)
      # store resulting spatial points to list
      ls_df[[j]] <-ls$p_dec
    }
    
  }# end loop j
  # return
  colnames(ls3)<-c("Buffer","detected","multiple matches","not detected","total not detected","detected in %","multiple matches in %","not detected in %","total not detected in %")
  rownames(ls3)<-1:nrow(ls3)
  ls3 <- as.data.frame(ls3)
  str(ls3)
  cat(" ",sep="\n")
  cat("##############################################",sep="\n")
  cat("Finished",sep="\n")
  cat("##############################################",sep="\n")
  return(list("results"=ls3,"list_detected"=ls_df,"not_detected_rest"=new))
}# end function #######################################################################
