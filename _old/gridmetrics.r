#'@title
#'  summarize lidar for raster pixels
#'
#'@description
#'  summarize lidar for raster pixels
#'
#'@details
#'  summarize lidar for raster pixels
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param las_files list of paths to las files
#'@param dtm_files optional list of paths to dtms
#'@param dtm_folder optional folder of dtms
#'@param dtm_ext what is the file extension of dtms
#'@param intersect_only only provide outputs over areas in which lidar and dtms intersect?
#'@param fns list of functions that will be used to generate columns of summary statistics from the las data
#'@param xmin (optional) force extent of analysis
#'@param xmax (optional) force extent of analysis
#'@param ymin (optional) force extent of analysis
#'@param ymax (optional) force extent of analysis
#'@param res resolution of processing analysis in units of provided lidar / dtm
#'@param grid pre-defined raster for analysis - lidar metrics are computed for each cell
#'
#'@return
#'  optional dataframe of xy coordinates for cell centers and additional columns of metrics
#'
#'@examples
#' dtms1="C:\\temp\\dtm_test\\"
#' las1="C:\\temp\\lidar_test\\"
#' las_files=list.files(las1,pattern="[.]las",full.names=T)
#' dtm_files=list.files(dtms1,pattern="[.]dtm",full.names=T)
#' metrics=gridmetrics(las_files=las_files,dtm_files=dtm_files)
#' head(metrics)
#'
#'@import raster plyr data.table rgeos lidR DBI
#'
#'@export
#'@seealso \code{\link{read_las}}\cr \code{\link{read_dtm}}\cr


gridmetrics=function(

  las_files
  ,dtm_files=NA
  ,dtm_folder=NA
  ,dtm_ext=".dtm"
  ,no_dtm=F
  ,fun=compute_metrics2 #list(min=min,max=max,mean=mean,sd=sd,p20=function(x,...)quantile(x,.2,...))
  ,xmin=NA
  ,xmax=NA
  ,ymin=NA
  ,ymax=NA
  ,res=66

  ,grid=NA

  ,n_read=NA

  ,out_name=NA
  ,return=F
  ,con=NA
  ,out_table=NA
  ,...

){
  require(raster)
  require(plyr)
  require(rgeos)
  require(lidR)
  require(data.table)

  #get / merge las data
  las_in=readLAS(las_files)@data

  #get / merge dtms
  if(!no_dtm){

    if(is.na(dtm_files[1]) & is.na(dtm_folder)) stop ("dtm paths or folders not provided")

    if(tolower(dtm_ext)==".dtm"){
      dtms=read_dtm(dtm_files,dtm_folder)
    }

    if(tolower(dtm_ext)!=".dtm"){
      if(is.na(dtm_files[1])) dtm_files=list.files(dtm_folder,pattern=dtm_ext,full.names=T)
      dtms=lapply(dtms,raster)
    }

    print("mosaic dtms")
    #mosaic dtms
    if(class(dtms)=="list"){
      if(length(dtms)>1){
        mos_dtm=do.call(function(x,y,...,fun=max)mosaic(x,y,...,fun=max),dtms)
      }else{
      mos_dtm=dtms[[1]]}
    }
    if(!class(dtms)=="list") mos_dtm=dtms
  }

  #clean up
  rm(list=c("dtms"));gc()

  #mos_dtm extent for later use
  mos_dtm_ext=extent(mos_dtm)

  #difference points and rasters
  las_in[,"be"]=raster::extract(mos_dtm,data.frame(las_in[,c("X","Y")]))
  rm(list=c("mos_dtm"));gc()
  las_in[,"ht"]=las_in$Z-las_in$be

  #create processing grid
  if(inherits(grid,"raster")) r0=grid
  if(!inherits(grid,"raster")){
    extlas=extent(apply(las_in[,c("X","Y")],2,function(x)c(min(x),max(x))))
    r0=raster(extlas,resolution=res)
  }

  #rasterize
  print("rasterize")
  las_in[,"cell"] <- cellFromXY(r0, data.frame(las_in[,c("X","Y")]))
  xyz=las_in[,compute_metrics2(z=.SD$Z,x=.SD$X,y=.SD$Y,as_list=F),by=cell,.SDcols=c("X","Y","Z")]

  #write / return data
  if(!is.na(out_table)) if(!(out_table =="")) try(dbWriteTable(con,out_table,xyz))
  if(!is.na(out_name)) try(write.csv(xyz,out_name,row.names=F))
  if(return) return(xyz)
  rm(list=ls());gc()

}

 compute_metrics2 = function(
    x,y,z
    ,ht_brk=c(6,3,seq(10,100,20))
    ,outliers=c(-6,400)
    ,elev_metrics=F         #adjust for the fact that heights aren't provided - offset by 5th percentile height
    ,vol_res=seq(5,100,20)
    ,as_list=T
    ){

    #make data convenient
    # z=lidar@data$Z
    # x=lidar@data$X
    # y=lidar@data$Y

    #adjust for missing dtm
    if(elev_metrics){
      z = z - quantile(z,.05)
    }

    #filter
    if(!is.na(outliers[1])) id_in=z>outliers[1] & z<outliers[2]
    else id_in=T

    z_in=z[id_in]
    x_in=x[id_in]
    y_in=y[id_in]

    id_brk=z_in>ht_brk[1]
    z_brk=z_in[id_brk]
    x_brk=x_in[id_brk]
    y_brk=y_in[id_brk]

    metrics_in = data.frame(
      n_all=length(z)
      ,n_in=length(z_in)
      ,n_lt_brk=length(z_in[!id_brk])
      ,n_gt_brk=length(z_brk)

      ,zmin   = min(z_brk,na.rm=T)
      ,zmax   = max(z_brk,na.rm=T)
      ,zmean   = mean(z_brk,na.rm=T)
      ,zsqmean = sqrt(mean(z^2,na.rm=T))  # Quadratic mean
      ,zsd   = sd(z_brk,na.rm=T)
      #,zcover = length(z_brk) / length(z_in)

      ,xsd = sd(x_brk,na.rm=T)
      ,ysd = sd(y_brk,na.rm=T)
      ,xysd= sd(sqrt(y_brk*x_brk),na.rm=T)
      ,xyzsd= sd((y_brk*x_brk*z_brk)^(1/3),na.rm=T)

    )

    #add cover
    nms_cov=sprintf("zcover_%03d",ht_brk)
    cov_pcts=1-(ecdf(z))(ht_brk)
    metrics_in[,nms_cov]=cov_pcts

    #add quantiles
    step=.1
    qts=c(step/2,seq(0+step,1-step,step),1-step/2)
    nms_zqts=sprintf("zqt%02d",qts*100)
    metrics_in[,nms_zqts]=quantile(z_brk,qts,na.rm=T)

    #add volume
    nms_vol=sprintf("vol%03d",vol_res)
    fn_vol=function(vol_res,x,y,z) length(unique(paste(round(x/vol_res,0),round(y/vol_res,0),round(z/vol_res,0),sep="_")))*(vol_res^3)
    metrics_in[,nms_vol]=sapply(vol_res,fn_vol,x_brk,y_brk,z_brk)

    #add area - use same resolution as volume
    nms_area=sprintf("area%03d",vol_res)
    fn_area=function(area_res,x,y) length(unique(paste(round(x/area_res,0),round(y/area_res,0),sep="_")))*(area_res^2)
    metrics_in[,nms_area]=sapply(vol_res,fn_area,x_brk,y_brk)

    #add density metrics
    nms_area=sprintf("area%03d",vol_res)
    fn_area=function(area_res,x,y) length(unique(paste(round(x/area_res,0),round(y/area_res,0),sep="_")))*(area_res^2)
    metrics_in[,nms_area]=sapply(vol_res,fn_area,x_brk,y_brk)

    if(!is.na(con)){
      require(DBI)
      dbWriteTable(con,out_table, metrics_in)
    }
    if(!as_list) return(unlist(metrics_in))
    else return(metrics_in)

  }
