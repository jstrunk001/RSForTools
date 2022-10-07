#'@title
#'  compute experimental lidar metrics
#'
#'@description
#'  take advantage of xy and 3d information
#'
#'@details
#'  <Delete and Replace>
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
#'1.0 \tab 4/4/2019 \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param x lidR object X vector : las1$X
#'@param y lidR object Y vector : las1$Y
#'@param z lidR object Z vector : las1$Z
#'@param i lidR object intensity vector : las1$Intensity
#'@param rn lidR object return vector : las1$ReturnNumber
#'@param cl lidR object return vector : las1$Classification
#'@param r lidR object red vector : las1$R
#'@param g lidR object green vector : las1$G
#'@param b lidR object blue vector : las1$B
#'@param resxy resolution to use when computing xy statistic - grid resolution
#'@param resxyz resolution to use when computing xyz statistic - voxel resolution
#'@param ressurf resolution to use when computing raster areas - grid resolution
#'@param htcover height to use when computing cover and threshold for eliminating ground returns
#'@param zthresh height thresholds to use for height ratios round(100*length(z[ z > zthresh[i]]) / length(z[ z > htcover]),2)
#'@param adjustz (optional) method used to shift z values up/down. When there is no DTM or the DTM is poor (e.g. when used with DAP) this can help
#'@param outlier a two value vector with minimum and maximum values used to restrict the range of z values considered
#'
#'@return
#'  list of statistics, currently only computed on x,y,z
#'
#'@examples
#'    if(!"las1" %in% ls()) las1 = readLAS("D:\\Box\\sync\\R\\analyses\\wa_dsm_env_fia\\data\\plot_clips_ht\\plot_44.laz")
#'    test2 = lasmetrics(las1,experimental_metrics(X,Y,Z))
#'    test2
#'
#@import lidR plyr raster
#'
#'@export
#
#'@seealso \code{\link{lidR::readLAS}}\cr \code{\link{lidR::lasmetrics}}\cr \code{\link{lidR::grid_metrics}}\cr

#Desired upgrades to this function:
#
#

experimental_metrics = function(
  x=NA
  ,y=NA
  ,z=NA
  ,i=NA
  ,rn=NA
  ,cl=NA
  ,r=NA
  ,g=NA
  ,b=NA
  ,resxy = 20
  ,resxyz = 20
  ,ressurf = 5
  ,htcover = 6
  ,zqts = c(1,5,10,20,30,40,50,60,70,80,90,95,99)
  ,zthresh = c(3,6,12,20)
  ,adjustz = c("none","positive","min0","zq01")
  ,outlier = c(min=NA,max=NA)
  ,stdmetrics = T
  ){

  #adjust z values to e.g. set the minimum value to zero
  #especially useful with DAP over a coarse ground model...
  if(!is.na(z[1]))if(!is.null(adjustz[1]))if(!is.na(adjustz[1])){
    if(adjustz[1]=="positive") if(min(z) < 0) z = z - min(z) + 1
    if(adjustz[1]=="min0") z = z - min(z)+.001
    if(adjustz[1]=="zq01") z = z - quantile(z,.01)
  }

  #remove outlier points
  if(!is.na(outlier[1])){
    outlier_ids = z < outlier[1] | z > outlier[2]
    z = z[!outlier_ids]
    if(!is.na(x[1])) x = x[!outlier_ids]
    if(!is.na(y[1])) y = y[!outlier_ids]
    if(!is.na(i[1])) i = i[!outlier_ids]
    if(!is.na(rn[1])) rn = rn[!outlier_ids]
    if(!is.na(cl[1])) cl = cl[!outlier_ids]
    if(!is.na(r[1])) r = r[!outlier_ids]
    if(!is.na(g[1])) g = g[!outlier_ids]
    if(!is.na(b[1])) b = b[!outlier_ids]

  }

  #begin metric computations
  mets_in = list()
  if(!is.na(x[1]) & !is.na(y[1]) & !is.na(z[1])){

    requireNamespace("plyr")
    requireNamespace("raster")

    x1 = x[z >= htcover]
    y1 = y[z >= htcover]
    z1 = z[z >= htcover]

    #enable non-fail if no points are above htcover
    if(length(x1)<5 | diff(range(x1))==0 ){

      x1 = seq(0,5,1)
      y1 = seq(0,5,1)
      z1 = seq(0,5,1)

      set_NA =T

    }else{

      set_NA = F
    }

    #compute various multiplicative metrics, x,y,z
    rtxy = (x*y)^(1/2)
    rtxy1 = (x1*y1)^(1/2)
    rtxyz = (x*y*z)^(1/3)
    rtxyz1 = (x1*y1*z1)^(1/3)
    xyz = data.frame(x = round(x/resxy) *  resxy, y = round(y/resxy) * resxy, z = z)
    xyz1 = data.frame(x = round(x1/resxy) *  resxy, y = round(y1/resxy) * resxy, z = z1)

    #compute various combinatorial metrics, used for area and volume
    combnsxy = plyr::count(data.frame(round(x/resxy), round(y/resxy)))
    combnsxy1 = plyr::count(data.frame(round(x1/resxy), round(y1/resxyz)))
    combnsxyz = plyr::count(data.frame(round(y/resxyz), round(x/resxyz), round(x/resxyz)))
    combnsxyz1 = plyr::count(data.frame(round(y1/resxyz), round(x1/resxyz), round(z1/resxyz)))

    #surface based metrics
    #make sure raster has at least 1 pixel, even if there is only one point
    r1 = terra::rast(
                    res=ressurf
                    ,xmin=floor(min(x))
                    ,xmax=floor(min(x)) + max(ressurf,ceiling((max(x)-min(x))/ressurf)*ressurf)
                    ,ymin=floor(min(y))
                    ,ymax=floor(min(y)) + max(ressurf,ceiling((max(y)-min(y))/ressurf)*ressurf)
                    )
    ch = terra::rasterize(x=cbind(x,y),y=r1,values=z,fun=function(x,...)quantile(x,.975,na.rm=T))
    chdf = as.data.frame(ch, xy=T)
    chdfsp = SpatialPixelsDataFrame(points=chdf[,1:2], data=chdf[,3,drop=F])

    #typical z metrics
    mets_in["zMin"] = min(z1,na.rm=T)
    mets_in["zMinRat"] = abs(round(100*(mets_in[["zMin"]] - min(z,na.rm=T)) / min(z,na.rm=T) ,2))
    mets_in["zMax"] = max(z1,na.rm=T)
    mets_in["zMaxRat"] = round(100*mets_in[["zMax"]] / max(z,na.rm=T) , 2)
    mets_in["zSd"] = sd(z1,na.rm=T)
    mets_in["zSdRat"] = round(100*mets_in[["zSd"]] / sd(z,na.rm=T) ,2)
    mets_in["zMean"] = mean(z1,na.rm=T)
    mets_in["zMeanRat"] = round(100*mets_in[["zMean"]] / mean(z,na.rm=T) ,2)
    mets_in["zCv"] = mets_in[["zSd"]]/ mets_in[["zMean"]]
    mets_in["zCvRat"] = round(100*mets_in[["zCv"]] / (sd(z,na.rm=T)/mean(z,na.rm=T) ),2)
    mets_in["zIQ"] = diff(range(z1,na.rm=T))
    mets_in["zIQRat"] = round(100*mets_in[["zIQ"]]/ diff(range(z,na.rm=T)),2)
    mets_in["zCover"] = round(100*length(z1)/ length(z),2)
    mets_in["zCoverHt"] = htcover
    mets_in[paste("zq",zqts,sep="")] = quantile(z1,zqts/100,na.rm=T)
    mets_in[paste("zRat",zthresh,sep="")] = lapply(zthresh,function(thr,z1)round(100*sum(z>thr,na.rm=T)/length(z1),2),z1)

    #add height band metrics between supplied height thresholds
    for(i in 1:(length(zthresh)-1)){
      nm_bnd_i = paste0("zBandHtRat_",zthresh[i],"_",zthresh[i+1])
      mets_in[nm_bnd_i] = round(100*sum( z1>=zthresh[i] & z1<zthresh[i+1] ) / length(z1), 2)
    }

    #x metrics
    mets_in["xMin"] = min(x1,na.rm=T)
    mets_in["xMax"] = max(x1,na.rm=T)
    mets_in["xSd"] = sd(x1,na.rm=T)
    mets_in["xRatHt"] = round(100*diff(range(x1,na.rm=T)) / diff(range(x,na.rm=T)),2)

    #y metrics
    mets_in["yMin"] = min(y1,na.rm=T)
    mets_in["yMax"] = max(y1,na.rm=T)
    mets_in["ySd"] = sd(y1,na.rm=T)
    mets_in["yRatHt"] = round(100*diff(range(y1,na.rm=T)) / diff(range(y,na.rm=T)),2)

    #xy metrics
    mets_in["xyMinRt"] = min(rtxy1,na.rm=T)
    mets_in["xyMinRtRat"] = round(100*mets_in[["xyMinRt"]] / min(rtxy,na.rm=T),2)
    mets_in["xyMaxRt"] = max(rtxy1,na.rm=T)
    mets_in["xyMaxRtRat"] = round(100*mets_in[["xyMaxRt"]] / max(rtxy,na.rm=T),2)
    mets_in["xySdRt"] = sd(rtxy1,na.rm=T)
    mets_in["xySdRtRat"] = round(100*mets_in[["xySdRt"]] / sd(rtxy,na.rm=T),2)
    mets_in["xyMnRt"] = mean(rtxy1,na.rm=T)
    mets_in["xyMnRtRat"] = round(100*mets_in[["xyMnRt"]] / mean(rtxy,na.rm=T),2)
    mets_in["xyCvRt"] = mets_in[["xySdRt"]] / mets_in[["xyMnRt"]]
    mets_in["xyCvRtRat"] = round(100*mets_in[["xyCvRt"]] / (sd(rtxy,na.rm=T) / mean(rtxy,na.rm=T)),2)
    mets_in["xyCor"] = cor(x1,y1)
    mets_in["xyCorRat"] = round(100*mets_in[["xyCor"]] / cor(x,y),2)

    #xyz metrics
    mets_in["xyzMinRt"] = min(rtxyz1)
    mets_in["xyzMinRtRat"] = round(100*mets_in[["xyzMinRt"]] / min(rtxyz,na.rm=T),2)
    mets_in["xyzMaxRt"] = max(rtxyz1)
    mets_in["xyzMaxRtRat"] = round(100*mets_in[["xyzMaxRt"]] / max(rtxyz,na.rm=T),2)
    mets_in["xyzSdRt"] = sd(rtxyz1)
    mets_in["xyzSdRtRat"] = round(100*mets_in[["xyzSdRt"]] / sd(rtxyz,na.rm=T),2)
    mets_in["xyzMnRt"] = mean(rtxyz1)
    mets_in["xyzMnRtRat"] = round(100*mets_in[["xyzMnRt"]] / (sd(rtxyz,na.rm=T)/mean(rtxyz,na.rm=T)),2)
    mets_in["xyzCvRt"] =  mets_in[["xyzSdRt"]] / mets_in[["xyzMnRt"]]
    mets_in["xyzCvRtRat"] = round(100*mets_in[["xyzCvRt"]] / (sd(rtxyz,na.rm=T)/mean(rtxyz,na.rm=T)),2)
    mets_in["xyzCor"] = cor(rtxy1,z1)
    mets_in["xyzCorRat"] = round(100*mets_in[["xyzCor"]] / cor(rtxy,z),2)

    #volume and area metrics
    mets_in["xyArea"] = nrow(combnsxy1) * resxy^2
    mets_in["voxVol"] = nrow(combnsxyz1)* resxyz^3
    mets_in["voxVolRat"] = round(100*mets_in[["voxVol"]] / (nrow(combnsxyz)* resxyz^3),2)
    #compute volume between min / max for given raster cell:
    mets_in["gridVol"] = sum(aggregate(z~x+y,data=xyz1,FUN = function(z,resxy)(max(z,na.rm=T) - min(z,na.rm=T))*resxy^2,resxy=resxy)[,3])
    mets_in["gridVolRat"] = round(100*mets_in[["gridVol"]] / sum(aggregate(z~x+y,data=xyz,FUN = function(z,resxy)(max(z,na.rm=T) - min(z,na.rm=T))*resxy^2,resxy=resxy)[,3]),2)
    mets_in["areaCover"] = round(100*mets_in[["xyArea"]] / ( nrow(combnsxy) * resxy^2 ),2)
    mets_in["surfArea"] = sp::surfaceArea(chdfsp)
    mets_in["surfAreaRat"] = round(mets_in[["surfArea"]] / (sum(!is.na(chdf[,3]))*ressurf^2)*100,2)

    if(set_NA){

      #mets_in[names(mets_in)] = NA
      mets_in[names(mets_in)] = -9999
      #provide feasible metrics with zero values?

      if(T){
        #z based metrics
        mets_in["zCover"] = 0
        mets_in["zCoverHt"] = htcover
        mets_in[paste("zRat",zthresh,sep="")] = 0
        mets_in[grep("zBandHt",names(mets_in),value=T)] = 0

        #xyz area and volume metrics
        mets_in["xyArea"] = 0
        mets_in["voxVol"] = 0
        mets_in["gridVol"] = 0
        mets_in["gridVolRat"] = 0
        mets_in["areaCover"] = 0
        mets_in["surfArea"] = 0
        mets_in["surfAreaRat"] = 0
      }

    }
    if(length(unique(sapply(mets_in,class)))>1 ){
      mets_in = sapply(mets_in, as.numeric)
    }

    return(mets_in)
  }
}

# all_metrics = function(obj){
#
#   cbind(
#     lasmetrics(obj,.stdmetrics)
#     ,lasmetrics(obj,entropy(Z-min(Z)+1))
#     ,lasmetrics(obj,.LAD)
#     )
#
# }


#some tests

if(F) {
  require(lidR)
  if(!"las1" %in% ls()) lidR::las1 = readLAS("D:\\Box\\sync\\R\\analyses\\wa_dsm_env_fia\\data\\plot_clips_ht\\plot_66399.laz")
  test2 = lasmetrics(las1,experimental_metrics(X,Y,Z,adjustz="none",outlier=c(-15,450)))
  test2
}

if(F){

  library(lidR)
  test=list()
  test["test1"] = 1
  test
  las1 = readLAS("D:\\Box\\sync\\R\\analyses\\wa_dsm_env_fia\\data\\plot_clips_ht\\plot_44.laz")

  test1 = lasmetrics(las1,.stdmetrics)
  test2 = lasmetrics(las1,.stdmetrics)
  test2 = lasmetrics(las1,LAD(Z))
  test3 = all_metrics(las1)

  test2 = lasmetrics(las1,experimental_metrics(X,Y,Z))


  m1 = grid_metrics(las, stdmetrics(X,Y,Z,Intensity,ReturnNumber,Classification,dz=1))

}
