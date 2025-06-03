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

  spectral_metrics = function(
    z=NA
    ,i=NA
    ,r=NA
    ,g=NA
    ,b=NA
    ,nir=NA
    ,htcover = 6
    ,qts = c(1,5,10,20,30,40,50,60,70,80,90,95,99)
    ,adjustz = c("none","positive","min0","zq01")
    ,outlier = c(min=NA,max=NA)
    ,stdmetrics = T
    ,...
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
    if(!is.na(i[1])) i = i[!outlier_ids]
    if(!is.na(r[1])) r = r[!outlier_ids]
    if(!is.na(g[1])) g = g[!outlier_ids]
    if(!is.na(b[1])) b = b[!outlier_ids]
    if(!is.na(nir[1])) nir = nir[!outlier_ids]
  }

  #begin metric computations
  mets_in = list()
  if(!is.na(i[1]) & !is.na(r[1]) & !is.na(g[1]) & !is.na(b[1]) & !is.na(nir[1]) ){

    i1 = i[z >= htcover]
    r1 = r[z >= htcover]
    g1 = g[z >= htcover]
    b1 = b[z >= htcover]
    nir1 = nir[z >= htcover]

    #enable non-fail if no points are above htcover
    n1=length(i1)
    if(n1 > 5) set_NA = F else set_NA = T
    if(set_NA | diff(range(i1))==0 ) i1 = seq(0,5,1)
    if(set_NA | diff(range(r1))==0 ) r1 = seq(0,5,1)
    if(set_NA | diff(range(g1))==0 ) g1 = seq(0,5,1)
    if(set_NA | diff(range(b1))==0 ) b1 = seq(0,5,1)
    if(set_NA | diff(range(nir1))==0 ) nir1 = seq(0,5,1)

    #add spectral quantiles
    mets_in = list(n=n1 )
    mets_in = c(mets_in, .f_qt(i1,qts,"i") )
    mets_in = c(mets_in, .f_qt(r1,qts,"r") )
    mets_in = c(mets_in, .f_qt(g1,qts,"g") )
    mets_in = c(mets_in, .f_qt(b1,qts,"b") )
    mets_in = c(mets_in, .f_qt(nir1,qts,"nir") )

    #zero out cells without values
    if(set_NA) mets_in[-1] = NA

    return(mets_in)
  }
}


  .f_1x = function(x,idx,qts,nm){

    if(!is.na(x[1])){
      #subset values based on supplied index - usually based on z
      x1 = x[idx]
      #test for bad vector
      bad_x = length(x1) < 5 | diff(range(x1))==0
      if(bad_x) qts_i = rep(NA,length(qts))
      #compute quantiles
      else qts_i = quantile(x1,qts/100,na.rm=T)
      #name vector
      names(qts_i) = paste0(nm,"_p",qts)
      return(qts_i)
    }else NULL

  }

    #compute quantiles from irgbnir
    .f_qt = function(x,qts,nm){
      qts_i = quantile(x,qts/100,na.rm=T)
      names(qts_i) = paste0(nm,"_p",qts)
      return(qts_i)
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
  las1 = readLAS("c:/temp/id_36267.laz")

  test1 = lasmetrics(las1,.stdmetrics)
  test2 = lasmetrics(las1,.stdmetrics)
  test2 = lasmetrics(las1,LAD(Z))
  test3 = all_metrics(las1)

  test2 = lasmetrics(las1,experimental_metrics(X,Y,Z))

  experimental_metrics(las1$X,las1$Y,las1$Z)


  m1 = grid_metrics(las, stdmetrics(X,Y,Z,Intensity,ReturnNumber,Classification,dz=1))

}
