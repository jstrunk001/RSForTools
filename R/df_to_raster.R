#'@title
#'  Convert columns of metrics in a data.frame to rasters, requires
#'
#'@description
#'  <Delete and Replace>
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
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 3/16/2020 Function created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param df a dataframe with xy and columns to use in making a rasters
#'@param colsxy  names of xy columns
#'@param cols2Raster which columns to grab
#'@param format  raster formate  e.g. .tif , .img etc
#'@param dirOut  where to export rasters
#'@param crs  proj4 string or other crs notation used by raster package (see writeRaster)
#'@param nProc  number of cores to use in converting to raster
#'@param doDebug run in debug mode and only read 50k rows?
#'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import raster
#'
#'@export
#
#'@seealso \code{\link{rasterFromXYZ}}\cr \code{\link{writeRaster}}\cr

#Desired upgrades to this function:
#
#
#

df_to_raster = function(
  df
  ,colsxy = c("center_x","center_y")
  ,cols2Raster = c(colsSomeX)
  ,format = ".img"
  ,dirOut = "E:\\projects\\2017_NAIP\\rasters\\"
  ,crs = NA
  ,nProc = 10
  ,doDebug=F

){
  bad_rows = is.na(df[,colsxy[1]]) | is.na(df[,colsxy[2]])
  warning("df has ",sum(bad_rows)," bad rows (NA in x or y values) - observation(s) removed")

  debugRows = 50000
  require(raster)
  if(doDebug) xy = df[!bad_rows,colsxy][1:debugRows,]
  else xy = df[!bad_rows,colsxy]
  if(!dir.exists(dirOut)) dir.create(dirOut)

  raster::beginCluster(nProc)

  r0 = raster::rasterFromXYZ(xy)
  xy1 = xy
  coordinates(xy) = xy[,colsxy]
  ids = raster::cellFromXY(r0, xy1)
  r0[] = NA
  crs(r0) = crs

  for(i in 1:length(cols2Raster)){

    print(paste("start:",cols2Raster[i],"at",Sys.time()))
    # if(doDebug) dati = dbGetQuery(db,paste("select",cols2Raster[i],"from",tb_csv,"limit",debugRows))
    # else dati = dbGetQuery(db,paste("select",cols2Raster[i],"from",tb_csv))

    r0[ids] = df[!bad_rows,cols2Raster[i]]
    outi = file.path(dirOut,paste(cols2Raster[i],format,sep=""))
    raster::writeRaster(r0,outi,overwrite=TRUE,crs = crs)

    print(paste("complete:",cols2Raster[i],"at",Sys.time()))

  }

  raster::endCluster()

}

#convenience variables
colsAll = c('row','col','center_x','center_y','total_return_count_above_6_00','ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_skewness','ht_kurtosis','ht_aad','ht_l1','ht_l2','ht_l3','ht_l4','ht_l_cv','ht_l_skewness','ht_l_kurtosis','ht_p01','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_6_00','return_2_count_above_6_00','return_3_count_above_6_00','return_4_count_above_6_00','return_5_count_above_6_00','return_6_count_above_6_00','return_7_count_above_6_00','return_8_count_above_6_00','return_9_count_above_6_00','other_return_count_above_6_00','percentage_first_returns_above_6_00','percentage_all_returns_above_6_00','all_returns_above_6_00_total_first_returns_100','first_returns_above_6_00','all_returns_above_6_00','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean','identifier')
colsID = c('row','col','center_x','center_y','identifier')
colsAllX = c('total_return_count_above_6_00','ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_skewness','ht_kurtosis','ht_aad','ht_l1','ht_l2','ht_l3','ht_l4','ht_l_cv','ht_l_skewness','ht_l_kurtosis','ht_p01','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_6_00','return_2_count_above_6_00','return_3_count_above_6_00','return_4_count_above_6_00','return_5_count_above_6_00','return_6_count_above_6_00','return_7_count_above_6_00','return_8_count_above_6_00','return_9_count_above_6_00','other_return_count_above_6_00','percentage_first_returns_above_6_00','percentage_all_returns_above_6_00','all_returns_above_6_00_total_first_returns_100','first_returns_above_6_00','all_returns_above_6_00','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean')
colsSomeX = c('ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_aad','ht_l1','ht_l2','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_6_00','percentage_first_returns_above_6_00','percentage_all_returns_above_6_00','all_returns_above_6_00_total_first_returns_100','first_returns_above_6_00','all_returns_above_6_00','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean')


