#'@title
#'  horizontal translation of xy locations (and vertical feet <-> meters)
#'
#'@description
#'  horizontal translation of xy locations (and vertical feet <-> meters)
#'
#'@details
#'  horizontal translation of xy locations (and vertical feet <-> meters)
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/21/2020 Created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param las (optional) las object returned by lidR::readLAS()
#'@param las_in_path (optional) character string path to input las/laz file
#'@param las_out_path (optional) character string path to translated las file
#'@param dir_out (optional) character string path to directory to hold translated file
#'@param out_prefix  add prefix to file name
#'@param out_suffix  add suffix to file name before las/laz extension
#'@param out_type character string ".las" or ".laz"
#'@param proj4from (optional) only optional if input las/laz
#'@param proj4to (required) character string proj4 string to translate
#'@param zMult specify multiplier for vertical units to go e.g. from feet to meters
#'@param return T/F return translated object?
#'@param doWriteLAS T/F write translated object to disk
#'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'
#'      require(sp)
#'      require(lidR)
#'      require(RSForInvt)
#'
#'      if(!"las6in" %in% ls()) las6in = readLAS("D:/temp/hood_canal_test/clip6in/clipElev_FtTenthAc/clip_1.las")
#'      if(!"las3in" %in% ls()) las3in = readLAS("D:/temp/hood_canal_test/clip3in/clipElev_FtTenthAc/clip_1.las")
#'      if(!"las6in0" %in% ls()) las6in0 = readLAS("D:/data/WA/wadnr_hood_canal/laz/hood_canal_6in_DSM_2015/area1_000_000.0_6.laz")
#'      if(!"lasLid0" %in% ls()) lasLid0 = readLAS("D:/data/WA/wadnr_hood_canal/laz/hood_canal_lidar_2015/Hood_Canal_001.laz")
#'
#'      proj4string(las6in)
#'      proj4string(las6in0)
#'      proj4string(las3in)
#'      proj4string(lasLid0)
#'
#'      #read lidar file and past LAS object to translateLAS
#'      lasLid0 = readLAS("D:/data/WA/wadnr_hood_canal/laz/hood_canal_lidar_2015/Hood_Canal_001.laz")
#'
#'      translateLAS(
#'        las = lasLid0
#'        ,las_in_path = NA
#'        ,las_out_path = "d:/temp/testtranslate/Hood_Canal_001.laz"
#'        ,dir_out = "d:/temp/testtranslate/"
#'        ,out_prefix = "rp_"
#'        ,out_suffix = "_utmZ10m"
#'        ,out_type=".laz"
#'        ,proj4from = NA
#'        ,proj4to = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#'        ,zFrom = c("ft")
#'        ,zTo = c("m")
#'      )
#'
#'      #provide path to las file
#'      #return las
#'      #don't write a new file
#'      lasLid0_rp = translateLAS (
#'        las = NA
#'        ,las_in_path = "D:/data/WA/wadnr_hood_canal/laz/hood_canal_lidar_2015/Hood_Canal_001.laz"
#'        ,las_out_path = "d:/temp/testtranslate/Hood_Canal_001.laz"
#'        ,dir_out = "d:/temp/testtranslate/"
#'        ,out_prefix = "rp_"
#'        ,out_suffix = "_utmZ10m"
#'        ,out_type=".laz"
#'        ,proj4from = NA
#'        ,proj4to = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
#'        ,zFrom = c("ft")
#'        ,zTo = c("m")
#'        ,return = T
#'        ,doWriteLAS = F
#'      )
#'
#'@import lidR rgdal gdalUtils
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


translateLAS = function(
          las = NA
          ,las_in_path = NA
          ,las_out_path = NA
          ,dir_out = NA
          ,out_prefix = ""
          ,out_suffix = ""
          ,out_type=c(".las",".laz")
          ,proj4from = NA
          ,proj4to = NA
          ,zFrom = c("ft","m")
          ,zTo = c("ft","m")
          ,zMult = c(1.0 , .3048 , 1/ .3048)[1]
          ,return=F
          ,doWriteLAS=T
          ){

  #not worth proceeding if we bump into these errors
  if(is.na(las_out_path) & is.na(las_in_path) & doWriteLAS) stop("must provide las_out_path when las_in_path is not provided")
  if(is.na(proj4to)) stop("proj4to is an empty string, please provide valid proj4 string")

  require(lidR)

  #deal with input las
  las_in = las
  if(!isS4(las_in)) las_in = lidR::readLAS(las_in_path)

  #deal with proj4 string
  proj4las = proj4string(las_in)
  if(is.na(proj4from)) proj4from = proj4las
  if(proj4from == "") proj4from = proj4las
  if(proj4from == "") stop("las file did not have proj4 string and proj4from is either NA or an empty string, please provide valid proj4 string")

  #prepare output folder / file
  if(doWriteLAS){
    if(is.na(dir_out) & is.na(las_out_path) & is.na(las_in_path)) stop("must provide one of dir_out or las_out_path when las_in_path is NA")
    if(is.na(dir_out) & is.na(las_out_path)) dir_out = paste(dirname(las_in_path),"/translate/")
    if(is.na(dir_out)) dir_out = dirname(las_out_path)
    if(!dir.exists(dir_out)) dir.create(dir_out)
    if(!is.na(las_out_path)) las_out_path = paste(dir_out,"/",out_prefix,gsub("[.]la.$","",basename(las_out_path)),out_suffix,out_type[1],sep="")
    if(is.na(las_out_path) & !is.na(las_in_path)) las_out_path = paste(dir_out,"/",out_prefix,gsub("[.]la.$","",basename(las_in_path)),out_suffix,out_type[1],sep="")
  }
  #translate xy data
  xy_in = las_in@data[,c("X","Y")]
  coordinates(xy_in)=~X+Y
  proj4string(xy_in) = proj4from
  xy_in1=spTransform(xy_in,proj4to)

  #reload updated coordinates
  las_in@data[,c("X","Y")] = as.data.frame(coordinates(xy_in1))

  #translate vertical coordinates
  las_in@data[,c("Z")] = las_in@data[,c("Z")] * zMult[1]

  #update header
  las_in@header@VLR$GeoKeyDirectoryTag = NULL
  las_in@header@PHB$`X scale factor` = 1
  las_in@header@PHB$`Y scale factor` = 1
  las_in@header@PHB$`Z scale factor` = 1
  las_in@header@PHB$`X offset` = 0
  las_in@header@PHB$`Y offset` = 0
  las_in@header@PHB$`Z offset` = 0

  #this probably doesn't work, lol
  defaultW <- getOption("warn")
  options(warn = -1)
  proj4string(las_in) = proj4to
  options(warn = defaultW)

  if(doWriteLAS) lidR::writeLAS(las_in,las_out_path)

  if(return) return(las_in)
}

if(F){

  require(sp)
  require(lidR)
  require(RSForInvt)

  if(!"las6in" %in% ls()) las6in = readLAS("D:/temp/hood_canal_test/clip6in/clipElev_FtTenthAc/clip_1.las")
  if(!"las3in" %in% ls()) las3in = readLAS("D:/temp/hood_canal_test/clip3in/clipElev_FtTenthAc/clip_1.las")
  if(!"las6in0" %in% ls()) las6in0 = readLAS("D:/data/WA/wadnr_hood_canal/laz/hood_canal_6in_DSM_2015/area1_000_000.0_6.laz")
  if(!"lasLid0" %in% ls()) lasLid0 = readLAS("D:/data/WA/wadnr_hood_canal/laz/hood_canal_lidar_2015/Hood_Canal_001.laz")

  proj4string(las6in)
  proj4string(las6in0)
  proj4string(las3in)
  proj4string(lasLid0)

  #read lidar file and past LAS object to translateLAS
  lasLid0 = readLAS("D:/data/WA/wadnr_hood_canal/laz/hood_canal_lidar_2015/Hood_Canal_001.laz")

  translateLAS (
    las = lasLid0
    ,las_in_path = NA
    ,las_out_path = "d:/temp/testtranslate/Hood_Canal_001.laz"
    ,dir_out = "d:/temp/testtranslate/"
    ,out_prefix = "rp_"
    ,out_suffix = "_utmZ10m"
    ,out_type=".laz"
    ,proj4from = NA
    ,proj4to = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    ,zMult =  .3048
  )

  #provide path to las file
  #return las
  #don't write a new file
  lasLid0_rp = translateLAS (
    las = NA
    ,las_in_path = "D:/data/WA/wadnr_hood_canal/laz/hood_canal_lidar_2015/Hood_Canal_001.laz"
    ,las_out_path = "d:/temp/testtranslate/Hood_Canal_001.laz"
    ,dir_out = "d:/temp/testtranslate/"
    ,out_prefix = "rp_"
    ,out_suffix = "_utmZ10m"
    ,out_type=".laz"
    ,proj4from = NA
    ,proj4to = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
    ,zMult =.3048
    ,return = T
    ,doWriteLAS = F
  )

}
