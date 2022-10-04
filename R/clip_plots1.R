#'@title
#'  clip plots or polygons form lidar
#'
#'@description
#'  clip plots or polygons form lidar - requires that one first make a project using project which
#'  includes both lidar and dtms
#'
#'@details
#'  There are three ways to provide the input shapes to this function - either as a
#'  list of plot ids, coordinates and plot diameters (4 column data.frame), as a list of vertices
#'  organized by plot_id (3 column data.frame), or as a polygon shapefile (path to shapefile) inwich case
#'  the plot id field must be specified "id_field_plots".
#'
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
#'1.1 \tab move to SF objects and geopackages \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param plot_polys sf polygons for plots
#'@param id_field_plots (optional) in the event that a polygon is provided
#'@param project_gpkg spatial polygons dataframe (intersection between dtm and las tiles)
#'@param project path to spatial polygons dataframe (intersection between dtm and las tiles)
#'@param plot_tile_intersect path to spatial polygons dataframe representing intersection between plots and project
#'@param dir_out where to send clipped plots
#'@param height T/F subtract the ground
#'@param do_plot T/F make a plot of the intersections
#'@param return T/F return intersectiosn
#'@param n_core number of cores to use in clipping
#'@param dir_dtm in case path to dtms has changed from project
#'@param dir_las in case path to las has changed from project
#'@param skip_existing skip csv files that have been processed already - in case processing was interupted


#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import rgdal plyr rgeos raster lidR data.table parallel terra
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ",readClipboard())),collapse="\n"))


clip_plots1=function(
  polys =NA #sf vector object
  ,poly_id = "id"
  ,dir_las = NA
  ,dir_dtm = NA
  ,file_las = NA
  ,file_dtm = NA
  ,pattern_las = c("[.]las$","[.]laz$")[1]
  ,pattern_dtm = c("[.]img$","[.]tif$","[.]dtm$","[.]vrt$")[1]
  ,dir_out=NA
  ,height=T
  ,n_core=6
  ,skip_existing=T
  ,fix_dsm_bug = F # laz files for hood canal DSMs are screwed up
  ,recurse_las=F
  ,recurse_dtm=F
  ,return = T
  ,temp="c/temp/clip_plots/"
){
  if(interactive()){
    require(rgdal)
    require(plyr)
    require(rgdal)
    require(rgeos)
    require(raster)
    require(lidR)
    require(data.table)
    require(parallel)
  }
browser()
  if(!file.exists(dir_out)) try(dir.create(dir_out, recursive=T),silent=T)
  dir_skip=file.path(dir_out,"skip")

  if(!file.exists(dir_skip)) try(dir.create(dir_skip, recursive=T),silent=T)

    if(is.na(file_las)){
      pths_las = unlist(sapply(dir_las, list.files, pattern = pattern_las, recurse = recurse_las, full.names=T))
      ctg_las = lidR::catalog(pths_las)
    }else{
      ctg_las = lidR::readLAS(file_las)
    }
    if(height){
      if(is.na(file_dtm) & !is.na(dir_dtm[1])){
        pths_dtm = unlist(sapply(dir_dtm, list.files, pattern = pattern_dtm, recurse = recurse_dtm, full.names=T))
        dtm_in = terra::vrt(pths_dtm, file.path(temp,"dtm_list.vrt"))
      }
      if(!is.na(file_dtm)){
        dtm_in = terra::raster(file_dtm)
      }
    }

    #prep parallel environment
    cl_in = parallel::makeCluster(n_core)
    doParallel::registerDoParallel(cl_in)

    pths_clip_in = foreach(i = 1:nrow(  polys ), .combine=c) %dopar% {

      pth_out_i = file.path(dir_out,paste0(polys[i,poly_id],".laz"))
      if(!skip_existing | !file.exists(skip_existing)){
        las_in_i = lidR::clip_roi(polys[i,], ctg_las)

        if(height){
          # see here for more approaches https://gis.stackexchange.com/questions/365118/how-to-save-polygons-generated-from-raster-extents-to-shapefile-in-r
          dtm_in_i = terra::crop(dtm_in, sf::st_buffer(polys[i,],10) )
          las_in_i_cr = lidR::clip_roi(las_in_i,sf::st_as_sf(sf::st_bbox(dtm_in_i)))
          las_in_i = lidR::normalize_height(las_in_i_cr,dtm_in_i)
        }

        #write to disk
        err = try(lidR::writeLAS(las_in_i, file.path(dir_out,paste0(polys[i,poly_id],".laz"))))
        pth_out_i
      }
    }

    if(return) return(lidR::catalog(pths_clip_in))

}

