#'@title
#'  create polygons from dataframe of bounding coordinates and ids
#'
#'@description
#'  create polygons from dataframe of bounding coordinates and ids
#'
#'@details
#'  create polygons from dataframe of bounding coordinates and ids
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
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param df_ext a data.frame with 4 columns of coordinates: minx, maxx, miny, and maxy indicating bounding boxes or extents
#'@param cols_ext a named vector with names 'xmin','xmax','ymin','ymax' indicating where bound box coordinates are stored
#'@param wkt2 standard crs format for sf and terra packages
#'
#'@return
#'  sf spatial data frame with all original fields and added geometry field
#'
#'@examples
#'  #none yet
#'
#'@import sf
#'
#
#'@seealso \code{\link{scan_las}}\cr \code{\link{read_las}}\cr

#'@export
bbox2polys = function(df_ext, cols_ext=c(xmin="min_x",xmax="max_x",ymin="min_y",ymax="max_y") , wkt2=NA){

  #tests
  if( sum(cols_ext %in% names(df_ext)) < 4) stop("must specify columns with coordinates for bound box; at least one column in cols_ext is not present in df_ext")
  if( sum(names(cols_ext) %in% c('xmin','xmax','ymin','ymax') ) < 4) stop("'cols_ext' must be a named vector with names 'xmin','xmax','ymin','ymax' which indicate extent columns in df_ext")

  #update column names to match sf requirements for st_bbox
  df_ext_in = df_ext[,cols_ext]
  names(df_ext_in) = names(cols_ext)

  #use sf to make bbox polygons
  df_in = sf::st_as_sf(
    data.frame(
    df_ext
    ,geometry=do.call(rbind,
        apply(df_ext_in,1,function(x) sf::st_as_sfc(sf::st_bbox(x)))
      )
    )
  )
  if(!is.na(wkt2)) sf::st_crs(df_in) = sf::st_crs(wkt2)

  df_in

}

