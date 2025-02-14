#'@title
#'  convert xy coordinates to fia plots
#'
#'@description
#'  supply fia plot centers, and have have Spatial Polygons Dataframe returned, where each record is
#'  an FIA plot footprint: 4 fixed area subplots of 24 foot radius, a central subplot and 3 peripheral
#'  subplots 120 feet from plot center at 0,120,240 degrees.
#'
#'  Ignores declination - assumes everything faces projected north
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
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 09/09/2022 Upgraded to sf added more parameters for output plot   \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param idxy dataframe with id, x, y columns available
#'@param col_names link dataframe columns to variable names used by function
#'@param create_polys TRUE / FALSE create polygons or just return theoretical subplot centers
#'@param  width radius or side length  of subplot
#'@param  endCapStyle enable square or round plots
#'@param  offx  set x offsets to subplots 2-4
#'@param  offy  set y offsets to subplots 2-4
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'    res= xy2FIAplot(data.frame(plot=1:10, x=101:110*10000, y=101:110*10000),create_polys=T)
#'
#'@import dplyr plyr sf
#'
#'@export
#
#'@seealso \code{\link{bbox2polys}}\cr \code{\link{sf}}\cr \code{\link{st_buffer}}\cr

#Desired upgrades to this function:
# add declination
# add different subplot distances
# add different polygon radii
# enable supplying any subplot as an initial condition

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ",readClipboard())),collapse="\n"))

xy2FIApoly=function(
                      idxy
                    , col_names=c(plot="plot",x="x",y="y")
                    , create_polys=T
                    , combine_subplots=T
                    , width=24
                    , endCapStyle="ROUND"
                    , offx = c("2"=0,"3"=103.923,"4"=-103.923)
                    , offy = c("2"=120,"3"=-60,"4"=-60)
                    ){


  # sqrt((120*cos(pi/6))^2+(120*sin(pi/6))^2)

  requireNamespace("plyr")

  col_names["subplot"]="subplot"
  idxy[,col_names["subplot"]]=1

  #prepare individual subplots
  dat2=as.data.frame(idxy,drop=T)
  dat2[,col_names["subplot"]]=2
  dat2[,col_names["x"]]=dat2[,col_names["x"]] + offx["2"]
  dat2[,col_names["y"]]=dat2[,col_names["y"]] + offy["2"]

  dat3=as.data.frame(idxy,drop=T)
  dat3[,col_names["subplot"]]=3
  dat3[,col_names["x"]]=dat3[,col_names["x"]] + offx["3"]
  dat3[,col_names["y"]]=dat3[ , col_names["y"] ] + offy["3"]

  dat4=as.data.frame(idxy,drop=T)
  dat4[,col_names["subplot"]]=4
  dat4[,col_names["x"]]=dat4[,col_names["x"]] + offx["4"]
  dat4[,col_names["y"]]=dat4[,col_names["y"]] + offy["4"]

  df_all = plyr::rbind.fill(idxy,dat2,dat3,dat4)
  if((!create_polys) && !combine_subplots) dat_return = df_all

  if(create_polys | combine_subplots){

    requireNamespace("sf")
    requireNamespace("dplyr")

    #create sf object
    sf_dat0 = sf::st_as_sf(df_all,coords=col_names[c("x","y")])

    if(combine_subplots){

      #group subplots by plot
      sf_dat1 = dplyr::group_by_at(sf_dat0, .vars=col_names["plot"])

      #merge geometries into single plot
      sf_dat2 = as.data.frame(dplyr::summarise(sf_dat1,geometry = sf::st_union(geometry)))
      dat_return = sf::st_as_sf(sf_dat2)

      #buffer by desired amount
      if(create_polys){
        sf_dat3 = sf::st_buffer(sf::st_as_sf(sf_dat2) , width , endCapStyle = endCapStyle)
        dat_return = sf_dat3
      }

    }
    if(!combine_subplots) dat_return = sf_dat0

  }

  names(dat_return)[1] = col_names["plot"]

  return(dat_return)

}


if(F){

  res=xy2FIApoly(data.frame(plot=1:10, x=101:110*10000, y=101:110*10000),create_polys=T)
  sp::spplot(res[1,],zcol=1,aspect=1,scales=list(draw=T),key.space=list(x=0.2,y=0.9,corner=c(0,1)))

  library(leaflet)
  l1=leaflet(data=res[1,],options=leafletOptions(crs=leafletCRS(proj4def="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")))
  addPolygons(l1,data=res[1:2,])

}

