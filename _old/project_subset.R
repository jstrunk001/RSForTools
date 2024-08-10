#'@title
#'  subset tiles in a RSForInvt project
#'
#'@description
#'  supply a polygon and subset tiles in a lasR project
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 April 14 Implemented \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'
#'@param path_project path to project file
#'@param path_shapefile path to shapefile
#'@param ignore_proj4 should proj4 differences be ignored
#'@param dir_out where to put subset
#'@param return should the subset project be returned
#'@param plot show a plot of project subset

#'
#'@return
#'  NULL
#'
#'@examples
#'  lasR_proj_small=lasR_subset("C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\intersections.csv"
#'  ,"C:\\projects\\2017_WA_DSM_Pilot\\boundary\\extentB.shp"
#'  ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\subset_project\\"
#'  )
#'
#'@import sf
#'
#'@export
#
#'@seealso \code{\link{lasR_project}}\cr \code{\link{scan_las}}\cr \code{\link{scan_dtm}}\cr

#Desired upgrades to this function:
#
#


project_subset=function(
   project_ply
  ,subset_ply
  ,path_out_gpkg = NA
  ,layer_out_gpkg = "project_subset"
  ,path_out_csv=c(NA,"")
  ,ignore_proj4=T
  ,return=F
  ,plot=T
  ){

  require(rgdal)
  require(rgeos)

  if(ignore_proj4 ) proj4string(project_ply) = proj4string(subset_ply)

  #load project
  if(is.na(path_project_gpkg)){

    proj_in=read.csv(path_project)

    #create spatial object from project
    tile_proj0=bbox2polys(proj_in[,c("tile_id","mnx","mxx","mny","mxy")])
    row.names(proj_in)=proj_in[,c("tile_id")]
    tile_proj1=SpatialPolygonsDataFrame(tile_proj0,proj_in)

    #load shapefile
    bounds=readOGR(dsn=dirname(path_shapefile),layer=gsub("[.]shp","",basename(path_shapefile)))

    #ignore proj strin


    #intersect tiles and bounds
    #tiles_subset=gContains(bounds,tile_proj1,byid=T)
    tiles_subset=gIntersects(gBuffer(bounds,width=0),tile_proj1,byid=T,returnDense=TRUE)
    if(sum(unlist(tiles_subset))>0) tile_proj2=tile_proj1[tiles_subset[,1,drop=T],]

    if(plot){
      plot(bounds,add=T)
      plot(tile_proj2,add=T)
    }

  }

  writeOGR( tile_proj2 , dsn = path_gpkg , layer = paste(basename(path_project),"_subset",sep="") , driver="GPKG")
  write.csv( tile_proj2 , paste(dir_out,"\\", paste(basename(path_project),"_subset.csv",sep="") , sep="") )

}

if(F){

  tiles=select_tiles("C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\intersections.csv"
                     ,"C:\\projects\\2017_WA_DSM_Pilot\\boundary\\extentB.shp"
                     ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\subset_project\\"
                     )

  }
