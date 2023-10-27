#'@title
#'  Build point cloud processing tiling project (.laz, .las, .dtm) for use with FUSION
#'
#'@description
#'  Scans lidar and dems and finds out where they intersect a project tiling scheme
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/07/2020 New package derived from old lasR package \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>

#'@param dir_las where are las files
#'@param dir_dtm where are FUSION dtm files - eventuall enable any dtm type (.img, .tif etc)
#'@param dir_project where to place project
#'@param project project name
#'@param project_dtm dtm project name
#'@param project_las las project name
#'@param dtm_year year of dtm files
#'@param las_year year of las files
#'@param scan_dtms ?scan dtm files
#'@param scan_las ?scan las files
#'@param tile_size processing tile size
#'@param pixel_size raster pixel size
#'@param xmn,xmx,ymn,ymx bound box for processing grid
#'@param crs projection string
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#' proj_tn = RSForInvt::project_create(
#'   #'proj = project_create(
#'   dir_las="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\Data\\Tennessee\\lidar_tiles\\"
#'   ,dir_dtm="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\Data\\Tennessee\\DTM_fusion\\"
#'   ,path_gpkg_out="D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\R\\DAP_Lidar_analysis\\RSForInvt\\project\\TNLidar_RSForInvtProject.gpkg"
#'   ,layer_project = "RSForInvt_prj"
#'   ,layer_config = "RSForInvt_config"
#'   ,overwrite_project = T
#'   ,project_dtm="lidar_dtm"
#'   ,project_las="lidar_las"
#'   ,dtm_year="2018"
#'   ,las_year="2018"
#'   ,do_scan_dtms=F #'we already scanned the dtm folder - ok, to rescan, but slower
#'   ,do_scan_las=F #'we already scanned the las folder - ok, to rescan, but slower
#'   ,duplicate_las = c("ignore","remove")
#'   ,duplicate_dtm = c("ignore","remove")
#'   ,tile_size=1650
#'   ,pixel_size=66
#'   #',xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
#'   ,proj4 = "+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
#'   ,mask=NA
#'   ,return=T
#' )
#'
#'
#'@import DBI RSQLite data.table rgdal rgeos sp raster
#'
#'@export
#
#'@seealso \code{\link{scan_dtm}}\cr \code{\link{scan_las}}\cr
#'
#'
#'desired updates:
#'  add ability to scan fusion dtms and generic raster dtms
#'

project_create=function(
   dir_las=NA
  ,dir_dtm=NA
  ,recurse_dtm = F
  ,recurse_las = F
  ,dir_out="c:/lidar_projects/"
  ,layer_project = "RSForInvt_prj"
  ,layer_config = "RSForInvt_config"
  ,overwrite_project = T
  ,project_dtm="someProject_dtm"
  ,project_las="someProject_las"
  ,dtm_year="2099"
  ,las_year="2099"
  ,do_scan_dtms=T
  ,do_scan_las=T
  ,duplicate_las = c("ignore","remove")
  ,duplicate_dtm = c("ignore","remove")
  ,tile_size=1650
  ,pixel_size=66
  ,xmn=c(NA,561066)
  ,xmx=c(NA,2805066)
  ,ymn=c(NA,33066)
  ,ymx=c(NA,1551066)
  ,proj4 = c(NA,"+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
  ,mask = NA
  ,return = T

){

  options(stringsAsFactors = F)
  this_time = Sys.time()

  requireNamespace("DBI")
  requireNamespace("RSQLite")
  requireNamespace("data.table")
  requireNamespace("rgdal")
  requireNamespace("rgeos")
  requireNamespace("sp")
  requireNamespace("raster")
  requireNamespace("plyr")

  warning("UPDATE ME!!! Allow me to 'update' intersections without complete reset")

  #create project folder
  project_path = dir_out
  if(!dir.exists(project_path)) dir.create(project_path,recursive=T)

  #match tile size with pixel size
  tile_pixel_ratio = tile_size / pixel_size
  if(ceiling(tile_pixel_ratio) > tile_pixel_ratio){
    tile_size = ceiling(tile_size / pixel_size)
    warning("tile size not a multiple of pixel size, tile size set to ", tile_size)
  }

  #inventory las and dtms
  if(do_scan_las) scan_las(project=project_las, project_year=las_year, dir_las=dir_las, dir_out=project_path, create_polys=T , recursive = recurse_las , proj4 = proj4[1])
  print("scan_las");print(Sys.time())
  if(do_scan_dtms) scan_dtm(project=project_dtm, project_year=dtm_year,dir_dtm=dir_dtm, dir_out=project_path, create_polys=T , recursive = recurse_dtm , proj4 = proj4[1])
  print("scan_dtm");print(Sys.time())

  #file names
  path_dtm_proj=paste(dir_out,"/manage_dtm.gpkg",sep="")
  path_las_proj=paste(dir_out,"/manage_las.gpkg",sep="")

  #read in las and dtm polygons
  dtm_polys = rgdal::readOGR(dsn = path_dtm_proj,"dtm_polys")
  las_polys = rgdal::readOGR(dsn = path_las_proj,"las_polys")
  #las_polys = sf::st_read(dsn = path_las_proj,"las_polys")
  #dtm_polys = sf::st_read(dsn = path_dtm_proj,"dtm_polys")

  #remove duplicates if present
  if(duplicate_las[1] == "remove"){
    las_polys = subset( las_polys , subset= !duplicated(las_polys$file_name) )
  }
  if(duplicate_dtm[1] == "remove"){
    dtm_polys = subset( dtm_polys , subset= !duplicated(dtm_polys$file_name) )
  }
browser()
  #get proj4 if not provided and add to dtms if needed
  if(!is.na(proj4)) proj4_in = proj4
  else proj4_in = sp::proj4string(las_polys)

  dtm_proj4 = sp::proj4string(dtm_polys)
  if(is.na(dtm_proj4)) sp::proj4string(dtm_polys) = proj4_in

  #buffer polygons
  dtm_polys1=rgeos::gBuffer(dtm_polys,byid=T,width=round(pixel_size*4+1),capStyle="square");gc()
  las_polys1=rgeos::gBuffer(las_polys,byid=T,width=round(pixel_size*4+1),capStyle="square");gc()
  print("completed: buffer las and dtm polygons");print(Sys.time())

  #create processing tiles
  if( ( is.na(xmn[1]) | is.na(xmx[1]) | is.na(ymn[1]) | is.na(ymx[1]) ) ){
    ext = raster::extent(las_polys1)
    xmn = ext@xmin
    xmx = ext@xmax
    ymn = ext@ymin
    ymx = ext@ymax
  }
  proc_rast = raster::raster(xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,resolution=tile_size,crs=raster::crs(proj4_in));gc()
  proc_rast[] = raster::cellsFromExtent(proc_rast,raster::extent(proc_rast));gc()
  xy = raster::as.data.frame(proc_rast,xy=T)
  proc_poly = raster::rasterToPolygons(proc_rast, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
  print("completed: create tile scheme ");print(Sys.time())

  #mask if desired
  if(!is.na(mask[1])){
    mask1=rgeos::gBuffer(mask,width=tile_size,capStyle="square")
    proc_poly = raster::crop(proc_poly,mask1)
  }
  print("completed: mask to sub extent");print(Sys.time())


  #extract dtm tiles with polygons
  dtm_poly_sf <- sf::st_as_sf(dtm_polys)
  proc_poly_sf <- sf::st_as_sf(proc_poly)
  sf::st_crs(dtm_poly_sf) = sf::st_crs(proc_poly_sf)
  ex_dtm <- sf::st_intersects(dtm_poly_sf, proc_poly_sf,sparse=T)
  names(ex_dtm) = dtm_polys1$file_path
  ex_dtm1 = ex_dtm[lapply(ex_dtm,length)>0]
  print("completed: extract dtm polygons");print(Sys.time())


  #extract las tiles with polygons
  las_polys1_sf <- sf::st_as_sf(las_polys1)
  sf::st_crs(las_polys1_sf) = sf::st_crs(proc_poly_sf)
  ex_las <- sf::st_intersects(las_polys1_sf, proc_poly_sf,sparse=T)
  names(ex_las)=las_polys1$file_path
  ex_las1 = ex_las[lapply(ex_las,length)>0]
  print("completed: extract las polygons");print(Sys.time())

  #create dataframe from dtm and las intersections on tiles
  print(paste("create data.frame from dtm and las intersections on tiles steps 1 and 2 (start):",as.character(Sys.time())))
  tiles_las_df=data.frame(data.table::rbindlist(mapply(function(layer,file){data.frame(layer,las_file=file,stringsAsFactors=F)},ex_las1,names(ex_las1),SIMPLIFY=F)))
  tiles_dtm_df = data.frame(data.table::rbindlist(mapply(function(layer,file){data.frame(layer,dtm_file=file,stringsAsFactors=F)},ex_dtm1,names(ex_dtm1),SIMPLIFY=F)))
  print(paste("Create data.frame from dtm and las intersections on tiles steps 1 and 2 (end):",as.character(Sys.time())))

  print(paste("create data.frame from dtm and las intersections on tiles steps 3 and 4 (start):",as.character(Sys.time())))
  tiles_dtm_agg=aggregate(dtm_file ~ layer,data=tiles_dtm_df,FUN=function(x)paste(unique(x),collapse=","))
  tiles_las_agg=aggregate(las_file ~ layer,data=tiles_las_df,FUN=function(x)paste(unique(x),collapse=","))
  print(paste("create data.frame from dtm and las intersections on tiles steps 3 and 4 (end):",as.character(Sys.time())))

  print(paste("merge dtm and las tile (start) :",as.character(Sys.time())))
  tiles_las_dtm = merge(tiles_las_agg,tiles_dtm_agg,by="layer")
  print(paste("merge dtm and las tile (end) :",as.character(Sys.time())))

  #add tile bounds
  tiles_coords = merge(x=tiles_las_dtm,y=xy,by.x="layer",by.y="layer")
  names(tiles_coords)[names(tiles_coords)=="layer"] = "tile_id"
  crd = tiles_coords[,c("x","y")]
  ts2 = tile_size/2
  bbx = data.frame(
    mnx = crd[,"x"] - ts2
    ,mny = crd[,"y"] - ts2
    ,mxx = crd[,"x"] + ts2
    ,mxy = crd[,"y"] + ts2
  )
  tiles_bbx = data.frame(tiles_coords,bbx)

  #create polys from bboxs and write to file
  tile_polys0 = bbox2polys(tiles_bbx[,c("tile_id","mnx","mxx","mny","mxy")])
  row.names(tiles_bbx) = tiles_bbx[,c("tile_id")]
  tile_polys1 = sp::SpatialPolygonsDataFrame(tile_polys0,tiles_bbx)

  #create config file
  df_config = data.frame(
    dir_out = dir_out
    ,layer_project = layer_project
    ,layer_config  =  layer_config
    ,layer_las_buf = "las_tiles_bfr"
    ,layer_dtm_buf = "dtm_tiles_bfr"
    ,tile_buffer = ts2

    ,dir_las = dir_las
    ,dir_dtm = dir_dtm
    ,project_dtm  = project_dtm
    ,project_las  = project_las

    ,dtm_year  = dtm_year
    ,las_year  = las_year
    ,n_las = nrow(las_polys)
    ,n_dtm = nrow(dtm_polys)
    ,n_tile = nrow(tile_polys1)
    ,origin_x = raster::origin(proc_rast)[1]
    ,origin_y = raster::origin(proc_rast)[2]

    ,overwrite_project  =  overwrite_project
    ,xmn  = xmn
    ,ymn  = ymn
    ,xmx  = xmx
    ,ymx = ymx
    ,do_scan_dtms = do_scan_dtms
    ,do_scan_las  = do_scan_las
    ,tile_size  = tile_size
    ,pixel_size  = pixel_size
    ,proj4  = proj4
    ,has_mask  = is.na(mask)
  )

  #prepare geopackage details

    #get projection
    sp::proj4string(tile_polys1) = proj4_in
    #convert to sf
    sf_proj = sf::st_as_sf(tile_polys1)
    #get output name
    path_gpkg_out = paste0(dir_out,"/",layer_project,"_RSprj.gpkg")

    #write project polygons to FRESH geopackage - overwrite!
    try(sf::st_write(obj = sf_proj , dsn = path_gpkg_out , layer = layer_project, driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

    #write dtm polygons to geopackage
    sp::proj4string(dtm_polys1) = proj4_in
    sf_dtm_bfr = sf::st_as_sf(dtm_polys1)
    try(sf::st_write(obj = sf_dtm_bfr , dsn = path_gpkg_out , layer = "dtm_tiles_bfr", driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

    #write las polygons - fix names to remove "[.]"  and " "
    las_polys2 = las_polys1
    names(las_polys2@data) = gsub("[.]","_",names(las_polys2@data ))
    names(las_polys2@data) = gsub(" ","_",names(las_polys2@data ))
    las_polys2@data$file_path = normalizePath(as.character(las_polys1@data$file_path), winslash = "/")
    sp::proj4string(las_polys2) = proj4_in
    sf_las_bfr = sf::st_as_sf(las_polys2)
    try(sf::st_write(obj = sf_las_bfr , dsn = path_gpkg_out , layer = "las_tiles_bfr", driver="GPKG",  layer_options = c("OVERWRITE=yes") ))

    #write config table to geopackage
    sqlite_proj = RSQLite::dbConnect(RSQLite::SQLite(), path_gpkg_out)
    smry_write_err = try(RSQLite::dbWriteTable(sqlite_proj ,layer_config , df_config, overwrite = T))
    RSQLite::dbDisconnect(sqlite_proj)

  #save RDS object for redundancy
  l_res = list(config=df_config , project_plys = tile_polys1 ,  dtm_tiles_bfr = dtm_polys1 ,  las_tiles_bfr = las_polys2)
  outRDS = file.path(dirname(path_gpkg_out), gsub("[.]gpkg",".RDS",basename(path_gpkg_out),ignore.case=T))
  saveRDS(l_res,outRDS)

  #return data to users
  if(return) return(l_res)

}



