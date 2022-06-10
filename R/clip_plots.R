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
#'@param idxyd dataframe with point id, x and y coordinates, and circle diameter
#'@param idxy (optional) dataframe with polygon vertices (x,y) organized by point id
#'@param plot_polys (optional) shapefile path, or sp object with plot boundaries
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
#'@import rgdal plyr rgeos raster lidR data.table parallel
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ",readClipboard())),collapse="\n"))


clip_plots=function(

  idxyd=NA #id,x,y,diameter
  ,idxy=NA #id,xy coordinates of vertices for each polygon
  ,plot_polys=NA #shapefile path, or sp object for plots
  ,id_field_plots="plot"
  ,project_gpkg=NA
  #,project_polys=NA
  ,intersect_gpkg=NA
  ,dir_out=NA
  ,height=T
  ,do_plot=F
  ,return=F
  ,n_core=6
  ,dir_dtm=NA #in case drive paths are wrong (External drives...)
  ,dir_las=NA #in case drive paths are wrong (External drives...)
  ,skip_existing=T
  ,fix_dsm_bug = F # laz files for hood canal DSMs are screwed up

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

  if(!file.exists(dir_out)) try(dir.create(dir_out, recursive=T),silent=T)
  dir_skip=file.path(dir_out,"skip")
  if(!file.exists(dir_skip)) try(dir.create(dir_skip, recursive=T),silent=T)

  if(is.na(intersect_gpkg)){

    dtm_sf = sf::st_read(project_gpkg,"dtm_tiles_bfr")
    las_sf = sf::st_read(project_gpkg,"las_tiles_bfr")
    if(do_plot) proj_polys = sf::st_read(project_gpkg,"RSForInvt_prj")

    print("load project");print(Sys.time())

    #create sp objects for plots
    plot_polys_in=NULL
    if(!is.na(plot_polys[1])){
      if(inherits(plot_polys,"SpatialPolygonsDataFrame")) plot_polys_in=plot_polys
      if(inherits(plot_polys,"character")) plot_polys_in=readOGR(plot_polys,stringsAsFactors=F)#readOGR(dirname(plot_polys),gsub("[.]shp","",basename(plot_polys)))
      if(is.null(plot_polys)) stop("Class of plot_polys must be character or SpatialPolygonsDataFrame")
    }
    if(!is.na(unlist(idxy)[1]) & !inherits(plot_polys_in,"SpatialPolygonsDataFrame")){
      plot_polys_in=points2polys(idxy)
    }
    if(!is.na(unlist(idxyd)[1]) & !inherits(plot_polys_in,"SpatialPolygonsDataFrame")){

      seq1=c(seq(-pi,pi,.1),pi+.1)
      circle1=data.frame(sin(seq1),cos(seq1))
      spl_idxyd=split(idxyd,1:nrow(idxyd))
      idxy=rbind.fill(lapply(spl_idxyd,function(x,circle1)data.frame(id=x[,1],x=circle1[,1]*x[,4]+x[,2],y=circle1[,2]*x[,4]+x[,3]),circle1))
      row.names(idxyd)=idxyd[,1]

      plot_polys_in0=points2polys(idxy)
      plot_polys_in=SpatialPolygonsDataFrame(plot_polys_in0,data=idxyd)

    }

    if(!id_field_plots %in% names(plot_polys_in@data)){stop("argument id_field_plots does not match a column in input plot data")}

    #fix row names
    ids_in = as.character(plot_polys_in@data[,id_field_plots])
    is_dup_ids = sum(duplicated(ids_in)) > 0
    if(is_dup_ids) stop("Supplied ID field for clips is not unique - make non-redundant variable field name")
    if(!is_dup_ids) row.names(plot_polys_in) = as.character(plot_polys_in@data[,id_field_plots])

    print("Get / create Plot Polys");print(Sys.time())

    #convert plots sp to st
    pl_sf = st_as_sf(plot_polys_in)

    #function to merge duplicate records
    .fn_simplify = function(x){
      x_in = x[1,]
      x_in[,"file_path"] = paste(x$file_path,collapse=",")
      x_in
    }

    #intersect plots with dtms
    pl_dtm_sf = st_intersection(pl_sf, dtm_sf[,c("file_path")])
    spl_dtm = split(pl_dtm_sf, pl_dtm_sf[,id_field_plots,drop=T])
    pl_dtm_sf1 = do.call(rbind, lapply(spl_dtm, .fn_simplify))
    pl_dtm_sf2 = pl_dtm_sf1[,c(id_field_plots,"file_path"),drop=T]

    #intersect plots with las
    pl_las_sf = st_intersection(pl_sf, las_sf[,c("file_path")])
    spl_las= split(pl_las_sf, pl_las_sf[,id_field_plots,drop=T])
    pl_las_sf1 = do.call(rbind, lapply(spl_las, .fn_simplify))
    pl_las_sf2 = pl_las_sf1[,c(id_field_plots,"file_path"),drop=T]

    #merge together
    pl_dtm_las = merge(x = merge(pl_sf,pl_dtm_sf2,by = id_field_plots), pl_las_sf2, suffixes = c(".dtm",".las"),by = id_field_plots)
    pl_dtm_las[,"X"] = NULL

  }else{

    pl_dtm_las = st_read(plot_tile_intersect,"pl_dtm_las",stringsAsFactors=F)

  }

  if(do_plot ){
    plot(proj_polys[,1])
    plot(pl_dtm_las[,1],col="red",add=T,lwd=10)
  }

  print("skip existing");print(Sys.time())

  #skip existing files
  if(skip_existing){
    files_out_dir=unlist(c(list.files(dir_out,pattern="[.]laz"),list.files(dir_skip,pattern="[.]laz")))
    if(length(files_out_dir) > 0) {
      out_nms = paste(id_field_plots,paste0(pl_dtm_las[,id_field_plots,drop=T],".laz"),sep="_")
      pl_dtm_las=pl_dtm_las[!out_nms %in% files_out_dir,]
    }
  }

  print("write shapefile");print(Sys.time())
  #write shapefile of intersections
  dir_overlap=file.path(dir_out,"plot_tile_overlap")
  if(!dir.exists(dir_overlap)) dir.create(dir_overlap)
  if(!file.exists(paste(dir_overlap,"pl_dtm_las_intersect.gpkg",sep="\\"))){

    st_write(pl_dtm_las
               , dsn = file.path(dir_overlap,"pl_dtm_las_intersect.gpkg")
               , layer = "pl_dtm_las"
               , driver="GPKG" , layer_options = c("OVERWRITE=yes")
               )

  }

  print("split polygons");print(Sys.time())
  #clip points
  spl_plots=sp::split(pl_dtm_las,1:nrow(pl_dtm_las))

  print("initiate clipping");print(Sys.time())
  if(n_core>1){
    clus=makeCluster(n_core)
    clusterEvalQ(clus,library(RSForInvt))
    res=parLapply(clus,spl_plots,.try_clip_plots,dir_out = dir_out,height=height,id=id_field_plots,fix_dsm_bug=fix_dsm_bug)
    stopCluster(clus)
  }
  if(n_core<2){

    lapply(spl_plots,.try_clip_plots,dir_out = dir_out , height=height,id=id_field_plots,fix_dsm_bug=fix_dsm_bug)

  }

  #.try_clip_plots(x=spl_plots[[2]],dir_out = dir_out , height=height,id=id_field_plots,fix_dsm_bug=fix_dsm_bug)
  print("clip plots");print(Sys.time())


  print("write outputs");print(Sys.time())

  if(return) return(plot_polys_merge)

}
.dup2=function(x,type=c("all","first","last"),value=F,invert=F){

  if(type[1]=="all") index = duplicated(x) | duplicated(x, fromLast=TRUE)
  if(type[1]=="first") index = duplicated(x)
  if(type[1]=="last") index = duplicated(x, fromLast=TRUE)

  if(invert) index = !index

  if(value) return(x[index])
  if(!value) return(index)

}

.try_clip_plots=function(...){

  #require(lasR)

  .clip_plots=function(x,id,dir_out,return=F,height=T, fix_dsm_bug = F){

    if(interactive()){
      require(lidR)
      #require(lasR)
      require(plyr)
    }

    las_files_in = grep("[.]la.$",as.character(unlist(strsplit(x[,"file_path.las",drop=T],",")[1])),value=T)
    dtm_files_in = grep("[.].{,4}$",unlist(strsplit(x[,"file_path.dtm",drop=T],",")[1]),value=T)
    las_in=readLAS(files = las_files_in)
    if(fix_dsm_bug) las_in@header@PHB['Header Size'] = 235
    if(grepl("[.]dtm$",dtm_files_in[1])){
      dtm_in = read_dtm(dtm_files_in)
    }else{

      if(length(dtm_files_in)>1) dtm_in = do.call(function(...)mosaic(... , fun=mean , na.rm=T , tolerance = 1.5), lapply(dtm_files_in,raster))
      else dtm_in = raster(dtm_files_in)

    }
    #match projections - dtm
    crs(dtm_in) = crs(x)
    st_crs(las_in) = st_crs(x)

    #clip dtm with buffer and las
    dtm_poly = try(crop(dtm_in,st_buffer(x,20)))
    las_poly = clip_roi(las_in, x , inside = TRUE)

    #cat error
    if(class(dtm_poly)=="try-error"){ warning("plot and dem do not intersect, plot: ",x@data[,1]);return() }

    #clean up
    rm(las_in); gc()

    #write empty file if there are no points in las_poly
    is_skip = class(las_poly)!="LAS"
    if(!is_skip) is_skip = length(las_poly$X) == 0
    if(is_skip){
      skip_file=file.path(dir_out,"skip",paste(id,"_",x@data[1,id],".laz",sep=""))
      file.create(skip_file)
      return()
    }
    if(height) las_hts = normalize_height(las_poly, dtm_poly)
    if(!height) las_hts = las_poly

    #write to file
    if(!dir.exists(dir_out)) dir.create(dir_out)

    if(class(las_hts)=="LAS"){
      out_file_i=file.path(dir_out,paste(id,"_",x[1,id,drop=T],".laz",sep=""))
      err=try(writeLAS(las_hts,out_file_i))
      print(paste("finished writing",out_file_i))
    }else{
      skip_file=file.path(dir_out,"skip",paste(id,"_",x@data[1,id],".laz",sep=""))
      file.create(skip_file)
    }
    gc()

  }

  try(.clip_plots(...))

}

