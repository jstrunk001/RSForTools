#'@title
#'  scan a directory of dtm files
#'
#'@description
#'  scan a directory of dtm files
#'
#'@details
#'  scan a directory of dtm files
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param project name of lidar project
#'@param project_year year of lidar project
#'@param wkt2 set wkt2 projection if not available from dtms
#'@param dir_dtm where to find dtm files
#'@param recursive T/F recurse into dir_dtm subdirectories ?
#'@param pattern pattern to use in searching for dtm files
#'@param notes and descriptionn that may be helpful in using a project
#'@param create_polys output shapefiles of polygon bboxes
#'@param return T/F return objects
#'
#'@return
#'  NULL
#'
#'  or
#'
#'  list(
#'   project_id - data.frame with one one row
#'   ,dtm_ids - data.frame with as many rows as dtms
#'   ,plys - SpatialPolygonsDataFrame with all of dtm extents
#'  )
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import sf uuid DBI terra
#'
#'@export
#
#'@seealso \code{\link{scan_las}}\cr \code{\link{read_dtm}}\cr \code{\link{read_dtm_header}}\cr

scan_dtm=function(
  project="some_dtm"
  ,project_year="2099"
  ,wkt2=NA
  ,dir_dtm=""
  ,dir_out = paste0(dir_dtm,"/manage_dtm/")
  ,recursive = F
  ,pattern="[.](dtm|tif|img)$"
  ,notes=""
  ,update = T
  ,return = F
  ,debug=F

){

  #can only
  dir_out=dir_out[1]

  require("uuid")
  require("RSQLite")

  proc_date=Sys.time()

  files_dtm = unlist(lapply(pattern,function(x) list.files(dir_dtm,full.names=T,recursive=recursive,include.dirs = FALSE,pattern=pattern,ignore.case = T)))
  if(length(files_dtm)==0) stop("'scan_dtm' argument dir_dtm is not a directory or is empty")
  if(debug) files_dtm = sample(files_dtm,50)

  #prepare / read project_id file
  project_id_csv = file.path(dir_out,"project_id.csv")
  dtm_id_csv = file.path(dir_out,"dtm_id.csv")
  dtm_gpkg = file.path(dir_out,"manage_dtm.gpkg")

  #create out directory if missing
  if(!dir.exists(dir_out)) dir.create(dir_out,recursive=T)

  #create or connect to geopackage
  if(!update){
    unlink(dtm_gpkg)
  }
  con_gpkg = dbConnect(RSQLite::SQLite(), dtm_gpkg)
  tables_gpkg = dbListTables(con_gpkg)

  #Test for files
  exist_project_id_csv = file.exists(project_id_csv)
  exist_dtm_id_csv = file.exists(dtm_id_csv)
  exist_project_id_gpkg = "project_id" %in% tables_gpkg
  exist_dtm_ply_gpkg = "dtm_polys" %in% tables_gpkg

  #if(exist_project_id_csv){
  if(exist_project_id_gpkg){
    project_id_df = dbReadTable( con_gpkg , "project_id" , stringsAsFactors = F )
  }
  if(!exist_project_id_gpkg){

    #make fresh project id table
    project_id_df=data.frame(
      project_id=UUIDgenerate(use.time = NA)
      ,project=project
      ,project_year=project_year
      ,load_date=proc_date
      ,file_path=dir_dtm #paste(dir_dtm,collapse=",")
      ,notes=notes
      #,wkt2=wkt2
    )

    #write to file
    write.csv(project_id_df, project_id_csv,row.names=F)
    dbWriteTable(con_gpkg, "project_id" ,project_id_df )

  }

  #close database to prepare to write spatial data
  dbDisconnect(con_gpkg)

  #prepare repository for dtm headers
  if(exist_dtm_ply_gpkg){
    dtm_id_df = sf::st_read(dtm_gpkg , "dtm_poly" , stringsAsFactors = F)
  }
  if(!exist_dtm_ply_gpkg){
    dtm_id_df = data.frame()
  }

  #get id for this project
  this_proj_id=project_id_df[1,"project_id"]

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory dtm files."
  disclaimer_txt=paste(dir_out,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #check if dtm files exist
  names_dtm = basename(files_dtm)
  names_dtm_exist = names_dtm %in% dtm_id_df$file_name
  dtm_update = sum(!names_dtm_exist) > 0

  #create template for headers
  template_header = read.csv(text="dtm_id,project_id,project,project_year,format,load_date")

  #update dtms
  if(dtm_update){

    #identify missing records
    files_dtm=files_dtm[!names_dtm_exist]
    template_header[1:length(files_dtm),"dtm_id"] = sapply(1:length(files_dtm),function(x)UUIDgenerate())

    #get fusion dtm headers
    is_dtm = grepl("[.]dtm$",files_dtm)
    if(sum(is_dtm ) > 0){

      dtm_temp = read_dtm_header(files_dtm[is_dtm])

      #update with separate function like terra rasters
      headers_dtm = template_header
      headers_dtm[,"min_x"] = dtm_temp[,"ll_x"]
      headers_dtm[,"min_y"] = dtm_temp[,"ll_y"]
      headers_dtm[,"max_x"] = dtm_temp[,"min_x"]+dtm_temp[,"n_cols"]*dtm_temp[,"col_spacing"]
      headers_dtm[,"max_y"] = dtm_temp[,"min_y"]+dtm_temp[,"n_rows"]*dtm_temp[,"row_spacing"]
      headers_dtm[,"n_cols"] = dtm_temp[,"n_cols"]
      headers_dtm[,"n_rows"] = dtm_temp[,"n_rows"]
      headers_dtm[,"resx"] = dtm_temp[,"col_spacing"]
      headers_dtm[,"resy"] = dtm_temp[,"row_spacing"]
      polys_dtm = bbox2polys(headers_dtm, wkt2 = wkt2)

      polys_dtm$format="fusion"

    }
    if(sum(is_dtm ) == 0){
      polys_dtm = template_header[0,]
    }

    #get raster headers using terra
    if(sum(!is_dtm ) > 0){
      dtm_temp = plyr::rbind.fill(lapply(files_dtm[!is_dtm],.read_rast_header))
      polys_rast = data.frame(template_header,dtm_temp)
      polys_rast$format="terra"
    }
    if(sum(!is_dtm ) == 0){
      polys_rast = template_header[0,]
    }

    #merge .dtm and raster headers
    headers = rbind( polys_dtm , polys_rast )

    #prep data for database
    headers[,"project_id"] = this_proj_id
    headers[,"project"] = project
    headers[,"project_year"] = project_year
    headers[,"dtm_id"] = sapply(1:nrow(headers),function(x)UUIDgenerate())
    headers[,"file_name"] = basename(files_dtm)
    headers[,"file_path"] = files_dtm
    headers[,"load_date"] = as.character(proc_date)
    headers[,"notes"] = notes

    #merge .dtm and raster headers
    headers_sf = sf::st_as_sf(headers)

    #override wkt2 from rasters - especially .dtm and rasters without projections
    if(!is.na(wkt2)) sf::st_crs = wkt2

    #merge old and updated dtms
    if(nrow(dtm_id_df) > 0){
      dtm_id_sf=rbind(headers_sf,dtm_id_df[,names(headers)[names(headers)%in% names(dtm_id_df)]])
    }
    if(nrow(dtm_id_df) == 0){
      dtm_id_sf = headers_sf
    }
    #create df version without geometry
    dtm_id_df = as.data.frame(dtm_id_sf)
    dtm_id_df$geometry=NULL

    #write results in various forms
    write.csv(dtm_id_df , dtm_id_csv )
    polys_rds=paste(dir_out,"dtm_polys.rds",sep="")
    try(saveRDS(dtm_id_sf,polys_rds))
    try(sf::st_write(obj = dtm_id_sf , dsn = dtm_gpkg , layer = "dtm_polys", driver="GPKG" , append=FALSE ))

  }

  if(return) return(list( project_id = project_id_df , dtm_ids = dtm_id_df  , sp_dtms = dtm_polys , sf_dtms = sf_dtms ))

}

.read_rast_header = function(x){

  r_in = try(terra::rast(x), silent =T )

  #grab extent and rename
  ext_in = terra::ext(r_in)
  df_ext = data.frame(t(as.vector(ext_in)))
  names(df_ext) = c("min_x","max_x","min_y","max_y")

  #get attributes
  r_details=data.frame(
      filename = x
      ,directory = dirname(x)
      ,n_cells = terra::ncell(r_in)
      ,n_cols = terra::ncol(r_in)#@ncols
      ,n_rows = terra::nrow(r_in)
    )
  r_details[,c("resx","resy")] = terra::res(r_in)

  #return sf object
  sf::st_sf(
    r_details
    ,df_ext
    ,geometry = sf::st_as_sfc(sf::st_bbox(r_in))
  )

}
