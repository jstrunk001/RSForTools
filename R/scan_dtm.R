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
#'@param wkt2 set wkt2 projection - new standard
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
#'@import sf uuid DBI
#'
#'@export
#
#'@seealso \code{\link{scan_las}}\cr \code{\link{read_dtm}}\cr \code{\link{read_dtm_header}}\cr

scan_dtm=function(
  project="wa_dtm"
  ,project_year="2099"
  ,wkt2=NA
  ,dir_dtm=""
  ,dir_out = paste0(dir_dtm,"/manage_las/")
  ,recursive = F
  ,pattern="[.](dtm|tif|img)$"
  ,notes=""
  ,create_polys=T
  ,update = T
  ,return = F
  ,debug=F

){


  #can only
  if(length(dir_out)>1) stop("length of \"dir_out\" greater than 1 - please specify a single output directory")

  require("uuid")
  require("RSQLite")

  proc_date=Sys.time()

  files_dtm = unlist(lapply(pattern,function(x) list.files(dir_dtm,full.names=T,recursive=recursive,include.dirs = FALSE,pattern=pattern,ignore.case = T)))
  if(length(files_dtm)==0) stop("'scan_dtm' argument dir_dtm is not a directory or is empty")
  if(debug) files_dtm = sample(files_dtm,50)


  #prepare / read project_id file
  project_id_csv=paste(dir_out,"project_id.csv",sep="")
  dtm_id_csv=paste(dir_out,"dtm_id.csv",sep="")
  dtm_gpkg=file.path(dir_out,"manage_dtm.gpkg",sep="")

  #create out directory if missing
  if(!dir.exists(dir_out)) dir.create(dir_out,recursive=T)

  #create or connect to geopackage
  if(!update){
    unlink(dtm_gpkg)
  }
  con_gpkg = dbConnect(RSQLite::SQLite(), dtm_gpkg)
  tables_gpkg = dbListTables(con_gpkg)

  #Test for files
  #exist_dir_out=dir.exists(dir_out)
  exist_project_id_csv=file.exists(project_id_csv)
  exist_dtm_id_csv=file.exists(dtm_id_csv)

  exist_project_id_gpkg = "project_id" %in% tables_gpkg
  exist_dtm_id_gpkg = "dtm_id" %in% tables_gpkg
  exist_dtm_ply_gpkg = "dtm_polys" %in% tables_gpkg

  #if(exist_project_id_csv){
  if(exist_project_id_gpkg){

    #project_id_df=read.csv(project_id_csv)
    project_id_df = dbReadTable( con_gpkg , "project_id" , stringsAsFactors = F )

  }else{

    #make fresh project id table
    project_id_df=data.frame(
      project_id=UUIDgenerate(use.time = NA)
      ,project=project
      ,project_year=project_year
      ,load_date=proc_date
      ,file_path=dir_dtm #paste(dir_dtm,collapse=",")
      ,notes=notes
      ,wkt2=wkt2
    )

    #write to file
    write.csv(project_id_df, project_id_csv,row.names=F)
    dbWriteTable(con_gpkg, "project_id" ,project_id_df )

  }

  #prepare repository for dtm headers
  if(exist_dtm_id_gpkg){
    dtm_id_df = dbReadTable(con_gpkg , "dtm_id" , stringsAsFactors = F)
  }else{
    dtm_id_df = data.frame()
  }
  proj_id=project_id_df[1,"project_id"]

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory dtm files."
  disclaimer_txt=paste(dir_out,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #check if dtm files exist
  names_dtm=basename(files_dtm)
  names_dtm_exist = names_dtm %in% dtm_id_df$file_name
  dtm_update = sum(!names_dtm_exist) > 0

  #update dtms
  if(dtm_update){

    #identify missing records
    files_dtm=files_dtm[!names_dtm_exist]

    #get .dtm headers
    is_dtm = grepl("[.]dtm$",files_dtm)
    if(sum(is_dtm ) > 0){
      headers_dtm = read_dtm_header(files_dtm[is_dtm])
      headers_dtm[,"min_x"]=headers_dtm[,"ll_x"]
      headers_dtm[,"min_y"]=headers_dtm[,"ll_y"]
      headers_dtm[,"max_x"]=headers_dtm[,"min_x"]+headers_dtm[,"n_cols"]*headers_dtm[,"col_spacing"]
      headers_dtm[,"max_y"]=headers_dtm[,"min_y"]+headers_dtm[,"n_rows"]*headers_dtm[,"row_spacing"]
      headers_dtm[,"ll_x"]=NULL
      headers_dtm[,"ll_x"]=NULL
    } else {
      headers_dtm = read.csv(text="project_id")
    }
    #get raster headers
    if(sum(!is_dtm ) > 0){
      headers_rast = plyr::rbind.fill(lapply(files_dtm[!is_dtm],.read_rast_header))
    }else{
      headers_rast = read.csv(text="project_id")
    }

    #merge .dtm and raster headers
    headers=rbind.fill(headers_dtm,headers_rast)

    #prep data for database
    headers[,"project_id"]=proj_id
    headers[,"project"]=project
    headers[,"project_year"]=project_year
    headers[,"dtm_id"]=sapply(1:nrow(headers),function(x)UUIDgenerate())
    headers[,"file_name"]=basename(files_dtm)
    headers[,"file_path"]=files_dtm
    headers[,"load_date"]=as.character(proc_date)
    headers[,"notes"]=notes

    #override wkt2 from rasters - especially .dtm and rasters without projections
    if(!is.na(wkt2)) headers[,"wkt2"] = wkt2

    #merge old and updated dtms
    if(nrow(dtm_id_df) > 0){
      dtm_id_df=plyr::rbind.fill(headers,dtm_id_df[,names(headers)[names(headers)%in% names(dtm_id_df)]])
    }else{
      dtm_id_df = headers
    }

    #write results
    write.csv( dtm_id_df , dtm_id_csv )
    dbWriteTable( con_gpkg , "dtm_id" , dtm_id_df, overwrite = T )

  }

  dbDisconnect(con_gpkg)

  if(create_polys){

    wkt2match = length(unique(dtm_id_df$wkt2))==1
    if(!wkt2match) warning("scan_dtm, create_polys=T: there are DTMS with different projections in inputs")

    #create sf extent polygons
    ply_exts = bbox2polys(dtm_id_df, wkt2 = wkt2)

    #save outputs
    #current force overwrite
    if(dtm_update & F){
      # polys_rds=paste(dir_out,"dtm_polys.rds",sep="")
      # try(sf::st_write(obj = sf_dtms , dsn = dtm_gpkg , layer = "dtm_polys", driver="GPKG" , append=FALSE ))
      # try(saveRDS(sf_dtms,polys_rds))
    }else{
      polys_rds=paste(dir_out,"dtm_polys.rds",sep="")
      try(sf::st_write(obj = ply_exts , dsn = dtm_gpkg , layer = "dtm_polys", driver="GPKG" , append=FALSE ))
      try(saveRDS(ply_exts,polys_rds))
    }
  }

  if(return) return(list(project_id = project_id_df, dtm_ids = dtm_id_df  , sp_dtms = dtm_polys, sf_dtms = sf_dtms))


}

.read_rast_header = function(x){
  r_in = try(raster(x), silent =T )
  ext_in = extent(r_in)
  v_ext = as(ext_in,"vector")
  df_ext = as.data.frame(t(v_ext ))
  names(df_ext) = c("min_x","max_x","min_y","max_y")
  df_ext$filename = x
  df_ext$directory = dirname(x)
  df_ext$n_cols = r_in@ncols
  df_ext$n_rows = r_in@nrows
  df_ext$col_spacing = xres(r_in)
  df_ext$row_spacing = yres(r_in)
  df_ext$wkt2 = sf::st_crs(r_in)
  df_ext
}
