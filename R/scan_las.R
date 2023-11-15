#'@title
#'  scan las files and add them to summary table
#'
#'@description
#'  scan las files and add them to summary table
#'
#'@details
#'  scan las files and add them to summary table
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@fs.fed.us>
#'
#'@param project name of lidar project
#'@param project_year year of lidar project
#'@param wkt2 wkt2 string
#'@param dir_las where to find lidar files
#'@param pattern pattern to use in searching for las files
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
#'   ,las_ids - data.frame with as many rows as las files
#'   ,plys - SpatialPolygonsDataFrame with all of las tile extents
#'  )
#'
#'@examples
#'
#'scan_las(project="test1", project_year="2015",dir_las="C:\\temp\\lidar_test\\",con=con_inv)
#'
#'@import sf uuid lidR DBI
#'
#'@export
#
#'@seealso \code{\link{scan_dtm}}\cr \code{\link{read_las}}\cr


scan_las=function(
    project = "some_project"
    ,project_year = "2099"
    ,wkt2 = NA
    ,dir_las = ""
    ,dir_out = paste0(dir_las,"/manage_las/")
    ,recursive = F
    ,pattern = "[.]la(s|z)$"
    ,notes = ""
    ,create_polys = T
    ,return = F
    ){

  requireNamespace("uuid")
  requireNamespace("RSQLite")
  requireNamespace("lidR")
  requireNamespace("sf")


  if(length(dir_out)>1) stop("length of \"dir_out\" greater than 1 - please specify a single output directory")

  proc_date=Sys.time()

  files_las = unlist(lapply(pattern,function(x) list.files(dir_las,full.names=T,recursive=recursive,include.dirs = FALSE,pattern=x,ignore.case = T)))
  if(length(files_las)==0) stop("'scan_las' argument dir_las is not a directory or is empty")

  #prepare / read project_id file
  project_id_csv = file.path(dir_out ,"project_id.csv")
  las_id_csv = file.path(dir_out,"las_id.csv")
  las_gpkg = file.path(dir_out,"manage_las.gpkg")

  #create out directory if missing
  if(!dir.exists(dir_out)) dir.create(dir_out,recursive=T)

  #get projection if missing - currently no good way to get wkt2 simple name
  if(is.na(wkt2)) wkt2 = sf::st_crs( lidR::readLASheader(files_las[1]))

  #create or connect to geopackage
  con_gpkg = RSQLite::dbConnect(RSQLite::SQLite(), las_gpkg)
  tables_gpkg = RSQLite::dbListTables(con_gpkg)

  #test for files
  exist_dir_out = dir.exists(dir_out)
  exist_project_id_csv = file.exists(project_id_csv)
  exist_las_id_csv = file.exists(las_id_csv)

  exist_project_id_gpkg = "project_id" %in% tables_gpkg
  exist_las_id_gpkg = "las_id" %in% tables_gpkg
  exist_las_ply_gpkg = "ply_exts" %in% tables_gpkg

  #if(exist_project_id_csv){
  if(exist_project_id_gpkg){

    #project_id_df=read.csv(project_id_csv,stringsAsFactors = F)
    project_id_df = RSQLite::dbReadTable( con_gpkg , "project_id" , stringsAsFactors = F )

  }else{

    project_id_df = data.frame(
      project_id = uuid::UUIDgenerate(use.time = NA)
      ,project = project
      ,project_year = project_year
      ,load_date = proc_date
      ,file_path = dir_las
      ,notes = notes
      ,wkt2 = as.character(wkt2)
    )
    write.csv(project_id_df, project_id_csv,row.names=F)

    RSQLite::dbWriteTable(con_gpkg, "project_id" ,project_id_df )

  }

  #if(exist_las_id_csv){
  if(exist_las_id_gpkg){

    #las_id_df = read.csv(las_id_csv,stringsAsFactors = F)
    las_id_df = RSQLite::dbReadTable(con_gpkg , "las_id" , stringsAsFactors = F)

  }else{

    las_id_df = data.frame()

  }

  proj_id=project_id_df[1,"project_id"]

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory las/laz files."
  disclaimer_txt=paste(dir_out,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #check if las files exist / are already in las_id_df
  names_las=basename(files_las)
  names_las_exist = names_las %in% las_id_df$file_name
  las_update = sum(!names_las_exist) > 0

  #update las
  if(las_update){

    #identify missing records
    files_las = files_las[!names_las_exist]

    #get lidar headers
    headers = read_header(files_las)

    #prep data for database
    names(headers)=gsub("max","max_",gsub("min","min_",tolower(names(headers))))
    headers[,c("min_x","max_x","min_y","max_y")] = sapply(headers[,c("min_x","max_x","min_y","max_y")], as.numeric)
    headers[,"project_id"]=proj_id
    headers[,"project"]=project
    headers[,"project_year"]=project_year
    headers[,"las_id"]=sapply(1:nrow(headers),function(x)uuid::UUIDgenerate())
    headers[,"file_name"]=basename(files_las)
    headers[,"file_path"]=files_las
    headers[,"load_date"]=proc_date
    headers[,"notes"]=paste(notes,collapse="T")

    if(nrow(las_id_df) > 0) las_id_df=rbind(headers,las_id_df[,names(headers)])
    else las_id_df = headers

    write.csv(las_id_df,las_id_csv,row.names=F)
    RSQLite::dbWriteTable( con_gpkg , "las_id" , las_id_df )

  }

  #if(exist_las_ply_gpkg) try(dbSendQuery(con_gpkg, "drop table ply_exts"))
  RSQLite::dbDisconnect(con_gpkg)

  if(create_polys){

    #scrape off bad records
    bad_files=apply(las_id_df[,c("min_x","max_x","min_y","max_y")],1,function(x)any(is.na(x)) )
    las_id_df1=las_id_df[!bad_files,]

    #create sf extent polygons
    ply_exts = bbox2polys(las_id_df1, wkt2 = wkt2)

    # #testng
    # if(F){
    #
    #   dplyr::filter(las_id_df1, file_path == "H:/projects/NAIP_DAP_OR/2022_NAIP_DAP_OR/DSM_update//Infocloud_set02/PointCloud_01183.laz")
    #   laz_test = lidR::readLASheader("H:/projects/NAIP_DAP_OR/2022_NAIP_DAP_OR/DSM_update//Infocloud_set02/PointCloud_01183.laz")
    #
    # }


    #save outputs
    path_polys_rds=paste(dir_out,"ply_exts.rds",sep="")
    try(saveRDS(ply_exts,path_polys_rds))
    names(ply_exts) = gsub("[.]","_",names(ply_exts))
    names(ply_exts) = gsub(" ","_",names(ply_exts))
    try(sf::st_write(obj = ply_exts , dsn = las_gpkg , layer = "las_polys", driver="GPKG" , append=FALSE))

  }

  if(return) return(list(project_id = project_id_df, las_ids = las_id_df  , plys = ply_exts))

}


