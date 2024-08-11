#'@name sqlite_to_raster
#'@title
#'  helpers to convert sqlite table to rasters
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 3/16/2020 Function created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param db a database object, tested for sqlite database
#'@param tb_gm which table has gridmetrics data
#'@param colsxy  names of xy columns
#'@param cols2Raster which columns to grab
#'@param format  raster formate  e.g. .tif , .img etc
#'@param dirOut  where to export rasters
#'@param raster_prefix give a name to rasters for id purposes
#'@param wkt2  crs approach used by terra or sf,
#'@param nproc  number of cores to use in converting to raster
#'@param doDebug run in debug mode and only read 50k rows?
#'@param debugRow number of rows to test / debug on
#'
#'@param thresh character string like "2_50" that matches height representation in column names in tb_gm
#'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'   dir_sqlite = "D:/temp/OR_dap_gridmetrics_sqlite/"
#'   nms_x1b = c('ht_p90','percentage_all_returns_above_3_00','profile_area','ht_mode','ht_stddev','ht_p05','ht_p10','ht_p50','canopy_relief_ratio','ht_mean')
#'   #function not compiled yet, from lasR package, have to source it..
#'   sqlite_to_raster(file.path(dir_sqlite,"OR2022_NAIP_Metrics.db")
#'                    #,cols2Raster = c("ht_p90", "ht_p30","canopy_relief_ratio","percentage_first_returns_above_6_00")
#'                    , dirOut = "d:/temp/or_dap_2022_gridmetrics_raster/"
#'                    , wkt2 = terra::crs("EPSG:6557")
#'                    , cols2Raster = nms_x1b
#'                    , nproc = 5
#'                    , doDebug=T
#'                    )
#'
#'
#'@import terra
#'
#
#'@seealso \code{\link{rasterFromXYZ}}\cr \code{\link{writeRaster}}\cr

#Desired upgrades to this function, something like:
# "select ?? from gm where NOT NULL center_x and NOT NULL center_y")
#
#  update to create raster and assign values to raster instead of creating a raster each time
#
#'@export
#'@rdname sqlite_to_raster
sqlite_to_raster = function(
  db
  ,tb_gm="gm"
  ,colsxy = c("center_x","center_y")
  ,cols2Raster = colsSomeX() # or colsAllX()
  ,coltest = "total_all_returns"
  ,res=c(NA,NA)
  ,format = ".img"
  ,dirOut = "E:\\projects\\2017_NAIP\\rasters\\"
  ,raster_prefix = ""
  ,wkt2 = NA
  ,nproc = 4
  ,doDebug=F
  ,debugRows = 500000
  ,set9999 = c(NA,-9999, 0)

  ,sfmask=NA
  #,skipExisting =T
){

  if(!class(db)=="SQLiteConnection"){
    db_in = DBI::dbConnect(RSQLite::SQLite(), db)
  }else{
    db_in = db
  }

  #setup output directory
  if(!dir.exists(dirOut)) dir.create(dirOut, recursive=T)
   gc()


   options(scipen=10E6)

  #get xy to make base raster
    #sql_xy = paste("select", paste(c(paste(colsxy,paste(" = round(",colsxy,")")), coltest ), collapse=" , "),"from",tb_gm,"where", colsxy[1],"NOT NULL and 'total.all.returns' > 0")

  #make generic sql script
    sql_all = c("select", "REPLACE_ME", paste("from",tb_gm,"where", colsxy[1],"NOT NULL"))

  #add filter for points inside of extent of interest
    if("sf" %in% class(sfmask)){
      bbxsf = sf::st_bbox(sfmask)
      where_xy = paste("and",colsxy[1],">=",floor(bbxsf[1]), "and" , colsxy[1],"<=",ceiling(bbxsf[3]),"and"
                       ,colsxy[2],">=",floor(bbxsf[2]), "and" , colsxy[2],"<=",ceiling(bbxsf[4]))
      sql_all = c(sql_all, where_xy)

    }

    #get all coordinates for gridmetrics rasters
    sql_xy = sql_all
    sql_xy[2] = paste(c(colsxy) , collapse=" , ")
    if(doDebug) sql_xy = c(sql_xy,paste("limit",debugRows,collapse=" "))
    xy = dbGetQuery(db_in , paste(sql_xy,collapse=" ") )

    #debugging stuff
    if(F){
      qry_test =  paste("select * from gm where center_x NOT NULL and center_x = 0 limit 1")
      test = dbGetQuery(db_in,qry_test)
    }

    #round off coordinates
    xy[,colsxy] = round(xy[,colsxy])
    xy[,3]=1

    #convert xy to rasters
    r0 = terra::rast(xy,  type="xyz", crs=wkt2)

    #get cell ids - doesn't work on NA raster
    cells_in = terra::cellFromXY(r0,xy=xy[,colsxy])

    #create binary mask
    if("sf" %in% class(sfmask)){
       sfmask_in = sf::st_transform(sfmask, sf::st_crs(r0))
       #add 1 within polygon mask extent, leave everything else the same
       mask_in = r0
       mask_in[1:terra::ncell(mask_in)] = 1
       mask_in1 = terra::mask(mask_in,mask=sfmask_in,inverse=F,updatevalue=NA)
    }


  #process sequentally
  if(nproc==1){

    print("make individual rasters")
    for(i in 1:length(cols2Raster)){

      print(paste("start:",cols2Raster[i],"at",Sys.time()))
      sqli = sql_all
      sqli[2] = cols2Raster[i]
      if(doDebug) sqli = c(sqli,paste("limit",debugRows,collapse=" "))
      dati = dbGetQuery(db_in,paste(sqli,collapse=" "))

      #mask NA values from gridmetrics
      if(is.na(set9999[1]) ){ dati[dati[,1] == -9999 ,1] = set9999[1] }

      #update raster values
      r0[cells_in] = dati[,1]

      #mask outside polygon
      if("sf" %in% class(sfmask_in)) r0 = r0*mask_in

      #write if not masked
      outi = file.path(dirOut,paste(raster_prefix,cols2Raster[i],format,sep=""))
      names(r0) = cols2Raster[i]
      terra::writeRaster(r0,filename=outi,overwrite=TRUE)

      #provide
      print(paste("complete:",cols2Raster[i],"at",Sys.time()))

    }# end for

  }#end if

  #close database connection, new connections will be formed in each thread
  if(!class(db) == "SQLiteConnection" ) dbDisconnect(db_in)

  #process in parallel
  if(nproc>1){

    if(class(db) == "SQLiteConnection" ) stop("db should be a character path to an sqlite database if nproc > 1")

    #put temp raster onto the disk
    path_temp_rast = file.path(dirOut,"temp_raster.tif")
    terra::writeRaster(r0, path_temp_rast , overwrite=TRUE)

    #test = terra::rast(path_temp_rast)

    #make parallel cluster
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
    clus_in = parallel::makeCluster(nproc)

    #export objects to parallel environment and prepare that environment
    parallel::clusterExport(clus_in, varlist=list("db","cells_in","wkt2","path_temp_rast"), envir = environment())
    #parallel::clusterEvalQ(clus_in, { rExt = terra::rast(path_temp_rast) })

    #export optional mask to parallel environment
    if("sf" %in% class(sfmask)){
      #write mask to disk
      path_temp_mask = file.path(dirOut,"temp_mask.tif")
      terra::writeRaster(mask_in1, path_temp_mask , overwrite=TRUE)

      #load in each cluster
      #parallel::clusterExport(clus_in, varlist=list("path_temp_mask"), envir = environment())
      #parallel::clusterEvalQ(clus_in, { maskExt = terra::rast(path_temp_mask) })
    }



    #read from db and write to disk in parallel
    parallel::parLapplyLB( clus_in
                 , cols2Raster
                 , .fn_proc
                 , db = db
                 , set9999=set9999
                 , dirOut=dirOut
                 , raster_prefix=raster_prefix
                 , format=format
                 , doDebug=doDebug
                 , debugRows=debugRows
                 , tb_gm = tb_gm
                 #, wkt2 = wkt2
                 , colsxy = colsxy
                 , doMask = "sf" %in% class(sfmask)
                 , path_template = path_temp_rast
                 , sql_all = sql_all
                 )

   # if(F)     lapply(
   #                cols2Raster
   #               , .fn_proc
   #               , db = db
   #               , set9999=set9999
   #               , dirOut=dirOut
   #               , raster_prefix=raster_prefix
   #               , format=format
   #               , doDebug=doDebug
   #               , debugRows=debugRows
   #               , tb_gm = tb_gm
   #               #, wkt2 = wkt2
   #               , colsxy = colsxy
   #               , doMask = "sf" %in% class(sfmask)
   #               , path_template = path_temp_rast
   #               )

    #delete temporary processing rasters
    unlink(path_temp_rast)
    if("sf" %in% class(sfmask)) unlink(path_temp_mask)
    parallel::stopCluster(clus_in)

  }

}

#function to process rasters
  .fn_proc=function(nm, path_template,set9999, db,dirOut,raster_prefix,format,doDebug, debugRows,tb_gm, colsxy, doMask, sql_all){

    #get base raster
    ri = terra::rast(path_template)

    # #connect to dabase
    db_in = DBI::dbConnect(RSQLite::SQLite(), db)

    #build sql statement and get data
    sqli = sql_all
    sqli[2] = nm
    if(doDebug) sqli = c(sqli,paste("limit",debugRows,collapse=" "))
    dati = DBI::dbGetQuery(db_in,paste(sqli,collapse=" "))

    #set -9999 values to NA
    if(is.na(set9999[1]) ){
      idx_9999 = dati[,1] == -9999
      idx_9999[is.na(idx_9999)] = F
      dati[idx_9999,1] = set9999[1]
    }

    #assign new values
    ri[get("cells_in",envir = .GlobalEnv)] = dati[,1]
    names(ri) = nm

    #mask if needed
    if(doMask){
      path_temp_mask = file.path(dirOut,"temp_mask.tif")
      maski = terra::rast(path_temp_mask)
      ri = ri*maski
    }

    #write to file
     outi = file.path(dirOut,paste(raster_prefix,nm,format,sep=""))
     terra::writeRaster(ri,outi,overwrite=TRUE)

    #clean up
    DBI::dbDisconnect(db_in)
    rm(ri);gc()

  }


#'@rdname sqlite_to_raster
#'@export
colsAll = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('row','col','center_x','center_y','total_return_count_above_XXTHRESHXX','ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_skewness','ht_kurtosis','ht_aad','ht_l1','ht_l2','ht_l3','ht_l4','ht_l_cv','ht_l_skewness','ht_l_kurtosis','ht_p01','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_XXTHRESHXX','return_2_count_above_XXTHRESHXX','return_3_count_above_XXTHRESHXX','return_4_count_above_XXTHRESHXX','return_5_count_above_XXTHRESHXX','return_6_count_above_XXTHRESHXX','return_7_count_above_XXTHRESHXX','return_8_count_above_XXTHRESHXX','return_9_count_above_XXTHRESHXX','other_return_count_above_XXTHRESHXX','percentage_first_returns_above_XXTHRESHXX','percentage_all_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX_total_first_returns_100','first_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean','identifier')))

#'@rdname sqlite_to_raster
#'@export
colsID = function()return(c('row','col','center_x','center_y','identifier'))

#'@export
#'@rdname sqlite_to_raster
colsAllX = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('total_return_count_above_XXTHRESHXX','ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_skewness','ht_kurtosis','ht_aad','ht_l1','ht_l2','ht_l3','ht_l4','ht_l_cv','ht_l_skewness','ht_l_kurtosis','ht_p01','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_XXTHRESHXX','return_2_count_above_XXTHRESHXX','return_3_count_above_XXTHRESHXX','return_4_count_above_XXTHRESHXX','return_5_count_above_XXTHRESHXX','return_6_count_above_XXTHRESHXX','return_7_count_above_XXTHRESHXX','return_8_count_above_XXTHRESHXX','return_9_count_above_XXTHRESHXX','other_return_count_above_XXTHRESHXX','percentage_first_returns_above_XXTHRESHXX','percentage_all_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX_total_first_returns_100','first_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean')))

#'@rdname sqlite_to_raster
#'@export
colsSomeX = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('ht_minimum','ht_maximum','ht_mean','ht_mode','ht_stddev','ht_variance','ht_cv','ht_iq','ht_aad','ht_l1','ht_l2','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p80','ht_p90','ht_p95','ht_p99','return_1_count_above_XXTHRESHXX','percentage_first_returns_above_XXTHRESHXX','percentage_all_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX_total_first_returns_100','first_returns_above_XXTHRESHXX','all_returns_above_XXTHRESHXX','percentage_first_returns_above_mean','percentage_first_returns_above_mode','percentage_all_returns_above_mean','percentage_all_returns_above_mode','all_returns_above_mean_total_first_returns_100','all_returns_above_mode_total_first_returns_100','first_returns_above_mean','first_returns_above_mode','all_returns_above_mean','all_returns_above_mode','total_first_returns','total_all_returns','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean')))


#'@rdname sqlite_to_raster
#'@export
colsFewX = function(thresh="6_00")return(gsub("XXTHRESHXX",thresh,c('ht_minimum','ht_maximum','ht_mean','ht_stddev','ht_p05','ht_p20','ht_p60','ht_p90','percentage_first_returns_above_XXTHRESHXX')))


if(F){


  options(scipen = 10E6)

  #prevent strings being read as factors
  options(stringsAsFactors = F)

# helpful functions

  #convenience function for more usable version of writeClipboard - overcomes newline issues
  writeClipboard2 = function(x) writeClipboard(charToRaw(paste0(x, ' ')))

  #generic file path editing function - forwards or backwards
  reslash<- function(x=NA, slash=c("back","forward")){
    if(is.na(x)) x = readClipboard()
    if(slash[1]=="back") path <- shQuote(gsub("/","\\\\",gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE))
    if(slash[1]=="forward") path <- shQuote(gsub("\\", "/", x, fixed = TRUE))
    writeClipboard2(path)
    return(path)
  }

  #correctly escape back slashes and quote path - a parameterized version o f
  bs=function(x=NA){reslash(x,slash="back")}

  #correctly escape back slashes and quote path
  fs=function(x=NA){reslash(x,slash="forward")}

  #function to grab names from data.frame, quote them, place commas between them, and send to clipboard
  nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep=""); writeClipboard2(x);return(x)}


  library(RSQLite)
  if(!"oregon" %in% ls()) oregon = sf::st_read("C:/Users/Jacob/Box/sync/data/GIS/OR/oregon_oregonLambert.gpkg")
  # library(RSForTools)
  dir_sqlite = "D:/temp/OR_dap_gridmetrics_sqlite/"
  if(F){
    db <- dbConnect( SQLite() , dbname= file.path(dir_sqlite,"OR2022_NAIP_Metrics.db") , sychronous = "off" )
    ids = dbGetQuery(db, "select distinct identifier from gm")
    DBI::dbListTables(db)
    nms_x = names(dbGetQuery(db, "select * from gm limit 1"))
    nms_x1 = nmsVec(dbGetQuery(db, "select * from gm limit 1"))
  }
  nms_x1b = c('ht_p90','percentage_all_returns_above_3_00','profile_area','ht_mode','ht_stddev','ht_p05','ht_p10','ht_p20','ht_p25','ht_p30','ht_p40','ht_p50','ht_p60','ht_p70','ht_p75','ht_p80','ht_p95','ht_p99','ht_mad_median','ht_mad_mode','canopy_relief_ratio','ht_quadratic_mean','ht_cubic_mean','ht_minimum','ht_maximum','ht_mean')
  nms_x1b = c('ht_p90','percentage_all_returns_above_3_00','profile_area','ht_mode','ht_stddev','ht_p05','ht_p10','ht_p50','canopy_relief_ratio','ht_mean')
  #function not compiled yet, from lasR package, have to source it..
  sqlite_to_raster(file.path(dir_sqlite,"OR2022_NAIP_Metrics.db")
                   #,cols2Raster = c("ht_p90", "ht_p30","canopy_relief_ratio","percentage_first_returns_above_6_00")
                   , dirOut = "d:/temp/or_dap_2022_gridmetrics_raster/"
                   , wkt2 = terra::crs("EPSG:6557")
                   , cols2Raster = nms_x1b
                   , nproc = 10
                   , doDebug=T
                   , debugRows = 5000000
                   , sfmask=oregon
                   )


}
