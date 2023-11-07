#'@title
#'  load csvs into database
#'
#'@description
#'  point at a directory and load fusion derived tabular csv data into an sqlite database
#'
#'@details
#'  <Delete and Replace>
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 1/18/2018 header added \cr
#'1.1 \tab 1/21/2021 parallel writes enabled \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <jacob.strunk@@usda.gov>
#'

#'@param db sqlite database connection
#'@param csv_folder location of csv files
#'@param tb_summary output name of summary table of csv files
#'@param tb_csv table to dump csv files into
#'@param project name of project
#'@param resolution resolution of input data
#'@param units unites of resolution
#'@param proj4 proj4 string of coordinate data
#'@param notes any relevant notes
#'@param skip_loaded should csv files which are already loaded be skipped
#'@param n_load max number of files to load
#'@param use_col_classes read colClasses from first file, and apply to remaining files - big read speedup
#'@param ncore number of parallel threads to use at one time, ssds can handle many
#'@param dir_status a way to report the status of the parallel threads, there is an output file for each input csv with at TRUE or FALSE in the name for write status

#'
#'@return
#'  NULL
#'
#'@examples
#'  library(lasR)
#'  dir_csv="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics1\\gridmetrics_csv\\"
#'    dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\sqlite\\"
#'    dir.create(dir_sqlite)
#'
#'    require(RSQLite)
#'    db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics2.db"),sychronous = "off")
#'
#'
#'    csv_to_sqlite(db=db
#'                  ,csv_folder=dir_csv
#'    )
#'
#'@import RSQLite parallel flock
#'
#'@export
#
#'@seealso \code{\link{run_gridmetrics}}\cr \code{\link{lasR_project}}\cr


csv_to_sqlite=function(
                       db_path = "c:/temp/gridmetrics_compilation.sqlite"
                       ,csv_folder
                       ,tb_summary="gm_summary"
                       ,tb_csv="gm"
                       ,project="naip_2015"
                       ,resolution="66"
                       ,units="feet"
                       ,proj4=""
                       ,notes=""
                       ,skip_loaded=T
                       ,n_load=NA
                       ,use_col_classes=T
                       ,ncore = 4
                       ,dir_status = file.path(csv_folder, "progress_tracking")

                       ){

  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")
  #dir_status_in = backslash(paste(dir_status,"/",proc_time,"/",sep=""))
  if(!dir.exists(dir_status)) dir.create(dir_status,recursive=T)
  if(!dir.exists(dirname(db_path))) dir.create(dirname(db_path),recursive=T)

  db_in = dbConnect( RSQLite::SQLite(), db_path  , sychronous = "normal" )

  if(interactive()) requireNamespace("RSQLite")

  csv_files=list.files(csv_folder,full.names=T,pattern="[.]csv")

  if(length(csv_files)<1) stop("no csv files in 'csv_folder' supplied directory")

  #check for existence of tables
  tb_exist=dbListTables(db_in)

  #prep data table - get column names
  dat0=read.csv(csv_files[1])
  names(dat0)=gsub("elev","ht",gsub("[.]+","_",gsub("^x[.]","",tolower(names(dat0)))))
  names0=names(dat0)
  col_classes=sapply(dat0,class)


  if(!tb_csv %in% tb_exist | !skip_loaded ){

    dbWriteTable(db_in,tb_csv,dat0[0,],overwrite = TRUE,append=F)
    dat1=dbGetQuery(db_in,paste("select * from ",tb_csv,"limit 50"))
    names1=names(dat1)

  }else{

    dat1=dbGetQuery(db_in,paste("select * from ",tb_csv,"limit 50"))
    names1=names(dat1)
  }

  if(!is.na(n_load)){

    csv_files=csv_files[1:min(n_load,length(csv_files))]

  }

  #prep / read summary table
  if(!tb_summary %in% tb_exist | !skip_loaded){

    summ0=data.frame(date=as.character(Sys.Date()),file="",nrows=0,status="")
    dbWriteTable(db_in,tb_summary,summ0[0,],overwrite = TRUE,append=F)

  }else{

    summ0=dbReadTable(db_in,tb_summary)
    files_loaded=basename(csv_files) %in% basename(summ0$file[summ0$status == "completed"])
    csv_files=csv_files[!files_loaded]

  }

  if(length(csv_files)==0) return()

  #prep process
  errs=list()
  n_files=length(csv_files)


  if(ncore > 1){

    clus=parallel::makeCluster(ncore)

    parallel::clusterExport(cl = clus, "db_path", envir = environment())
    parallel::clusterEvalQ(clus,
     {
        library(RSQLite)
        #library(RSForInvt)
        dbGlobal <- dbConnect(SQLite(), dbname = db_path , sychronous = "normal")
        res <- dbSendQuery(dbGlobal, "PRAGMA busy_timeout=50000;")
        gc()
      }
    )

    res=parallel::parLapply(
                  clus
                  ,csv_files
                  ,.fn_read_csv_write_sql
                  ,use_col_classes = use_col_classes
                  ,col_classes = col_classes
                  ,names1 = names1
                  ,tb_csv = tb_csv
                  ,tb_summary = tb_summary
                  ,dir_status = dir_status
                  ,print_status = F
                  ,lock.name = tempfile()
                  )
    gc()
    parallel::stopCluster(clus);gc()

  }else{

    assign("dbGlobal", db_in , envir = .GlobalEnv)

    res=lapply(csv_files
              ,.fn_read_csv_write_sql
              ,use_col_classes = use_col_classes
              ,col_classes = col_classes
              ,names1 = names1
              ,tb_csv = tb_csv
              ,tb_summary = tb_summary
              ,dir_status = dir_status
              ,print_status = T
              ,lock.name = tempfile()
    )

    gc()

  }

  dbDisconnect(db_in)
  closeAllConnections()

}

.fn_read_csv_write_sql = function(
                  csv_file_i
                  ,use_col_classes
                  ,col_classes
                  ,names1
                  ,tb_csv
                  ,tb_summary
                  ,dir_status
                  ,print_status = F
                  ,lock.name

                ){

    db_in_i = get("dbGlobal", envir = .GlobalEnv)


    if(use_col_classes) dati=try(read.csv(csv_file_i,colClasses = col_classes))
    if(!use_col_classes) dati=try(read.csv(csv_file_i))

    if(!class(dati)=="try-error"){

      #match names / number of cols
      dati=dati[,1:length(names1)]
      names(dati)=names1
      dati=dati[ dati[,"total_all_returns"] > -1 , ]

      ll = flock::lock(lock.name)

      #write data
      if(nrow(dati)>0){
        err_i=try(dbWriteTable(dbGlobal,tb_csv,dati,append=T))

        #write summary record
        if(!class(err_i) == "try-error"){
          summ_i=data.frame(date=as.character(Sys.Date()),file=csv_file_i,nrows=nrow(dati),status="completed")
          try(dbWriteTable(dbGlobal,tb_summary,summ_i,append=T))
        }

        is_ok = as.character( !class(err_i) == "try-error" )
        writeLines(as.character(err_i), paste(dir_status,basename(csv_file_i),is_ok,"_writeStatus.txt",sep=""))
      }
      flock::unlock(ll)
    }

    if(print_status) print(paste("complete:",csv_file_i,"at",Sys.time()))

  }


# if(F){
#
#   #library(RSForInvt)
#   dir_csv = "D:\\Temp\\gridmetrics_csv\\"
#   dir_sqlite = "d:\\Temp\\wa_dap_sqlite\\"
#   if(!dir.exists(dir_sqlite)) dir.create(dir_sqlite)
#
#   library(RSQLite)
#   library(DBI)
#
#   #db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2017_NAIPDAP_Metrics.db"),sychronous = "off")
#
#   time_par4 = system.time(
#     csv_to_sqlite(db_path = file.path(dir_sqlite,"2017_NAIPDAP_Metrics_par4.db")
#                   ,csv_folder = dir_csv
#                   #,n_load=150
#                   ,skip_loaded = T
#                   ,use_col_classes = F
#                   ,dir_status = "d:\\Temp\\wa_dap_csv_loadstatus_par4\\"
#                   ,ncore = 15
#     )
#   )
#
#   time_par1 = system.time(
#     csv_to_sqlite(db_path = file.path(dir_sqlite,"2017_NAIPDAP_Metrics_par1.db")
#                   ,csv_folder = dir_csv
#                   #,n_load=150
#                   ,skip_loaded = T
#                   ,use_col_classes = F
#                   ,dir_status = "d:\\Temp\\wa_dap_csv_loadstatus_par1\\"
#                   ,ncore = 1
#     )
#   )
#
#
# }
