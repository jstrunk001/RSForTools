#'@title
#'  run gridmetrics across a project
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
#'1.0 \tab 2018-01-28 Header added \cr
#'1.1 \tab 2020-03-17 force csv or geopackage \cr
#'1.2 \tab 2021-04-30 remove fast cache and stub for processing with lidR \cr
#'1.3 \tab 2021-07-12 split process across multiple input disk \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param project_gpkg polygon file of intersections created by project_make() function
#'@param n_core number of corest to run process on
#'@param gridmetrics_path where is gridmetrics.exe (FUSION)
#'@param heightbreak Height break for cover calculation
#'@param cellsize output raster resolution
#'@param minht set minht for gridmetrics.ex
#'@param first T/F use only first returns
#'@param intensity T/F include intensity metrics
#'@param outlier c(-5,500) range of inclusion
#'@param fusion_switches other fusion switches as a string e.g. "/noground"
#'@param xmn,xmx,ymn,ymx set extent for analysis
#'@param new_dtm_path = c(from = NA, to = NA) rename input file paths (e.g. files have moved)
#'@param new_las_path = c(from = NA, to = NA) rename input file paths (e.g. files have moved)
#'@param skip_existing skip tiles if they already have output csvs
#'@param table output folder name
#'@param existing_coms path to existing batch comamnds, incase processing was interrupted the first time
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'  gmi=run_gridmetrics(
#'  project_gpkg="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\RSForInvt_project003.gpkg"
#' ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_07\\"
#' ,dir_dtm="c:\\usgs_dtms\\dtms\\"
#' ,dir_las="D:\\naip_2015_laz\\"
#' ,n_core=10
#' ,existing_coms="C:\\dir_proc\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt"
#' ,n_cache=400
#' )
#'

#'
#'@import parallel sf terra
#'
#'@export
#
#'@seealso \code{\link{project_make}}\cr \code{\link{lidR::gridmetrics}}\cr
#'
#'Desired updates
#' 1. fix issues with mismatched tile size and cell size
#' 2. allow dynamic update of tile sizes
#'
#'
#'

run_gridmetrics=function(

   proj_polys = NA
  ,proj_gpkg_path = NA
  ,layer_proj_polys = "RSForInvt_prj"
  ,layer_proj_config = "RSForInvt_config"
  ,dir_out = "c:/dir_proc/test_project/gridmetrics"
  ,n_core = 4
  ,gridmetrics_path = "c:\\fusion\\gridmetrics64.exe"
  ,heightbreak = 3
  ,minht = NA
  ,cellsize = 66
  ,first = T
  ,intensity = F
  ,outlier = c(-5,400)
  ,fusion_switches = "/nointensity /failnoz"
  ,xmn = NA , xmx= NA , ymn=NA , ymx=NA


  ,new_dtm_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)
  ,new_las_path = list(from = NA, to = c(NA,NA), prop=c(.6,.4)) #in case drive paths are wrong (e.g. External drives...)

  ,skip_existing = T
  ,subset_tiles = NA

  ,existing_coms = c(NA,"C:\\dir_proc\\run_gridmetrics\\2017Aug17_100740\\all_commands.txt")   #skip setting up new dtm and las files

  ,do_run = T

  ,debug = F

  ,... #additonal arguments to fns

  ){

  warning("If project cell size doesn't match gridmetrics cell size (or multiple), you are probably causing problems")

  if(is.na(minht)) minht = heightbreak

  options(scipen = 999)

  requireNamespace("parallel")
  requireNamespace("raster")

  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create dir_proc folder
  dir_proc = .bs(paste(dir_out,"/processing/",sep=""))
  if(!dir.exists(dir_proc)) try(dir.create(dir_proc,recursive=T))
  gm_out=.bs(paste(dir_out,"/gridmetrics_csv/",sep=""))
  if(!dir.exists(gm_out)) try(dir.create(gm_out,recursive=T))

  #create csv folder dump
  if(is.na(existing_coms[1])) dir_proc_time = .bs(paste(dir_proc,"/",proc_time,"/",sep=""))
  if(!is.na(existing_coms[1])) dir_proc_time = paste(dirname(existing_coms[1]),"/",sep="")
  if(!dir.exists(dir_proc_time)) try(dir.create(dir_proc_time,recursive=T))

  coms_out=file.path(dir_proc_time,"all_commands.txt")

 #load project
  if("sf" %in% class(proj_polys)){
    proj_polys_in = proj_polys
  }

  if(!("sf" %in% class(proj_polys)) & !is.na(proj_gpkg_path ) ){
    proj_polys_in = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_polys )
  }
  if(!("sf" %in% class(proj_polys)) & is.na(proj_gpkg_path ) ) stop("must provide either proj_polys or proj_gpkg_path")

  print("load project");print(Sys.time())
  #fix drive paths in lasR_project

  #rename paths in case files have moved
  if(!is.na(new_dtm_path)[1]){
    new_dtm_path_in = normalizePath(new_dtm_path)
    names(new_dtm_path_in) = names(new_dtm_path)
    dtm_old = normalizePath(proj_polys_in[,"dtm_file",drop=T])
    dtm_new = gsub(new_dtm_path_in["from"], new_dtm_path_in["to"], dtm_old , fixed = T)
    proj_polys_in[,"dtm_file"] = dtm_new
  }

  #testing
  if(F){
    test_id=1500
    tile_test = dplyr::filter(proj_polys_in, tile_id==test_id)
  }


  if(!is.na(new_las_path$from[1])){

    new_las_path_in = new_las_path
    new_las_path_in$from = normalizePath(new_las_path$from )
    new_las_path_in$to = normalizePath(new_las_path$to)

    las_new = normalizePath(proj_polys_in[,"las_file",drop=T])

    #set upd systematic sample
    prop_in = new_las_path$prop / sum(new_las_path$prop )
    N = length(las_new)
    ni = round(prop_in*N)
    if(sum(ni) != N) ni[1] = ni + (N-sum(ni))

    #get systematic samples
    idx = sample.systematic(N,n=ni)

    #iterate across indices and update paths
    for(j in 1:length(new_las_path_in[["to"]])){

      #get observations for out path j
      idx_j = idx == j

      #rename appropriate files
      las_new[idx_j] = gsub(tolower(new_las_path_in[["from"]]), tolower(new_las_path_in[["to"]][j]), tolower(las_new[idx_j]) , fixed = T)
    }
    proj_polys_in[,"las_file"] = las_new
  }

  #skip existing files
  if(skip_existing){
    files_done = list.files(gm_out,pattern="[.]csv")
    ids_done = gsub("tile_|_all.*|_first.*","",files_done)
    files_exist = as.character(proj_polys_in[,"tile_id",drop=T]) %in% ids_done
    proj_polys_in=subset(proj_polys_in,subset=!files_exist)
  }
  if(!is.na(subset_tiles[1])){
    proj_polys_in=subset( proj_polys_in , subset=proj_polys_in$tile_id %in% subset_tiles )
  }

  print("skip files");print(Sys.time())

  #prepare output directory
  proj_polys_in[,"outf"]=.bs(file.path(gm_out,paste0("tile_",proj_polys_in[,"tile_id",drop=T],".csv")))
  print(paste(nrow(proj_polys_in),"tiles to process"))

  #prepare batch commands
    proj_polys_in[,"dtm_txt"] = .bs(paste(dir_proc_time,.bs(proj_polys_in[,"tile_id",drop=T]),"_dtm.txt",sep=""))
    proj_polys_in[,"las_txt"] = .bs(paste(dir_proc_time,.bs(proj_polys_in[,"tile_id",drop=T]),"_las.txt",sep=""))
    proj_polys_in[,"switches"] = paste("/minht:",minht
                              ," /outlier:",paste(outlier,collapse=",")
                              ," /cellbuffer:2 /gridxy:"
                              ,apply(round(proj_polys_in[,c("xmin","ymin","xmax","ymax"),drop=T]),1,paste,collapse=",")
                              ,sep="")

    if(!is.null(fusion_switches)){
      gmpath = paste(paste0('"',gridmetrics_path[1],'"'),fusion_switches)
    }else{
      gmpath = paste0('"',gridmetrics_path[1],'"')
    }

    coms_df=data.frame(gm=gmpath
                         ,sw=proj_polys_in[,c("switches"),drop=T]
                         ,ids=paste("/id:",proj_polys_in[,"tile_id",drop=T],sep="")
                         ,dtms=.bs(proj_polys_in[,c("dtm_txt"),drop=T])
                         ,hb=heightbreak
                         ,cs=cellsize
                         ,outf=.bs(proj_polys_in[,"outf",drop=T])
                         ,las=.bs(proj_polys_in[,"las_txt",drop=T])
                         )

    coms=apply(coms_df,1,paste,collapse=" ")
    print("set up commands");print(Sys.time())

    if(is.na(existing_coms[1]) ){

      writeLines(coms,.bs(coms_out))

      for(i in 1:nrow(proj_polys_in)){
        writeLines(gsub(",","\n",.bs(proj_polys_in[i,"las_file",drop=T])),proj_polys_in[i,"las_txt",drop=T])
        writeLines(gsub(",","\n",.bs(proj_polys_in[i,"dtm_file",drop=T])),proj_polys_in[i,"dtm_txt",drop=T])
      }
      print("create and write list of dtms and las files");print(Sys.time())
    }

    #get identical behavior if debugging
    #the commands are now randomly selected to help with file interference
    rm(proj_polys_in);gc()


    if(do_run){
      if(n_core>1 ){
        print("begin parallel processing");print(Sys.time())

        fn_proc = function(x, dir_out){
          file_out = paste0(dir_out,"/gridmetrics_messages_", Sys.getpid(), ".txt")
          y = try( system(x,intern=T))
          write(paste(y, collapse=" /n" ), file=file_out, append=T)
          gc()
        }
        set.seed(50)
        clus=parallel::makeCluster(n_core)#,setup_strategy = "sequential")
        #parallel::clusterExport(clus,varlist=list("dir_proc_time"), envir = environment())
        #parallel::clusterEvalQ(clus, sink(paste0(dir_proc_time,"/gridmetrics_messages_", Sys.getpid(), ".txt")))
        #clusterEvalQ(clus,{library(RSForInvt);gc()})
        #res=parallel::parLapplyLB(clus,sample(coms,length(coms)),function(x) try( shell(x,intern=T)) );gc()
        res=parallel::parLapplyLB( clus , sample(coms,length(coms)) , fn_proc , dir_out = dir_proc_time );gc()
        #res=parallel::parLapplyLB( clus , sample(coms,length(coms)) , fn_proc , dir_out = dir_out );gc()
        gc();parallel::stopCluster(clus);gc()

      }else{

       #writeClipboard(coms[1])
       #testing if(F) lapply(coms[110:200],shell) ;gc()
       lapply(coms,function(x)try(shell(x))) ;gc()

      }




      print("run fusion (done)");print(Sys.time())
    }

}

  .reslash<- function(x=NA, slash=c("back","forward")){
    if(is.na(x[1])) return(NA)
    if(slash[1]=="back") path <- gsub("\\\\\\\\","\\",gsub("/","\\",x, fixed = TRUE))
    if(slash[1]=="forward") path <- gsub("\\", "/", x, fixed = TRUE)
    return(path)
  }

  #correctly escape back slashes and quote path - a parameterized version o f
  .bs=function(x=NA){
    .reslash(gsub("//","/",.reslash(x,slash="forward")),slash="back")
   }

  # #correctly escape back slashes and quote path
  # .fs=function(x=NA){gsub("//","/",.reslash(x,slash="forward"))}
  #
  # #function to grab names from data.frame, quote them, place commas between them, and send to clipboard
  # .nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep=""); writeClipboard2(x);return(x)}


