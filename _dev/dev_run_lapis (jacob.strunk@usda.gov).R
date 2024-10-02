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
#'@param cellsize override the cellsize set in project create
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
#' 3. use config.ini instead of command line arguments ...
#' 4. use subfolders for each tile of processing - hassle but necessary until jonathan updates lapis
#'
#'Note from Jonathan Kane (developer) on implementing bbox approach:
#'
#' As for specifying by an aoi instead of by files, that option does actually
#' exist! But it's incomplete at the moment, so I removed it from the gui to
#' avoid confusion. But it's still an undocumented feature that can be accessed
#' through the command line/ini file. If you specify the four variables
#' aoixmin, aoixmax, aoiymin, and aoiymax, it should limit the processing to
#' the rectangle of interest. By default, you specify these four in the same
#' CRS as the output files, or you can specify which CRS you're using
#' with the variable aoicrs.
#'
#

run_lapis=function(

  # proj_polys = NA
  proj_gpkg_path = NA
  ,layer_proj_polys = "RSForInvt_prj"
  ,layer_proj_config = "RSForInvt_config"
  ,dir_out = "c:/dir_proc/test_project/gridmetrics"
  ,n_core = 4
  ,path_lapis = "c:\\lapis\\lapis.exe"

  ,new_dtm_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)
  ,new_las_path = list(from = NA, to = c(NA,NA), prop=c(.6,.4)) #in case drive paths are wrong (e.g. External drives...)

  ,skip_existing = T
  ,subset_tiles = NA

  ,existing_coms = c(NA,"C:\\dir_proc\\run_gridmetrics\\2017Aug17_100740\\all_commands.txt")   #skip setting up new dtm and las files

  ,do_run = T

  ,debug = F

  #,... #additonal arguments to fns

  ,lapis_args = list(
        "--ini-file" = NULL
      , "--cellsize" = NA
      , "--csm-cellsize" = 66
      , "--out-crs" = NA
      , "--minht" = -10
      , "--maxht" = 400
      , "--class" = NA
      , "--user-units" = NA
      , "-N" = "Run_Name"
      , "--canopy" = 6
      , "--strata" = paste(c(0,5,10,15,20,seq(25,100,25),200, 400),collapse=",")
      , "--adv-point"
      , "--fine-int"
    )

 # ,lapis_args = list(
 #      path_ini = NA
 #      ,path_A = NA
 #      ,metrics_cellsize = 66
 #      ,csm_cellsize = 9
 #      ,out_crs = NA
 #      ,min_ht = -10
 #      ,max_ht = 400
 #      ,las_class = NA
 #      ,user_units = NA
 #      ,run_name = NA
 #      ,ht_canopy = 6
 #      ,strata = c(0,5,10,15,20,seq(25,100,25),200, 400)
 #      ,all_metrics = F
 #      ,fine_intensity = F
 #    )

  ){


list("--user-units" = "test")

  warning("If project cell size doesn't match gridmetrics cell size (or multiple), you are probably causing problems")

  #if(is.na(minht)) minht = heightbreak

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
  # if("sf" %in% class(proj_polys)){
  #   proj_polys_in = proj_polys
  # }

  #load project
  if(is.na(proj_gpkg_path ) ) stop("provide path to project: argument proj_gpkg_path ")
  proj_polys_in = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_polys )
  proj_cfg_in = try(sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_config ), silent=T)

  #load the cellsize
  cfg_cellsize = proj_cfg_in$pixel_size
  #if(!is.na(cellsize)) if(cellsize != cfg_cellsize) warning("cellsize provided to run_gridmetrics: ", cellsize, " does not match RSForInvt_config: ",cfg_cellsize )
  #if(is.na(cellsize)) cellsize = cfg_cellsize

  print("project loaded");print(Sys.time())
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

  #prepare lapis commands
    #las and tif files
    files_las_in = paste0("-L ",gsub(","," -L ",proj_polys_in$las_file))
    warning("bad hack on line 235: rename .dtm to .tif etc...")
    proj_polys_in$dtm_file = gsub("[.]dtm",".tif",gsub("_fusion","_tif",proj_polys_in$dtm_file))
    files_dtm_in = paste0("-D ",gsub(","," -D ",proj_polys_in$dtm_file))
    cmds_in1 = paste(paste(path_lapis,paste("-O",dir_out), files_las_in),files_dtm_in)

    #prep generic named lapis arguments
    isnull_args = sapply(lapis_args,is.null)
    isna_args = is.na(lapis_args)
    isnamed_args = sapply(names(lapis_args),nchar) > 0
    lapis_args_nm = lapis_args[!isna_args & !isnull_args & isnamed_args]
    cmds_args_nm = paste(apply(cbind(names(lapis_args_nm),lapis_args_nm),1,paste, collapse=" "), collapse=" ")

    #prep generic UNnamed lapis arguments
    lapis_args_un = lapis_args[!isnamed_args]
    cmds_args_un = paste(lapis_args_un, collapse= " ")

    #prepare bounding box
    #aoixmin, aoixmax, aoiymin, and aoiymax
    cmds_bbx = apply(
                  cbind(paste("aoixmin", proj_polys_in[,c("xmin"),drop=T])
                  , paste("aoixmax", proj_polys_in[,c("xmax"),drop=T])
                  , paste("aoiymin", proj_polys_in[,c("ymin"),drop=T])
                  , paste("aoiymax", proj_polys_in[,c("ymax"),drop=T])
                  )
              ,1, paste, collapse= " ") #end apply

    #final set of generic arguments
    cmds_args_in = paste(cmds_args_nm, cmds_args_un, sep=" ")

    #combine command with arguments - clean up white space
    cmds_in2 = gsub("[  ]"," ",gsub("[  ]"," ",paste(paste(cmds_in1, cmds_bbx,sep=" "),cmds_args_in,sep=" ")))

  browser()


    print("set up commands");print(Sys.time())


    #get identical behavior if debugging
    #the commands are now randomly selected to help with file interference
    rm(proj_polys_in);gc()


    if(do_run){
      if(n_core>1 ){
        print("begin parallel processing");print(Sys.time())

        fn_proc = function(x, dir_out){
          file_out = paste0(dir_out,"/lapis_messages_", Sys.getpid(), ".txt")
          y = try( system(x,intern=T))
          write(paste(y, collapse=" \n" ), file=file_out, append=T)
          gc()
        }
        set.seed(50)
        clus=parallel::makeCluster(n_core)#,setup_strategy = "sequential")

        res=parallel::parLapplyLB( clus , sample( cmds_in2,length( cmds_in2)) , fn_proc , dir_out = dir_proc_time );gc()

        gc();parallel::stopCluster(clus);gc()

      }else{

       #writeClipboard(coms[1])
       #testing if(F) lapply(coms[110:200],shell) ;gc()
       lapply( cmds_in2,function(x)try(shell(x))) ;gc()

      }




      print("run lapis (done)");print(Sys.time())
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


