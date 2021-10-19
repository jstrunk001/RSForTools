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
#'@param temp temp folder to hold batch files for use with gridmetrics.exe
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
#' ,existing_coms="C:\\Temp\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt"
#' ,n_cache=400
#' )
#'

#'
#'@import parallel rgdal raster
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
  ,dir_out = "c:/temp/test_project/gridmetrics"
  ,n_core = 4
  ,gridmetrics_path = "c:\\fusion\\gridmetrics64.exe"
  ,heightbreak = 3
  ,minht = NA
  ,cellsize = 66
  ,first = T
  ,intensity = F
  ,outlier = c(-5,400)
  ,fusion_switches = "/nointensity /first /failnoz"
  ,xmn = NA , xmx= NA , ymn=NA , ymx=NA
  ,temp = "c:\\temp\\run_gridmetrics\\"

  ,new_dtm_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)
  ,new_las_path = list(from = NA, to = c(NA,NA), prop=c(.6,.4)) #in case drive paths are wrong (e.g. External drives...)

  ,skip_existing = T

  ,existing_coms = c(NA,"C:\\Temp\\run_gridmetrics\\2017Aug17_100740\\all_commands.txt")   #skip setting up new dtm and las files

  ,do_run = T

  ,debug = F

  ,... #additonal arguments to fns

  ){

  warning("If project cell size doesn't match gridmetrics cell size (or ultiple), you are probably causing problems")
  
  if(is.na(minht)) minht = heightbreak

  options(scipen = 999)

  requireNamespace("parallel")
  requireNamespace("raster")
  requireNamespace("rgdal")

  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create temp folder
  gm_out=backslash(paste(dir_out,"/gridmetrics_csv/",sep=""))
  if(!dir.exists(gm_out)) try(dir.create(gm_out,recursive=T))

  #create csv folder dump
  if(is.na(existing_coms[1])) temp = backslash(paste(temp,"/",proc_time,"/",sep=""))
  if(!is.na(existing_coms[1])) temp = paste(dirname(existing_coms[1]),"/",sep="")
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

  coms_out=file.path(temp,"all_commands.txt")

 #load project
  if(!is.na(proj_polys[1])){
    proj = proj_polys
  }
  if(is.na(proj_polys[1]) & !is.na(proj_gpkg_path ) ){
    proj_polys = rgdal::readOGR(dsn=proj_gpkg_path , layer = layer_proj_polys )
    #proj_polys = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_polys )
    #df_config = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_config )
  }
  if(is.na(proj_polys[1]) & is.na(proj_gpkg_path ) ) stop("must provide either proj_polys or proj_gpkg_path")

  print("load project");print(Sys.time())
  #fix drive paths in lasR_project

  #rename paths in case files have moved
  if(!is.na(new_dtm_path)[1]){
    new_dtm_path_in = normalizePath(new_dtm_path)
    names(new_dtm_path_in) = names(new_dtm_path)
    dtm_old = normalizePath(proj_polys@data[,"dtm_file"])
    dtm_new = gsub(new_dtm_path_in["from"], new_dtm_path_in["to"], dtm_old , fixed = T)
    proj_polys@data[,"dtm_file"] = dtm_new
  }

  if(!is.na(new_las_path$from[1])){

    new_las_path_in = new_las_path
    new_las_path_in$from = normalizePath(new_las_path$from )
    new_las_path_in$to = normalizePath(new_las_path$to)

    las_new = normalizePath(proj_polys@data[,"las_file"])
    
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
    proj_polys@data[,"las_file"] = las_new
  }

  #skip existing files
  if(skip_existing){

    files_done=list.files(gm_out,pattern="[.]csv")
    ids_done=gsub("_.*","",files_done)
    files_exist=as.character(proj_polys@data[,"tile_id"]) %in% ids_done
    proj_polys=subset(proj_polys,subset=!files_exist)

  }
  print("skip files");print(Sys.time())

  #prepare output directory
  proj_polys@data[,"outf"]=paste(gm_out,proj_polys@data[,"tile_id"],".csv",sep="")

  print(paste(nrow(proj_polys@data),"tiles to process"))

  #prepare batch commands
    proj_polys@data[,"dtm_txt"]=backslash(paste(temp,backslash(proj_polys@data[,"tile_id"]),"_dtm.txt",sep=""))
    proj_polys@data[,"las_txt"]=backslash(paste(temp,backslash(proj_polys@data[,"tile_id"]),"_las.txt",sep=""))
    proj_polys@data[,"switches"]=paste("/minht:",minht
                              ," /outlier:",paste(outlier,collapse=",")
                              ," /cellbuffer:2 /gridxy:"
                              ,apply(round(proj_polys@data[,c("mnx","mny","mxx","mxy")]),1,paste,collapse=",")
                              ,sep="")
    
    if(!is.null(fusion_switches)){ 
      gmpath = paste(paste0('"',gridmetrics_path[1],'"'),fusion_switches)
    }else{ 
      gmpath = paste0('"',gridmetrics_path[1],'"')
    }
    
    coms_df=data.frame(gm=gmpath
                         ,sw=proj_polys@data[,c("switches")]
                         ,ids=paste("/id:",proj_polys@data[,"tile_id"],sep="")
                         ,dtms=backslash(proj_polys@data[,c("dtm_txt")])
                         ,hb=heightbreak
                         ,cs=cellsize
                         ,outf=backslash(proj_polys@data[,"outf"])
                         ,las=backslash(proj_polys@data[,"las_txt"])
                         )

    coms=apply(coms_df,1,paste,collapse=" ")
    print("set up commands");print(Sys.time())


    rm(proj_polys);gc()
    
    if(is.na(existing_coms[1]) ){

      writeLines(coms,coms_out)

      for(i in 1:nrow(proj_polys@data)){
        writeLines(gsub(",","\n",backslash(proj_polys@data[i,"las_file"])),proj_polys@data[i,"las_txt"])
        writeLines(gsub(",","\n",backslash(proj_polys@data[i,"dtm_file"])),proj_polys@data[i,"dtm_txt"])
      }
      print("create and write list of dtms and las files");print(Sys.time())
    }

    #get identical behavior if debugging
    #the commands are now randomly selected to help with file interference


    set.seed(50)
    if(do_run){
      if(n_core>1 ){
        print("begin parallel processing");print(Sys.time())

        clus=makeCluster(n_core)
        #clusterEvalQ(clus,{library(RSForInvt);gc()})
        res=parLapplyLB(clus,coms,shell);gc()
        gc();stopCluster(clus);gc()

      }else{
        #writeClipboard(coms[1])
        if(F) lapply(coms[1:5],shell) ;gc()
       lapply(coms,shell) ;gc()
        
      }
      print("run fusion (done)");print(Sys.time())
    }

}



