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
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param lasR_project_csv csv file of intersections created by lasR_project() function
#'@param lasR_project__gpkg polygon file of intersections created by lasR_project() function
#'@param dir_out where should csv files be sent
#'@param n_core number of corest to run process on
#'@param fusion_path where is gridmetrics.exe (FUSION)
#'@param heightbreak Height break for cover calculation
#'@param cellsize output raster resolution
#'@param minht set minht for gridmetrics.ex
#'@param first T/F use only first returns
#'@param intensity T/F include intensity metrics
#'@param outlier c(-5,500) range of inclusion
#'@param fusion_switches other fusion switches as a string e.g. "/noground"
#'@param xmn,xmx,ymn,ymx set extent for analysis
#'@param fun a custom function if gridmetrics is not used
#'@param temp temp folder to hold batch files for use with gridmetrics.exe
#'@param fast_cache experimental - a high-speed temporary folder to copy las files to
#'@param n_cache experimental - number of las files to copy to cache
#'@param dir_dtm in case path to dtms has changed from lasR_project
#'@param dir_las in case path to las has changed from lasR_project
#'@param skip_existing skip tiles if they already have output csvs
#'@param con a parallel connection, optional, function can also create parallel threads
#'@param table output folder name
#'@param existing_coms path to existing batch comamnds, incase processing was interrupted the first time
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'  gmi=run_gridmetrics(
#' lasR_project_poly="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp"
#' ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_07\\"
#' ,dir_dtm="c:\\usgs_dtms\\dtms\\"
#' ,dir_las="D:\\naip_2015_laz\\"
#' ,n_core=10
#' ,existing_coms="C:\\Temp\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt"
#' ,fast_cache=c(rep("r:\\temp",10),rep("c:\\temp",3),rep("i:\\temp",3),rep(NA,3))
#' ,n_cache=400
#' )
#'

#'
#@import some_package,some_package2
#'
#'@export
#
#'@seealso \code{\link{lasR_project}}\cr \code{\link{gridmetrics}}\cr

run_canopyModel=function(
  proj_polys = NA
  ,proj_gpkg_path = NA
  ,layer_proj_polys = "RSForInvt_prj"
  ,layer_proj_config = "RSForInvt_config"
  ,dir_out = "c:/temp/test_project/canopymodel"
  ,n_core = 4
  ,fusion_path = "c:\\fusion\\canopymodel.exe"
  ,cellsize = 5
  ,xyunits = c("F","M")
  ,zunits = c("F","M")
  ,cdsys = c(0,1,2) #see fusion
  ,zone = c(0,1,2)
  ,hzd = c(0,1,2)
  ,vtd = c(0,1,2,3)

  ,outlier = c(-5,400)
  ,fusion_switches = NA
  ,xmn = 561066,xmx=2805066,ymn=33066,ymx=1551066
  ,fun = compute_metrics2#list(min=min,max=max,mean=mean,sd=sd)#,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))
  ,temp = "c:\\temp\\run_canopymodel\\"

  ,dir_dtm = NA #in case drive paths are wrong (External drives...)
  ,dir_las = NA #in case drive paths are wrong (External drives...)

  ,new_dtm_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)
  ,new_las_path = c(from = NA, to = NA) #in case drive paths are wrong (e.g. External drives...)


  ,skip_existing = T

  ,existing_coms = c(NA,"C:\\Temp\\run_gridmetrics\\2017Aug17_100740\\all_commands_canopyModel.txt")   #skip setting up new dtm and las files

  ,debug = F

  ,... #additonal arguments to fns

  ){

  options(scipen = 999)

  require("parallel")
  require("raster")
  require("rgdal")


  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create temp folder
  fusion_out=backslash(paste(dir_out,"/canopymodel/",sep=""))
  if(!dir.exists(fusion_out)) try(dir.create(fusion_out,recursive=T))

  #create csv folder dump
  if(is.na(existing_coms[1])) temp = backslash(paste(temp,"/",proc_time,"/",sep=""))
  if(!is.na(existing_coms[1])) temp = paste(dirname(existing_coms[1]),"/",sep="")
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

  coms_out=file.path(temp,"all_commands_canopyModel.txt")

  #load project
  if(!is.na(proj_polys[1])){
    proj = proj_polys
  }
  if(is.na(proj_polys[1]) & !is.na(proj_gpkg_path ) ){
    proj_polys = rgdal::readOGR(dsn=proj_gpkg_path , layer = layer_proj_polys )
    #df_config = sf::st_read(dsn=proj_gpkg_path , layer = layer_proj_config )
  }
  if(is.na(proj_polys[1]) & is.na(proj_gpkg_path ) ) stop("must provide either proj_polys or proj_gpkg_path")
  print("load project");print(Sys.time())

  #rename paths in case files have moved
  if(!is.na(new_dtm_path)[1]){
    new_dtm_path_in = normalizePath(new_dtm_path)
    names(new_dtm_path_in) = names(new_dtm_path)
    dtm_old = normalizePath(proj_polys@data[,"dtm_file"])
    dtm_new = gsub(new_dtm_path_in["from"], new_dtm_path_in["to"], dtm_old , fixed = T)
    proj_polys@data[,"dtm_file"] = dtm_new
  }
  if(!is.na(new_las_path)[1]){
    new_las_path_in = normalizePath(new_las_path)
    names(new_las_path_in) = names(new_las_path)
    las_old = normalizePath(proj_polys@data[,"las_file"])
    las_new = gsub(new_las_path_in["from"], new_las_path_in["to"], las_old , fixed = T)
    proj_polys@data[,"las_file"] = las_new
  }

  #skip existing files
  if(skip_existing){

    files_done=list.files(fusion_out,pattern="[.]dtm")
    ids_done=gsub("_.*","",files_done)
    files_exist=as.character(proj_polys@data[,"tile_id"]) %in% ids_done
    proj_polys=subset(proj_polys,subset=!files_exist)

  }
  print("skip files");print(Sys.time())

  #prepare output directory
  proj_polys@data[,"outf"]=paste(fusion_out,proj_polys@data[,"tile_id"],".dtm",sep="")

  print(paste(nrow(proj_polys@data),"tiles to process"))

  #prepare batch commands
    proj_polys@data[,"dtm_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_dtm.txt",sep=""))
    proj_polys@data[,"las_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_las.txt",sep=""))
    proj_polys@data[,"switches"]=paste(
                              #"/minht:",minht
                              "/outlier:",paste(outlier,collapse=",")
                              #," /cellbuffer:2"
                              ," /gridxy:",apply(proj_polys@data[,c("mnx","mny","mxx","mxy")],1,paste,collapse=",")
                              ,sep="")


    if(!is.na(fusion_switches))
      coms_df=data.frame(gm=paste(fusion_path[1],fusion_switches)
                         ,sw=proj_polys@data[,c("switches")]
                         ,dtms=paste("/ground:",forwardslash(proj_polys@data[,c("dtm_txt")]),sep="")
                         ,outf=proj_polys@data[,"outf"]
                         ,cs=cellsize
                         ,cs = cellsize
                         ,xyunits = xyunits[1]
                         ,zunits = zunits[1]
                         ,cdsys = cdsys[1]
                         ,zone = zone[1]
                         ,hzd = hzd[1]
                         ,vtd = vtd[1]
                         ,las=proj_polys@data[,"las_txt"]
                         )

    if(is.na(fusion_switches))
      coms_df=data.frame(fusion_path[1]
                         ,sw = proj_polys@data[,c("switches")]
                         ,dtms=paste("/ground:",forwardslash(proj_polys@data[,c("dtm_txt")]),sep="")
                         ,outf = proj_polys@data[,"outf"]
                         ,cs = cellsize
                         ,xyunits = xyunits[1]
                         ,zunits = zunits[1]
                         ,cdsys = cdsys[1]
                         ,zone = zone[1]
                         ,hzd = hzd[1]
                         ,vtd = vtd[1]
                         ,las = proj_polys@data[,"las_txt"]
                         )

    coms=apply(coms_df,1,paste,collapse=" ")
    print("set up commands");print(Sys.time())

    if(is.na(existing_coms[1]) ){

      writeLines(coms,coms_out)

      for(i in 1:nrow(proj_polys@data)){
        writeLines(gsub(",","\n",proj_polys@data[i,"las_file"]),proj_polys@data[i,"las_txt"])
        writeLines(gsub(",","\n",proj_polys@data[i,"dtm_file"]),proj_polys@data[i,"dtm_txt"])
      }
      print("create list of dtms and las files");print(Sys.time())
    }

    if(n_core>1 ){

      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR);gc()})
      res=parLapply(clus,coms,shell);gc()
      gc();stopCluster(clus);gc()


    }else{

     lapply(coms,shell) ;gc()

    }
    print("run fusion");print(Sys.time())


}

.fn_copy_shell=function(x){
  if(class(x)=="data.frame"){
    diffs = x[,1] != x[,2]
    file.copy(x[diffs,1],x[diffs,2],overwrite = F)
  }
  else return(shell(x))
}



