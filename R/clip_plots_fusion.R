#'@title
#'  clip plots or polygons form lidar
#'
#'@description
#'  clip plots or polygons form lidar - requires that one first make a project using lasR_project which
#'  includes both lidar and dtms
#'
#'@details
#'  There are three ways to provide the input shapes to this function - either as a
#'  list of plot ids, coordinates and plot diameters (4 column data.frame), as a list of vertices
#'  organized by plot_id (3 column data.frame), or as a polygon shapefile (path to shapefile) inwich case
#'  the plot id field must be specified "id_field_plots".
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
#'1.0 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#' cd = read.csv("D:\\data\\wadnr_hood_canal\\field_data\\hood_canal_locations.csv")
#' cd1 = data.frame(cd[!is.na(cd[,"x"]),c("plot_id","x","y")], width = 37.2*2 )
#'
#'
#' res=clip_plots_fusion(
#'   idxyd=cd1
#'   ,dir_las = "D:\\data\\wadnr_hood_canal\\las\\hood_canal_6in_DSM_2015\\"
#'   ,dir_dtm = "D:\\data\\lidar_wa_dtms\\"
#'   ,dir_clipdata="c:\\fusion\\clipdata.exe"
#'   ,dir_out = "D:\\data\\wadnr_hood_canal\\plot_clips\\6in"
#'   ,out_f = c(".las",".laz")[1]
#'   ,clipdata_switches="/height /shape:1"
#'   ,n_core=6
#'   ,temp = "c:\\temp\\clipdata"
#'   ,run=T
#' )
#'
#'
#'@import parallel
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
# Add polgyon clip functionality
#   1) Clip with clipdata for plot extents and get heights
#   2) clip with polyclipdata and get polygons
#

clip_plots_fusion=function(
  idxyd=NA #id,x,y,diameter
  #,polys=NA #shapefile path, or sp object for plots
  #,polys_idfield="plot"
  ,dir_las = NA
  ,dir_dtm = NA
  ,dir_clipdata="c:\\fusion\\clipdata.exe"
  #,dir_polyclipdata="c:\\fusion\\polyclipdata.exe"
  ,dir_out = NA
  ,out_f = c(".las",".laz")
  ,clipdata_switches=c("/height /shape:1","")[1]
  ,n_core=6
  ,temp = "c:\\temp\\clipdata"
  ,run=T

){

  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")
  if(interactive()){require(parallel)}
  if(is.na(dir_las)) stop("dir_las not provided")
  if(is.na(dir_out)){
    warning("dir_out not provided, using temp:",temp)
    dir_out=temp
  }
  if(is.na(dir_dtm)){
    warning("dir_dtm not provided, points will not be elevation adjusted")
  }
  if(!file.exists(dir_out)) try(dir.create(dir_out, recursive=T),silent=T)
  temp = backslash(paste(temp,"/",proc_time,"/",sep=""))
  dir.create(temp,recursive=T)

  #prepare coordinates
  cds_df= data.frame(
                    xmin = idxyd[,2] - idxyd[,4]/2
                     ,ymin = idxyd[,3] - idxyd[,4]/2
                     ,xmax = idxyd[,2] + idxyd[,4]/2
                     ,ymax = idxyd[,3] + idxyd[,4]/2
                     )
  #output files
  lasz_out = file.path(dir_out,paste(idxyd[,1],out_f[1],sep=""))

  #prepare dtm list
  if(!is.na(dir_dtm)){
    dtm_list=file.path(temp,"dtm_list.txt")
    dtm_files = list.files(dir_dtm, full.names=T,pattern="[.]dtm$")
    writeLines(dtm_files,dtm_list)
    dtm_switch = paste("/dtm:",dtm_list,sep="",collapse="")
  }
  #prepare las list
  lasz_list=file.path(temp,"lasz_list.txt")
  lasz_files = unlist(c(list.files(dir_las, full.names=T,pattern="[.]las$")
                        ,list.files(dir_las, full.names=T,pattern="[.]laz$")))
  writeLines(lasz_files,lasz_list)

  #prepare commands
  cmd_df = data.frame(dir_cd=dir_clipdata)
  if(!is.na(clipdata_switches[1]))if(nchar(clipdata_switches[1]) > 0) cmd_df = data.frame(cmd_df,sw=clipdata_switches[1])
  if(!is.na(dir_dtm)) cmd_df = data.frame(cmd_df,dtm_sw=dtm_switch)
  cmds_df = data.frame(cmd_df,lasz_list,lasz_out,cds_df)
  cmds = apply(cmds_df,1,paste,collapse=" ")

  #write commands to batch file
  cmds_out = file.path(temp,"fusion_commands.bat")
  writeLines(cmds, cmds_out)

  #run commands
  if(run){
    if(interactive()){require(parallel)}
    clus=makeCluster(n_core)
    res=parLapply(clus,cmds,shell);gc()
    gc();stopCluster(clus);gc()
    return(list(res=res,cmds=cmds))

  }else{

    return(list(res=NA,cmds=cmds))

  }

}


if(F){
  if(!"cd" %in% ls()){
    library(lasR)
    cd = read.csv("D:\\data\\wadnr_hood_canal\\field_data\\hood_canal_locations.csv")
    cd1 = data.frame(cd[!is.na(cd[,"x"]),c("plot_id","x","y")], width = 37.2*2 )
  }

  res=clip_plots_fusion(
                 idxyd=cd1
                 ,dir_las = "D:\\data\\wadnr_hood_canal\\las\\hood_canal_6in_DSM_2015\\"
                 ,dir_dtm = "D:\\data\\lidar_wa_dtms\\"
                 ,dir_clipdata="c:\\fusion\\clipdata.exe"
                 ,dir_out = "D:\\data\\wadnr_hood_canal\\plot_clips\\6in"
                 ,out_f = c(".las",".laz")[1]
                 ,clipdata_switches="/height /shape:1"
                 ,n_core=6
                 ,temp = "c:\\temp\\clipdata"
                 ,run=T
  )
}
