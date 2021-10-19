plot_metrics=function(
  dir_las=NA
  ,las_files=NA
  ,pattern="[.]las*z*$"
  ,dir_out=NA
  ,return=T
  ,do_fusion=F
  ,dir_fusion="c:\\fusion\\cloudmetrics.exe"
  ,fun=compute_metrics1
  ,n_core=7
  ,...
){

  requireNamespace("lidR")
  requireNamespace("data.table")
  if(!is.na(dir_las)) las_files=list.files(dir_las,pattern="[.]las*z*",full.names=T)
  if(n_core<2){
    res=rbindlist(mapply(.proc_plot,las_files,SIMPLIFY=F,MoreArgs=list(fun=fun,...)))
  }
  if(n_core>1){
    requireNamespace("parallel")
    clus=makeCluster(n_core)
    res=rbindlist(clusterMap(clus,.proc_plot,las_files,SIMPLIFY=F,MoreArgs=list(fun=fun,...)))
    stopCluster(clus)
  }

  if(!is.na(dir_out)){
    if(!dir.exists(dir_out)) dir.create(dir_out)
    n_out=length(list.files(dir_out,"plot_metrics...[.]csv$"))
    outf=file.path(dir_out,sprintf("plot_metrics%03d.csv",n_out+1))
    write.csv(res,outf)
  }
  if(return) return(res)
}

  compute_metrics1 = function(
    lidar
    ,ht_brk=c(6,3,seq(10,100,20))
    ,outliers=c(-6,400)
    ,elev_metrics=F         #adjust for the fact that heights aren't provided - offset by 5th percentile height
    ,vol_res=seq(5,100,20)
    ){

    #make data convenient
    z=lidar@data$Z
    x=lidar@data$X
    y=lidar@data$Y

    #adjust for missing dtm
    if(elev_metrics){
      z = z - quantile(z,.05)
    }

    #filter
    if(!is.na(outliers[1])) id_in=z>outliers[1] & z<outliers[2]
    else id_in=T

    z_in=z[id_in]
    x_in=x[id_in]
    y_in=y[id_in]

    id_brk=z_in>ht_brk[1]
    z_brk=z_in[id_brk]
    x_brk=x_in[id_brk]
    y_brk=y_in[id_brk]

    metrics_in = data.frame(
      n_all=length(z)
      ,n_in=length(z_in)
      ,n_lt_brk=length(z_in[!id_brk])
      ,n_gt_brk=length(z_brk)

      ,zmin   = min(z_brk,na.rm=T)
      ,zmax   = max(z_brk,na.rm=T)
      ,zmean   = mean(z_brk,na.rm=T)
      ,zsqmean = sqrt(mean(z^2,na.rm=T))  # Quadratic mean
      ,zsd   = sd(z_brk,na.rm=T)
      #,zcover = length(z_brk) / length(z_in)

      ,xsd = sd(x_brk,na.rm=T)
      ,ysd = sd(y_brk,na.rm=T)
      ,xysd= sd(sqrt(y_brk*x_brk),na.rm=T)
      ,xyzsd= sd((y_brk*x_brk*z_brk)^(1/3),na.rm=T)

    )

    #add cover
    nms_cov=sprintf("zcover_%03d",ht_brk)
    cov_pcts=1-(ecdf(z))(ht_brk)
    metrics_in[,nms_cov]=cov_pcts

    #add quantiles
    step=.1
    qts=c(step/2,seq(0+step,1-step,step),1-step/2)
    nms_zqts=sprintf("zqt%02d",qts*100)
    metrics_in[,nms_zqts]=quantile(z_brk,qts,na.rm=T)

    #add volume
    nms_vol=sprintf("vol%03d",vol_res)
    fn_vol=function(vol_res,x,y,z) length(unique(paste(round(x/vol_res,0),round(y/vol_res,0),round(z/vol_res,0),sep="_")))*(vol_res^3)
    metrics_in[,nms_vol]=sapply(vol_res,fn_vol,x_brk,y_brk,z_brk)

    #add area - use same resolution as volume
    nms_area=sprintf("area%03d",vol_res)
    fn_area=function(area_res,x,y) length(unique(paste(round(x/area_res,0),round(y/area_res,0),sep="_")))*(area_res^2)
    metrics_in[,nms_area]=sapply(vol_res,fn_area,x_brk,y_brk)

    #add density metrics
    nms_area=sprintf("area%03d",vol_res)
    fn_area=function(area_res,x,y) length(unique(paste(round(x/area_res,0),round(y/area_res,0),sep="_")))*(area_res^2)
    metrics_in[,nms_area]=sapply(vol_res,fn_area,x_brk,y_brk)

    return(metrics_in)

  }


  .proc_plot = function(LASFile,fun,...){
    requireNamespace("lidR")

    # Load the data
    lidar = readLAS(LASFile)

    # compute metrics
    metrics_i = fun(lidar,...)

    return(data.frame(LASFile,metrics_i))

  }




if(F){

  library(lasR)
  dir1="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_clips\\"
  metrics0=plot_metrics(dir1,n_core=1,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_metrics\\",fun=compute_metrics1,elev_metrics=F)
  dir1="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_clips_elev\\"
  metrics1=plot_metrics(dir1,n_core=1,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_metrics_elev\\",elev_metrics=T)
  #plot_metrics=plot_metrics(dir1,n_core=7,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_metrics_elev\\",elev_metrics=F,outliers=NA)
}

