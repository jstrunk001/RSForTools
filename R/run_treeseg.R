#'@title
#'  runs treeSeg.exe (USDA FUSION) across a bunch of .dtm caanopy surfaces
#'
#'@description
#'  runs treeSeg.exe  (USDA FUSION) across a bunch of .dtm caanopy surfaces
#'
#'@details
#'  runs treeSeg.exe  (USDA FUSION) across a bunch of .dtm caanopy surfaces
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/8/2020 FUnction created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param dir_dtms  (character - optional) must provide either dir_dtm - a directory with .dtm CSM files - or dtm_paths
#'@param dtm_paths  (character - optional) must provide either dtm_paths - a vector of .dtm CSM paths - or dir_dtm
#'@param dir_out  (character - required) where to send outputs
#'@param ht_threshold  (numeric or character - required) minimum height to be segmented
#'@param switches (character - optional) switches to treeseg.exe, see fusion documentation
#'@param treeseg_path  (character - required) where is the .exe located
#'@param ncore  (integer - optional) number of cores to use for processing, defaults to 4
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#' run_treeseg(
#'    dir_dtms = "D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\R\\DAP_Lidar_analysis\\RSForInvt\\canopymodel\\individual"
#'    ,dtm_paths = NA
#'    ,dir_out = "D:\\Box\\VMARS\\Projects\\DAP_evaluation_Meston\\R\\DAP_Lidar_analysis\\RSForInvt\\ITD"
#'    ,switches = "/shape"
#'    ,treeseg_path = "c:\\fusion\\treeseg.exe"
#'    ,ncore = 4
#'  )
#'
#'@import parallel
#'
#'@export
#
#'@seealso \code{\link{run_canopyModel}}\cr \code{\link{project_create}}\cr

#Desired upgrades to this function:
#
#

run_treeseg = function(
    dir_dtms = NA
    ,dtm_paths = NA
    ,dir_out = NA
    ,ht_threshold = 40
    ,switches = "/shape"
    ,treeseg_path = "c:\\fusion\\treeseg.exe"
    ,ncore = 4
  ){

  if(!is.na(dir_dtms)) dtm_paths_in = normalizePath(list.files(dir_dtms,pattern="[.]dtm$",full.names=T),mustWork = F)
  if(is.na(dir_dtms) & !is.na(dtm_paths)) dtm_paths_in = dtm_paths
  if(is.na(dir_dtms) & is.na(dtm_paths)) stop("must provide either dir_dtms or dtm_paths")
  if(is.na(dir_out) ) stop("must provide dir_out")
  if(!dir.exists(dir_out) ) dir.create(dir_out, recursive=T)

  csv_paths_out = normalizePath(file.path(dir_out, basename(dtm_paths_in)),mustWork = F)

  df_cmds = data.frame(treeseg_path , switches , shQuote(dtm_paths_in), ht_threshold , shQuote(csv_paths_out) )
  cmds = apply(df_cmds,1,paste,collapse=" ")

  if(ncore>1){

    clus_in = makeCluster(ncore)
    res = parSapply(clus_in,cmds,shell)
    stopCluster(clus_in)

  }else{

    res = sapply(cmds,shell)

  }

  res

}

