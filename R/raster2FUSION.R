#'@title
#'  <Delete and Replace>
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
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param dir_in where are the input rasters
#'@param  dir_out where to put output .dtm fusion rasters
#'@param  pattern  pattern for seeking rasters in dir_in
#'@param  recursive  should function recurse into subfolders
#'@param  ncore  use more than one core to process ?
#'@param  ...  other arguments to list.files
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import parallel raster
#'
#'@export
#
#'@seealso \code{\link{list.files}}\cr \code{\link{raster}}\cr

#Desired upgrades to this function:
#
#


#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))

raster2FUSION = function( dir_in
                          , dir_out
                          , pattern = "[.]img"
                          , recursive = T
                          , ncore=4
                          , ...
                          ){


  dtm_paths = list.files(dir_in, pattern = pattern, recursive = recursive , full.names=T,  ...)
  out_paths = file.path(dir_out,gsub(pattern,".dtm",basename(dtm_paths)))

  proc1 = function(in_i, out_i, write_dtm){
    require(raster)
    ri = raster(in_i)
    RSForInvt::write_dtm(ri, out_i)
  }
  if( ncore > 1 ){

    require(parallel)
    clus1 = makeCluster(ncore)
    res = clusterMap( clus1 , proc1 , dtm_paths , out_paths )

  }else{

    res = mapply( proc1 , dtm_paths , out_paths )

  }

  return(res)

}
