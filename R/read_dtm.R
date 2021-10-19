#'@name read_dtm
#'@title read USDA FUSION format (Bob McGaughey) raster files and headers
#'
#'@description
#'
#' read a single USDA FUSION format raster into an r raster object (raster package).
#' read a single header
#' read headers for all files in directory
#'
#'@details
#'
#' the DEM format is an esoteric raster format used only by (and required by) FUSION gridmetrics. SInce it is
#' required and we extensivey use FUSION (esp gridmetrics) we need the ability to interact with this file format.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'0.1 \tab 2014 July 09 - Operational \cr
#'}
#'@author
#'Jacob Strunk
#'
#'@param dtm_file path to specific FUSION dtm file
#'@param dir_dtm path to directory containing FUSION dtm files
#'@param pattern extension for FUSION dtm files
#'@param plot with 'dtm_file' optionally plot dtm with rgl and rasterVis
#'@param NA_val recoded fusion NA values to R NA - unfotunately FUSION uses -1.0 as NA values ..
#'@param ncore how many cores to use in processing dtm files - used with 'dir_dtm'
#'
#'@return
#'raster, rasters, or dataframe with header(s) information
#'
#'@examples
#'
#' many_dtms=read_dtm_header(dir_dtm="c:\\temp\\dtm_files" ,out_file="c:\\temp\\headers.csv")
#'
#' one_dtm=read_dtm_header("//dnrgpoly112/e$/rs_prototype/dtm/dtm_files/38_58.dtm")
#'
#'@import plyr raster
#'
#'
#'@seealso \code{\link{read_las}}\cr
#'
#'
#'@export
read_dtm=function(
  dtm_files=NA
  ,dir_dtm=NA
  ,pattern="[.]dtm$"
  ,plot=F
  ,NA_val=-1
  ,return_polygon=F
  ,mosaic=T
){

  if(!is.na(dir_dtm) | length(dtm_files) > 1){


    if(is.na(dtm_files[1])) dtm_files=list.files(dir_dtm,full.names=TRUE,ignore.case=TRUE,pattern=pattern,recursive=recursive)
    dtms=lapply(dtm_files,read_dtm,plot=F,NA_val=NA_val,return_polygon=return_polygon)
    if(mosaic) dtms=do.call(function(...)mosaic(...,fun=mean,na.rm=T), dtms)
    return(dtms)



  }else{

    require(raster)

    if(missing(dtm_files)) dtm_files=file.choose()
    if (!file.exists(dtm_files)) {
      stop(dtm_files, ' cannot be found. Please include a valid path to a dtm ')
    }

    con <- file(dtm_files, open = 'rb')
    bin_1 <- readBin(con, 'raw', n = 200)

    #get header
    header_in <- read_dtm_header (dtm_files)

    if(header_in[1,"format_version"] > 1.99){

      header_in[1,"coord_sys"]=readBin(bin_1[157:158], 'int', size = 2, n = 1)

    }
    if(header_in[1,"format_version"] > 3.099){

      header_in[1,"coord_zone"]=readBin(bin_1[159:160], 'int', size = 2, n = 1)
      header_in[1,"h_datum"]=readBin(bin_1[161:162], 'int', size = 2, n = 1)
      header_in[1,"v_datum"]=readBin(bin_1[163:164], 'int', size = 2, n = 1)

    }
    #header_in2=data.frame(t(header_in))

    #read in data


    r_grid <- raster(
      apply(
        matrix(
          readBin(con
                  , c("integer","integer","numeric","numeric")[as.numeric(header_in[1,"z_format"])+1]
                  , n = as.numeric(header_in[1,"n_cols"])*as.numeric(header_in[1,"n_rows"])
                  , size = c(2,4,4,8)[as.numeric(header_in[1,"z_format"])+1]
                  ,endian="little")
          ,ncol=as.numeric(header_in[1,"n_cols"])
          ,byrow = F
        )
        ,2,rev)
      ,xmn=as.numeric(header_in[1,"ll_x"])
      ,xmx=as.numeric(header_in[1,"ll_x"])+as.numeric(header_in[1,"n_cols"])*as.numeric(header_in[1,"col_spacing"])
      ,ymn=as.numeric(header_in[1,"ll_y"])
      ,ymx=as.numeric(header_in[1,"ll_y"])+as.numeric(header_in[1,"n_rows"])*as.numeric(header_in[1,"row_spacing"])
    )
    close(con)

    if(header_in[1,"z_format"] == 0 ) dataType(r_grid) = "INT2U"
    if(header_in[1,"z_format"] == 1 ) dataType(r_grid) = "INT4S"
    if(header_in[1,"z_format"] == 2 ) dataType(r_grid) = "FLT4S"
    if(header_in[1,"z_format"] == 3 ) dataType(r_grid) = "FLT8S"

    if(header_in[1,"z_format"] < 2 ) r_grid = as.integer(r_grid)

    #assign NA values
    r_grid[r_grid==NA_val]=NA

    return(r_grid)

  }


}

#'@export
#'@rdname read_dtm
read_dtm_header=function(
  dtm_file
  ,dir_dtm=NA
  ,pattern="[.]dtm$"
  ,recursive=TRUE
  ,out_file=NA
  #,header_only=TRUE   - eventually read z value
){
  require(plyr)
  if(!is.na(dir_dtm)){

    dtm_files=list.files(dir_dtm,full.names=TRUE,ignore.case=TRUE,pattern=pattern,recursive=recursive)
    header_in=data.frame(name=basename(dtm_files),do.call(rbind,lapply(dtm_files,function(x)t(read_dtm_header(x)))),full_path=dtm_files,row.names=NULL)

  }else if(length(dtm_file)>1){

    header_in=data.frame(name=basename(dtm_file),rbind.fill(lapply(dtm_file,read_dtm_header)),full_path=dtm_file,row.names=NULL)

  }else{

    if(missing(dtm_file)) dtm_file=file.choose()
    if (!file.exists(dtm_file[1])) {
      stop(dtm_file, ' cannot be found. Please include a valid path to a dtm ')
    }

    con <- file(dtm_file, open = 'rb')
    rBcon1 <- readBin(con, 'raw', n = 159, size = 1)

    header_in <- data.frame(
        "plans_signature"=readBin(rBcon1[1:21], 'char', size = 20, n = 1)
        ,"DTM_name"=readBin(rBcon1[22:82], 'char', size = 61, n = 1)
        ,"format_version"=readBin(rBcon1[83:86], 'double', size = 4, n = 1)
        ,"ll_x"=readBin(rBcon1[87:94], 'numeric', size = 8, n = 1)
        ,"ll_y"=readBin(rBcon1[95:102], 'numeric', size = 8, n = 1)
        ,"min_z"=readBin(rBcon1[103:110],'numeric', size = 8, n = 1)
        ,"max_z"=readBin(rBcon1[111:118],'numeric', size = 8, n = 1)
        ,"rotation" =readBin(rBcon1[119:126], 'numeric', size = 8, n = 1)
        ,"col_spacing"=readBin(rBcon1[127:134], 'numeric', size = 8, n = 1)
        ,"row_spacing"=readBin(rBcon1[135:142], 'numeric', size = 8, n = 1)
        ,"n_cols"=readBin(rBcon1[143:146], 'int', size = 4, n = 1)
        ,"n_rows"=readBin(rBcon1[147:150],  'int', size = 4, n = 1)
        ,"units_xy"=readBin(rBcon1[151:152], 'int', size = 2, n = 1)
        ,"units_z"=readBin(rBcon1[153:154], 'int', size = 2, n = 1)
        ,"z_format"=readBin(rBcon1[155:156], 'int', size = 2, n = 1)
        )
        # ,"Formats 2.0+ - adds coordinate system (0 2-utm,3-state plane,4 unknown)"
        # ,"Formats 3.1+ - adds horizontal datum"
        # ,"Formats 3.1+ - adds vertical datum"

    close(con)
  }

  if(!is.na(out_file)) write.csv(header_in,out_file)
  return(header_in)

}

