#'@name write_dtm
#'@title write USDA FUSION format raster
#'
#'@description
#'write USDA FUSION format raster

#'
#'@details
#'
#' the DEM format is a niche raster format used only by (and required by) FUSION gridmetrics. SInce it is
#' required and we extensively use FUSION (esp gridmetrics) we need the ability to interact with this file format.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'0.1 \tab 2017 Dec 12 - Operational \cr
#'}
#'@author
#'Jacob Strunk <jstrunk@@fs.fed.us>
#'
#'@param dtm raster file
#'@param dtm_out path to specific FUSION dtm file
#'@param NA_val recode fusion NA values to R NA values - unfotunately FUSION uses -1.0 as NA values ..
#'
#'@return
#'Null, data is written to file
#'
#'
#'@examples
#'  dtm1=read_dtm("C:\\Temp\\36_63.dtm")
#'  write_dtm(dtm=dtm1,dir_out="c:\\temp\\36_63_b.dtm")
#'
#'@import raster Thermimage
#
#'@seealso \code{\link{read_las}}\cr

#'@export
write_dtm=function(
  dtm
  ,dir_out=NULL
  ,NA_val=as.integer(-1)
  ,zfmt=NA
){

    #require(raster)
    #require(Thermimage)

    if(!class(dtm)=="RasterLayer") dtm = raster::raster(dtm)

    #crosswalk raster and fusion types
    type_cw=c(
      LOG1S = 0
      ,INT1S = 0
      ,INT1U = 0
      ,INT2S = 0
      ,INT2U = 0
      ,INT4S = 1
      ,INT4U = 1
      ,FLT4S = 2
      ,FLT8S = 3
    )
    if(is.na(zfmt)) zfmt = type_cw[dtm@file@datanotation]

    #prepare header data

    header=vector(mode="raw",length=200)

    header[1:21 ]=writeBin("PLANS-PC BINARY .DTM", raw(),size=20)
    header[22:(22+nchar("RSForInvt R Package") )]=writeBin("RSForInvt R Package", raw())
    header[83:86 ]=writeBin(3.1, raw(),size=4)
    header[87:94 ]=writeBin(as.double(dtm@extent[1]), raw(), size = 8)
    header[95:102 ] = writeBin(dtm@extent[3], raw(), size = 8)
    header[103:110 ] = writeBin(as.double(minValue(dtm)),raw(), size = 8)
    header[111:118 ] = writeBin(as.double(maxValue(dtm)),raw(), size = 8)
    header[119:126 ] = writeBin(0, raw(), size = 8)
    header[127:134 ] = writeBin(res(dtm)[1], raw(), size = 8)
    header[135:142 ] = writeBin(res(dtm)[2], raw(), size = 8)

    header[143:146 ] = writeBin(dtm@ncols, raw(), size = 4)
    header[147:150 ] = writeBin(dtm@nrows,  raw(), size = 4)

    header[151:152 ] = writeBin(as.integer(0), raw(), size = 2)
    header[153:154 ] = writeBin(as.integer(0), raw(), size = 2)
    header[155:156 ] = writeBin(as.integer(zfmt), raw(), size = 2)
    header[159:160 ] = writeBin(as.integer(0), raw(), size = 2)
    header[161:162 ] = writeBin(as.integer(0), raw(), size = 2)
    header[163:164 ] = writeBin(as.integer(0), raw(), size = 2)

    #write header data
    con <- file(dir_out, open = 'wb')
    writeBin(header,con)

    #write data
      #get data size as a function of input formats - fusion only has a limted number of formats
      dtm_size=c(2,4,4,8)[zfmt+1]
      #flip data vertically
      vec_dtm=as.vector(Thermimage::flip.matrix(raster::as.matrix(dtm)))
      #assign NA
      vec_dtm[is.na(vec_dtm)] = NA_val
      #cast data to integer if necessary
      if(zfmt < 2) vec_dtm = as.integer(vec_dtm)
      else vec_dtm = as.double(vec_dtm)
      writeBin(vec_dtm,con, size = dtm_size)

    close(con)


  }


