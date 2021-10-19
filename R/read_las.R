#'@title
#'  read an las file
#'
#'@description
#'  read an las file
#'
#'@details
#'  read an las file
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param path single path
#'@param paths vector of paths for 'read_header()'
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'    files=list.files("C:\\temp\\lidar_test\\",full.names=T)
#'    print(read_header("C:\\temp\\lidar_test\\183_302.las"))
#'    print(read_header(files))
#'
#'@import tools sp
#'
#'@export
#
#'@seealso \code{\link{read_dtm}}\cr \code{\link{gridmetrics}}\cr


read_las=function(path,...){

  UseMethod("read_las",path    )
}

read_las.character=function(path,n_read=NA){

  if(length(path)==0) stop("'path' is empty")
  if(length(path) > 1) stop("path must be of length 1, currently of length",length(path))

  head_in=read_header(path)

  list(
    pts=.read_points(path,head_in,n_read=n_read)
    ,header=head_in

    )

}

#
#
# read_header=function(paths,...){
#
#   UseMethod("read_header",paths    )
#
# }
#
#
# read_header.character=function(paths){
#
#   if(length(paths)==0) stop("'path' is empty")
#   if(length(paths)==1) try(return(data.frame(t(.read_one_header(paths)))))
#   if(length(paths) >1) try(return(.read_headers(paths)))
#
# }
#
# .read_headers=function(paths){
#
#   headers=data.frame(do.call(rbind,lapply(paths,function(x)t(.read_one_header(x)))),row.names=NULL)
#
# }
#
# .read_one_header=function(path){
#
#
#   con <- file(path, open = 'rb')
#   rBcon <-readBin(con, 'raw', n = 227, size = 1, signed = F)
#   try(close(con))
#
#   phb <- data.frame(
#     row.names = c(
#       'signature'
#       , 'Source_ID'
#       , 'Encoding'
#       , 'GUID1'
#       , 'GUID2'
#       , 'GUID3'
#       , 'GUID4'
#       , 'V_Maj'
#       , 'V_Min'
#       , 'Sys_ID'
#       , 'Software'
#       , 'Create_Day'
#       , 'Create_Year'
#       , 'Header_Size'
#       , 'Off2points'
#       , 'N_VLR'
#       , 'Format_ID'
#       , 'Rec_length'
#       , 'N_all'
#       , 'N_1st'
#       , 'N_2nd'
#       , 'N_3rd'
#       , 'N_4th'
#       , 'N_5th'
#       , 'Xscale'
#       , 'Yscale'
#       , 'Zscale'
#       , 'Xoffset'
#       , 'Yoffset'
#       , 'Zoffset'
#       , 'MaxX'
#       , 'MinX'
#       , 'MaxY'
#       , 'MinY'
#       , 'MaxZ'
#       , 'MinZ')
#
#     ,Value = c(
#       readBin(rBcon[1:4], 'char', size = 4, n = 1),
#       readBin(rBcon[5:6], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[7:8], 'int', size = 2, n = 1),
#       readBin(rBcon[9:12], 'int', size = 4, n = 1),
#       readBin(rBcon[13:14], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[15:16], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[17:24], 'char', size = 4, n = 1, signed = FALSE),
#       readBin(rBcon[25], 'char', size = 1, n = 1, signed = FALSE),
#       readBin(rBcon[26], 'char', size = 1, n = 1, signed = FALSE),
#       readBin(rBcon[27:58], 'char', size = 32, n = 1),
#       readBin(rBcon[59:90], 'char', size = 32, n = 1),
#       readBin(rBcon[91:92], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[93:94], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[95:96], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[97:100], 'int', size = 4, n = 1),
#       readBin(rBcon[101:104], 'int', size = 4, n = 1),
#       readBin(rBcon[105],"int", size = 1, signed = F),
#       readBin(rBcon[106:107], 'int', size = 2, n = 1, signed = FALSE),
#       readBin(rBcon[108:111], 'int', size = 4, n = 1),
#       readBin(rBcon[112:115], 'int', size = 4, n = 1),
#       readBin(rBcon[116:119], 'int', size = 4, n = 1),
#       readBin(rBcon[120:123], 'int', size = 4, n = 1),
#       readBin(rBcon[124:127], 'int', size = 4, n = 1),
#       readBin(rBcon[128:131], 'int', size = 4, n = 1),
#       readBin(rBcon[132:139], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[140:147], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[148:155], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[156:163], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[164:171], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[172:179], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[180:187], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[188:195], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[196:203], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[204:211], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[212:219], 'double', size = 8, n = 1, signed = TRUE),
#       readBin(rBcon[220:227], 'double', size = 8, n = 1, signed = TRUE)
#     ),
#     stringsAsFactors = FALSE
#   )
#
#
#
#   isLASF <- phb[1, 1] == 'LASF'
#
#   if (!isLASF){
#
#     try(close(con))
#     warning(path, ' is not a valid LAS file')
#     phb[,]=NA
#   }
#
#   if(isLASF){
#
#     phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\001', 1, phb['V_Min', 1])
#     phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\002', 2, phb['V_Min', 1])
#     phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\003', 3, phb['V_Min', 1])
#     phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\004', 4, phb['V_Min', 1])
#
#     phb['V_Maj', 1] <- ifelse(phb['V_Maj', 1] == '\001', 1, phb['V_Maj', 1])
#     phb['V_Maj', 1] <- ifelse(phb['V_Maj', 1] == '\002', 2, phb['V_Maj', 1])
#
#     phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\001', 1, phb['Format_ID', 1])
#     phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\002', 2, phb['Format_ID', 1])
#     phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\003', 3, phb['Format_ID', 1])
#     phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\004', 4, phb['Format_ID', 1])
#
#     #laz files mess with format field
#     phb['Format_ID', 1] <- ifelse(as.numeric(phb['Format_ID', 1]) > 127, as.numeric(phb['Format_ID', 1]) - 128, phb['Format_ID', 1])
#
#     if (!isLASF) { warning(path, ' is not a valid LAS/LAZ file') }
#
#     if(phb['V_Min',]=="") phb['V_Min',]=0
#
#     if(as.numeric(phb['V_Maj',])==1 & as.numeric(phb['V_Min',])>3){
#
#       con <- file(path, open = 'rb')
#       rBcon <- readBin(con, 'raw', n = 375, size = 1)
#       try(close(con))
#
#       phb_add<- data.frame(
#         row.names = c(
#           'st_wdpr'
#           , 'st_vlr'
#           , 'n_vlr'
#           , 'n_pr'
#           , 'n_1'
#           , 'n_2'
#           , 'n_3'
#           , 'n_4'
#           , 'n_5'
#           , 'n_6'
#           , 'n_7'
#           , 'n_8'
#           , 'n_9'
#           , 'n_10'
#           , 'n_11'
#           , 'n_12'
#           , 'n_13'
#           , 'n_14'
#           , 'n_15'
#         ),
#         Value = c(
#           readBin(rBcon[228:235], 'int', size = 8, n = 1)
#           ,readBin(rBcon[236:243], 'int', size = 8, n = 1)
#           ,readBin(rBcon[244:247], 'int', size = 4, n = 1)
#           ,readBin(rBcon[248:255], 'int', size = 8, n = 1)
#
#           ,readBin(rBcon[256:263], 'int', size = 8, n = 1)
#           ,readBin(rBcon[264:271], 'int', size = 8, n = 1)
#           ,readBin(rBcon[272:279], 'int', size = 8, n = 1)
#           ,readBin(rBcon[280:287], 'int', size = 8, n = 1)
#           ,readBin(rBcon[288:295], 'int', size = 8, n = 1)
#           ,readBin(rBcon[296:303], 'int', size = 8, n = 1)
#           ,readBin(rBcon[304:311], 'int', size = 8, n = 1)
#           ,readBin(rBcon[312:319], 'int', size = 8, n = 1)
#           ,readBin(rBcon[320:327], 'int', size = 8, n = 1)
#           ,readBin(rBcon[328:335], 'int', size = 8, n = 1)
#           ,readBin(rBcon[336:343], 'int', size = 8, n = 1)
#           ,readBin(rBcon[344:351], 'int', size = 8, n = 1)
#           ,readBin(rBcon[352:359], 'int', size = 8, n = 1)
#           ,readBin(rBcon[360:367], 'int', size = 8, n = 1)
#           ,readBin(rBcon[368:375], 'int', size = 8, n = 1)
#         )
#         ,stringsAsFactors = FALSE
#       )
#
#       phb <- rbind(phb , phb_add)
#       if(phb["N_all",]==0){
#         phb["N_all",]=phb["n_pr",]
#       }
#     }
#
#     isLASF <- phb[1, 1] == 'LASF'
#
#     phb[17, 1] <- ifelse(phb[17, 1] == '\001', 1, phb[17, 1])
#     phb[17, 1] <- ifelse(phb[17, 1] == '\002', 2, phb[17, 1])
#     phb[17, 1] <- ifelse(phb[17, 1] == '\003', 3, phb[17, 1])
#     phb[17, 1] <- ifelse(phb[17, 1] == '\004', 4, phb[17, 1])
#
#     phb[8, 1] <- ifelse(phb[8, 1] == '\001', 1, phb[8, 1])
#     phb[8, 1] <- ifelse(phb[8, 1] == '\002', 2, phb[8, 1])
#
#     phb[9, 1] <- ifelse(phb[9, 1] == '\001', 1, phb[9, 1])
#     phb[9, 1] <- ifelse(phb[9, 1] == '\002', 2, phb[9, 1])
#
#
#   }
#
#   return(phb)
#
# }


.read_points=function (
  path
  ,phb
  ,n_read = NA

) {
  #kludge
  #if(is.function(read_fn))read_las=read_fn
    require(tools)
    require(sp)
    require(data.table)

    #preparatory variables
      rlength <- as.numeric(phb["Rec_length",])
      nrecords=as.numeric(phb["N_all",])
      seek_to=as.numeric(phb["Off2points",])
      if(!is.na(n_read)) nrecords=min(n_read,nrecords)
      nBytes=rlength*nrecords

      #read in data
      con <- file(path, open = 'rb')
      seek(con, seek_to, rw = "read")
      dat_bin=readBin(con, "raw", n = nBytes, size = 1, endian = "little")
      try(close(con))

      if(length(dat_bin) !=nBytes ){

        n_pts=floor(length(dat_bin)/rlength)
        dat_bin=dat_bin[1:(n_pts*rlength)]
        warning("file '",path,"' is a bad tile: missing ",nrecords-n_pts," records")

        nrecords=n_pts

      }


      bytes.matrix <- matrix(dat_bin , ncol= rlength , nrow = nrecords , byrow = TRUE)

      Xscalefactor <- as.numeric(phb[25, 1])
      Yscalefactor <- as.numeric(phb[26, 1])
      Zscalefactor <- as.numeric(phb[27, 1])
      Xoffset <- as.numeric(phb[28, 1])
      Yoffset <- as.numeric(phb[29, 1])
      Zoffset <- as.numeric(phb[30, 1])

      X = Xoffset + Xscalefactor * readBin(t(bytes.matrix[, 1:4]), 'integer', size = 4, n = nrecords, signed = TRUE,endian="little")
      Y = Yoffset + Yscalefactor * readBin(t(bytes.matrix[, 5:8]), 'integer', size = 4, n = nrecords, signed = TRUE,endian="little")
      Z <- Zoffset + Zscalefactor * readBin(t(bytes.matrix[, 9:12]), 'integer', size =4, n = nrecords, signed = TRUE,endian="little")
      bad_pts = X<0 | Y<0 | is.na(Z)
      bad_pts[is.na(bad_pts)]=T

      if(sum(bad_pts)>0){
        bytes.matrix <- bytes.matrix[!bad_pts,]
        nrecords=sum(!bad_pts)
        X = Xoffset + Xscalefactor * readBin(t(bytes.matrix[, 1:4]), 'integer', size = 4, n = nrecords, signed = TRUE,endian="little")
        Y = Yoffset + Yscalefactor * readBin(t(bytes.matrix[, 5:8]), 'integer', size = 4, n = nrecords, signed = TRUE,endian="little")
        Z <- Zoffset + Zscalefactor * readBin(t(bytes.matrix[, 9:12]), 'integer', size =4, n = nrecords, signed = TRUE,endian="little")
      }

      sr <- readBin(t(bytes.matrix[, 17]), 'integer', size = 1, n = nrecords)
      #sn <- ifelse(sr > 90, sr - 90, 90 - sr)
      i <- readBin(t(bytes.matrix[, 13:14]), 'integer', size = 2, n = 1 * nrecords)
      cl <- readBin(t(bytes.matrix[, 16]), 'integer', size = 1, n = nrecords, signed = FALSE)

      return(data.frame(X, Y, Z, Intensity = i, Classification = cl))

}



