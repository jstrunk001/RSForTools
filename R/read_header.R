#'@title
#'  read an las/laz file header
#'
#'@description
#'  read an las/laz file header
#'
#'@details
#'  read an las/laz file header
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2020 May 07 Split from read_las \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jacob.strunk@@usda.gov>
#'
#'@param paths vector of paths to las files
#'@param dir_las folder with las files in it
#'@param recursive recurse into subdirectories
#'@param pattern pattern to find las/laz files
#'@param ... other arguments to list.files
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
#'
#'@seealso \code{\link{read_dtm}}\cr \code{\link{gridmetrics}}\cr



#'@export

read_header=function(paths=NA
                     ,dir_las=NA
                     ,recursive = F
                     ,pattern = "[.]la.$"
                     , ...
                     ){

  if(is.na(paths[1]) & (!is.na(dir_las[1]))) paths_in = list.files(dir_las,recursive=recursive,pattern=pattern,...)
  else paths_in = paths

  if(length(paths)==0) stop("'path' is empty")
  if(length(paths)==1) try(return(data.frame(t(.read_one_header(paths_in)))))
  if(length(paths) >1) try(return(.read_headers(paths_in)))

}


# read_header.character=function(paths){
#
#   if(length(paths)==0) stop("'path' is empty")
#   if(length(paths)==1) try(return(data.frame(t(.read_one_header(paths)))))
#   if(length(paths) >1) try(return(.read_headers(paths)))
#
# }

.read_headers=function(paths){

  headers=data.frame(do.call(rbind,lapply(paths,function(x)t(.read_one_header(x)))),row.names=NULL)

}

.read_one_header=function(path){


  con <- file(path, open = 'rb')
  rBcon <-readBin(con, 'raw', n = 227, size = 1, signed = F)
  try(close(con))

  phb <- data.frame(
    row.names = c(
      'signature'
      , 'Source_ID'
      , 'Encoding'
      , 'GUID1'
      , 'GUID2'
      , 'GUID3'
      , 'GUID4'
      , 'V_Maj'
      , 'V_Min'
      , 'Sys_ID'
      , 'Software'
      , 'Create_Day'
      , 'Create_Year'
      , 'Header_Size'
      , 'Off2points'
      , 'N_VLR'
      , 'Format_ID'
      , 'Rec_length'
      , 'N_all'
      , 'N_1st'
      , 'N_2nd'
      , 'N_3rd'
      , 'N_4th'
      , 'N_5th'
      , 'Xscale'
      , 'Yscale'
      , 'Zscale'
      , 'Xoffset'
      , 'Yoffset'
      , 'Zoffset'
      , 'MaxX'
      , 'MinX'
      , 'MaxY'
      , 'MinY'
      , 'MaxZ'
      , 'MinZ')

    ,Value = c(
      readBin(rBcon[1:4], 'char', size = 4, n = 1),
      readBin(rBcon[5:6], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[7:8], 'int', size = 2, n = 1),
      readBin(rBcon[9:12], 'int', size = 4, n = 1),
      readBin(rBcon[13:14], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[15:16], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[17:24], 'char', size = 4, n = 1, signed = FALSE),
      readBin(rBcon[25], 'char', size = 1, n = 1, signed = FALSE),
      readBin(rBcon[26], 'char', size = 1, n = 1, signed = FALSE),
      readBin(rBcon[27:58], 'char', size = 32, n = 1),
      readBin(rBcon[59:90], 'char', size = 32, n = 1),
      readBin(rBcon[91:92], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[93:94], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[95:96], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[97:100], 'int', size = 4, n = 1),
      readBin(rBcon[101:104], 'int', size = 4, n = 1),
      readBin(rBcon[105],"int", size = 1, signed = F),
      readBin(rBcon[106:107], 'int', size = 2, n = 1, signed = FALSE),
      readBin(rBcon[108:111], 'int', size = 4, n = 1),
      readBin(rBcon[112:115], 'int', size = 4, n = 1),
      readBin(rBcon[116:119], 'int', size = 4, n = 1),
      readBin(rBcon[120:123], 'int', size = 4, n = 1),
      readBin(rBcon[124:127], 'int', size = 4, n = 1),
      readBin(rBcon[128:131], 'int', size = 4, n = 1),
      readBin(rBcon[132:139], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[140:147], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[148:155], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[156:163], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[164:171], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[172:179], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[180:187], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[188:195], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[196:203], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[204:211], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[212:219], 'double', size = 8, n = 1, signed = TRUE),
      readBin(rBcon[220:227], 'double', size = 8, n = 1, signed = TRUE)
    ),
    stringsAsFactors = FALSE
  )



  isLASF <- phb[1, 1] == 'LASF'

  if (!isLASF){

    try(close(con))
    warning(path, ' is not a valid LAS file')
    phb[,]=NA
  }

  if(isLASF){

    phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\001', 1, phb['V_Min', 1])
    phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\002', 2, phb['V_Min', 1])
    phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\003', 3, phb['V_Min', 1])
    phb['V_Min', 1] <- ifelse(phb['V_Min', 1] == '\004', 4, phb['V_Min', 1])

    phb['V_Maj', 1] <- ifelse(phb['V_Maj', 1] == '\001', 1, phb['V_Maj', 1])
    phb['V_Maj', 1] <- ifelse(phb['V_Maj', 1] == '\002', 2, phb['V_Maj', 1])

    phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\001', 1, phb['Format_ID', 1])
    phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\002', 2, phb['Format_ID', 1])
    phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\003', 3, phb['Format_ID', 1])
    phb['Format_ID', 1] <- ifelse(phb['Format_ID', 1] == '\004', 4, phb['Format_ID', 1])

    #laz files mess with format field
    phb['Format_ID', 1] <- ifelse(as.numeric(phb['Format_ID', 1]) > 127, as.numeric(phb['Format_ID', 1]) - 128, phb['Format_ID', 1])

    if (!isLASF) { warning(path, ' is not a valid LAS/LAZ file') }

    if(phb['V_Min',]=="") phb['V_Min',]=0

    if(as.numeric(phb['V_Maj',])==1 & as.numeric(phb['V_Min',])>3){

      con <- file(path, open = 'rb')
      rBcon <- readBin(con, 'raw', n = 375, size = 1)
      try(close(con))

      phb_add<- data.frame(
        row.names = c(
          'st_wdpr'
          , 'st_vlr'
          , 'n_vlr'
          , 'n_pr'
          , 'n_1'
          , 'n_2'
          , 'n_3'
          , 'n_4'
          , 'n_5'
          , 'n_6'
          , 'n_7'
          , 'n_8'
          , 'n_9'
          , 'n_10'
          , 'n_11'
          , 'n_12'
          , 'n_13'
          , 'n_14'
          , 'n_15'
        ),
        Value = c(
          readBin(rBcon[228:235], 'int', size = 8, n = 1)
          ,readBin(rBcon[236:243], 'int', size = 8, n = 1)
          ,readBin(rBcon[244:247], 'int', size = 4, n = 1)
          ,readBin(rBcon[248:255], 'int', size = 8, n = 1)

          ,readBin(rBcon[256:263], 'int', size = 8, n = 1)
          ,readBin(rBcon[264:271], 'int', size = 8, n = 1)
          ,readBin(rBcon[272:279], 'int', size = 8, n = 1)
          ,readBin(rBcon[280:287], 'int', size = 8, n = 1)
          ,readBin(rBcon[288:295], 'int', size = 8, n = 1)
          ,readBin(rBcon[296:303], 'int', size = 8, n = 1)
          ,readBin(rBcon[304:311], 'int', size = 8, n = 1)
          ,readBin(rBcon[312:319], 'int', size = 8, n = 1)
          ,readBin(rBcon[320:327], 'int', size = 8, n = 1)
          ,readBin(rBcon[328:335], 'int', size = 8, n = 1)
          ,readBin(rBcon[336:343], 'int', size = 8, n = 1)
          ,readBin(rBcon[344:351], 'int', size = 8, n = 1)
          ,readBin(rBcon[352:359], 'int', size = 8, n = 1)
          ,readBin(rBcon[360:367], 'int', size = 8, n = 1)
          ,readBin(rBcon[368:375], 'int', size = 8, n = 1)
        )
        ,stringsAsFactors = FALSE
      )

      phb <- rbind(phb , phb_add)
      if(phb["N_all",]==0){
        phb["N_all",]=phb["n_pr",]
      }
    }

    isLASF <- phb[1, 1] == 'LASF'

    phb[17, 1] <- ifelse(phb[17, 1] == '\001', 1, phb[17, 1])
    phb[17, 1] <- ifelse(phb[17, 1] == '\002', 2, phb[17, 1])
    phb[17, 1] <- ifelse(phb[17, 1] == '\003', 3, phb[17, 1])
    phb[17, 1] <- ifelse(phb[17, 1] == '\004', 4, phb[17, 1])

    phb[8, 1] <- ifelse(phb[8, 1] == '\001', 1, phb[8, 1])
    phb[8, 1] <- ifelse(phb[8, 1] == '\002', 2, phb[8, 1])

    phb[9, 1] <- ifelse(phb[9, 1] == '\001', 1, phb[9, 1])
    phb[9, 1] <- ifelse(phb[9, 1] == '\002', 2, phb[9, 1])


  }

  return(phb)

}


