#'@title
#' Search for proj4 string
#'
#'@description
#'
#'helper to parse results form EPSG() in RGDAL and get desired proj4 string
#'
#'@details
#'
#' helper to parse results form EPSG() in RGDAL and get desired proj4 string
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2015 Jan 05 Roxygen header added \cr
#'}
#'
#'
#'
#'
#'@author
#'Jacob Strunk
#'
#'@param details strings to use in searching for projection nam
#'@param EPSG (optional) dataframe of EPSGs as from make_EPSG()
#'@param omit strings not desired - e.g. "ft" when looking for meters
#'@param ...
#'
#'@return
#' data frame of candidates
#'
#'@examples
#'
#' get_proj4(c("NAD83(HARN)","Washington","ft"))
#' get_proj4(c("NAD83(HARN)","Washington","South","ft"))
#'
#'
#'@seealso \code{\link{rgdal}}\cr \code{\link{EPSG}}\cr
#'
#'@import sf
#'
#'@export
get_proj4=function(
                      details=c("HARN","wash","ftUS","nad83")
                      ,EPSG
                      ,omit="deprecated"
                      ,ignore.case=T
                      ,fixed=T
                      ,...
                      ){

           require("rgdal")

          #get list of preojections
           if(missing(EPSG)) EPSG <-make_EPSG()

          #match list of details to "note"
           details_in=details #gsub("[(]","",details)
           note_tmp=EPSG$note #gsub("[(]","",EPSG$note)
           if(ignore.case & fixed) m1=do.call(cbind,lapply(X=tolower(details_in),grepl,x=tolower(note_tmp),fixed=fixed,...))
           else if (fixed)  m1=do.call(cbind,lapply(X=details_in,grepl,x=note_tmp,fixed=fixed,...))
           else if (ignore.case) m1=do.call(cbind,lapply(X=details_in,grepl,x=note_tmp,ignore.case=ignore.case,...))
           else m1=do.call(cbind,lapply(X=details_in,grepl,x=note_tmp,...))

           keep=which(apply(m1,1,prod)==1)
           out_proj=EPSG[keep,]

           if(!missing(omit)) out_proj=out_proj[!grepl(omit,out_proj[,"note"]),]

           out_proj

        }





