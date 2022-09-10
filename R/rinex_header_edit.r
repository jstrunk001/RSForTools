#'@title
#'  edit rinex 3.05 header keyword values
#'
#'@description
#'  edit rinex 3.05 header keyword values
#'
#'@details
#'
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
#'1.0 \tab 8/15/2022 Created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param path_rinex full path to single or multiple rinex file
#'@param path_out full path to single or multiple output rinex file
#'@param edits list of named vectors list(c(id="", value=""), c(id="", value="")) where id must exactly match a rinex header keyword and value is an appropriate value
#'@param add list of named vectors list(c(id="", value=""), c(id="", value="")) where id must exactly match a rinex header keyword and value is an appropriate value
#'@param version_rnx "3.05" only rinex version 3.05 is currently supported - or perhaps other versions where epochs are indicated with ">" symbol
#'@param rinex_txt instead of providing path, provide rinex text directly
#'@param return_rnx T/F should edited filed be returned as text
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'
#'@export
#

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))
#
#various helpers:
# bs<- function(){path <- shQuote(gsub("\\", "\\\\", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# fs<- function(){path <- shQuote(gsub("\\", "/", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep="");writeClipboard(x);return(x)}


	rinex_header_edit = function(
													  path_rinex =NA
													, path_out=NA
													, edits = list(
															#c(id = "MARKER NAME", value ="new_pt1")
															c(id = "MARKER NAME", value ="some_project")
															#,c(id  = "ANT # / TYPE", value ="-Unknown-           JAVTRIUMPH_2A   NONE ")
														)
													, adds=list(
															#c(id = "MARKER NAME", value ="new_pt1")
															c(id = "MARKER NUMBER", value ="1")
															#,c(id  = "ANT # / TYPE", value ="-Unknown-           JAVTRIUMPH_2A   NONE ")
														)
													, version_rnx="3.05"
													, rinex_txt = NA
													, return_rnx = F
		){


		if(is.na(path_rinex[1]) & is.na(rinex_txt[1])) stop("rinex file not provided")
		if(is.na(path_out[1]) & (!return_rnx[1]) ) stop("output rinex file not provided")

		if(version_rnx=="3.05" | version_rnx==3.05){

			#read data
			if(!is.na(path_rinex)) rinex_in = readLines(	path_rinex)
      if(is.na(path_rinex[1]) & !is.na(rinex_txt[1])) rinex_in = rinex_txt

			if(!is.na(edits[1])){
				for(i in 1:length(edits)){

					idx_i = which(grepl(edits[[i]]["id"], rinex_in))
					rinex_in[idx_i] = paste0(sprintf("%-60s", edits[[i]]["value"]), edits[[i]]["id"])

				}
			}
			if(!is.na(adds[1])){
				for(i in 1:length(adds)){
					rinex_in = c(rinex_in[1:4],paste0(sprintf("%-60s", adds[[i]]["value"]), adds[[i]]["id"]), rinex_in[5:length(rinex_in)])
				}
			}

		}

		if(!is.na(path_out)) writeLines(rinex_in, path_out)
    if(!is.na(return_rnx)) return(rinex_in)

	}

	# #test code
	# if(F){
	#
	# 	all_rinex = list.files(dir_out, pattern="[.]..o",ignore.case=T,full.names=T)
	# 	rinex_header_edit(file.path(dir_out, paste0("test1_",basename(all_rinex[1])) ), file.path(dir_out, paste0("test1_edit_",basename(all_rinex[1])) )  )
	#
	# }
