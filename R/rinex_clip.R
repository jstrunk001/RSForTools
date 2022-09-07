#'@title
#'  clip rinex 3.05 files
#'
#'@description
#'  clip rinex 3.05 files based on starting and ending epoch
#'
#'@details
#'
#'  Clip rinex files based on epoch number - for example in a 1-second epoch file you
#'  could clip epochs 30 to 60 from a rinex file. It does not consider the time interval
#'  you must already new that e.g., a file has 20 second epochs and you want 30 of them to
#'  represent 10 minutes...
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
#'1.0 \tab 8/15/2022 Create \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param dir_rinex string, directory of input rinex files
#'@param dir_out string, where to send clipped rinex files
#'@param path_rinex full path to single or multiple rinex file
#'@param path_out full path to single or multiple output rinex file
#'@param start_epoch integer, which epoch to start clip
#'@param finish_epoch integer, which epoch to finish clip
#'@param ignore.short.file T/F some files have fewer epochs than we wish to clip, should this be ignored?
#'@param version_rnx "3.05" only rinex version 3.05 is currently supported - or perhaps other versions where epochs are indicated with ">" symbol
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
#update header with last epoch

# x = function(x){}

#copy function arguments and use this code to format arguments
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))
#
#various helpers:
# bs<- function(){path <- shQuote(gsub("\\", "\\\\", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# fs<- function(){path <- shQuote(gsub("\\", "/", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep="");writeClipboard(x);return(x)}


	rinex_clip = function(
													  dir_rinex=NA
													, dir_out=NA
													, path_rinex = NA
													, path_out = NA
													, start_epoch=1
													, finish_epoch = NA
													, ignore.short.file = T
													, version_rnx="3.05"
													, return_rinex = F

		){

		if(version_rnx=="3.05" | version_rnx==3.05){

			if(is.na(dir_rinex) & length(path_rinex)==1)
				.rinex305_clip(path_rinex=path_rinex, dir_out=dir_out, path_out=path_out , start_epoch=start_epoch, finish_epoch=finish_epoch , ignore.short.file=ignore.short.file)

			if(!is.na(dir_rinex) | length(path_rinex)>1){

				#handle vectors of inputs
				if(length(path_rinex)>1){
					files_rinex = path_rinex
				}else{
					files_rinex = list.files(dir_rinex, pattern="[.]..o",ignore.case=T)
				}

				#handle vectors of outputs
				if(length(path_out)>1){
					files_out = path_out
				}else{
					files_out = file.path(dir_out, basename(files_rinex))
				}

				#run clipping across multiple files
				mapply(
					.rinex305_clip
					, path_rinex = files_rinex
					, path_out = files_out
					, MoreArgs = list( start_epoch=start_epoch, finish_epoch=finish_epoch
					                   , ignore.short.file=ignore.short.file, return_rinex = return_rinex)
						)

			}


		}


	}


	#clipper for version 3.05
	.rinex305_clip=function(
													  path_rinex = NA
													, path_out = NA
													, dir_out = NA
													, start_epoch=1
													, finish_epoch=60*15+5
													, ignore.short.file = T
													, rinex_txt = NA
													, return_rinex = F
		){

		if(start_epoch >= finish_epoch) stop("start_epoch should be less than finish_epoch")
		if(is.na(path_rinex[1]) & is.na(rinex_txt[1])) stop("rinex not provided")
		if(is.na(path_out) & is.na(dir_out) & is.na(rinex_txt)) stop("output rinex file not provided and return_rinex=F")
		if(is.na(path_out) & !is.na(dir_out)) path_out = file.path(dir_out, basename(path_rinex))

		#read data
		if(!is.na(path_rinex[1])) rinex_in = readLines(path_rinex )
    if(is.na(path_rinex) & !is.na(rinex_txt[1])) rinex_in = rinex_txt

		#find each epoch
		epochs_in = which(grepl(".*>.*",rinex_in))

		#use last epoch in the case
		if(is.na(finish_epoch)) finish_epoch = nrow(rinex_in)

		#get file header
		header_idx = 1:(epochs_in[1] -1)

		#find start and end indices in rinex file
		idx_start = ifelse((start_epoch < length(epochs_in) ) , epochs_in[start_epoch] , stop("start_epoch larger than total epochs in file") )
		idx_finish = ifelse(
													length(epochs_in) > finish_epoch
													, epochs_in[finish_epoch] ,
													if(ignore.short.file) epochs_in[length(epochs_in)] else stop("finish_epoch larger than total epochs in file")  )


		#actually slice
		rin_clip = rinex_in[c(header_idx , idx_start:idx_finish)]

		#upate header for shortened duration
		last_time = sprintf("%-60s", paste(c(strsplit(rinex_in[idx_finish],"\\s+")[[1]][2:7],"GPS"),collapse="    "))

  	rin_clip_hd = rinex_header_edit(
  		edits = list(c(id = "TIME OF LAST OBS", value = last_time ) )
			, rinex_txt = rin_clip
			, return_rnx=T
  	)

		#write sliced rinex to file
		if(!is.na(path_out)) writeLines(	rin_clip_hd,path_out)
		if(return_rinex) return(rin_clip_hd)

	}

	if(F){

		dir_out = "C:\\Users\\jstrunk\\Box\\sync\\projects_jacob\\HPGNSS(Jacob)\\projects\\2018 OFSL GPS AccuracyC\\Data\\all_rovers_rinex3.05_testclip"
		dir_in = "C:\\Users\\jstrunk\\Box\\sync\\projects_jacob\\HPGNSS(Jacob)\\projects\\2018 OFSL GPS AccuracyC\\Data\\all_rovers_rinex3.05"

		dir_out = "C:\\Users\\jacob\\Box\\sync\\projects_jacob\\HPGNSS(Jacob)\\projects\\2018 OFSL GPS AccuracyC\\Data\\all_rovers_rinex3.05_testclip"
		dir_in = "C:\\Users\\jacob\\Box\\sync\\projects_jacob\\HPGNSS(Jacob)\\projects\\2018 OFSL GPS AccuracyC\\Data\\all_rovers_rinex3.05"

		all_rinex = list.files(dir_in, pattern="[.]..o",ignore.case=T,full.names=T)

		rinex_clip( path_rinex = all_rinex[1] , dir_out= dir_out , start_epoch=1 , finish_epoch=30 )
		rinex_clip( path_rinex = all_rinex[1] , path_out= file.path(dir_out, paste0("test1_",basename(all_rinex[1])) ) , start_epoch=1 , finish_epoch=30 )

	}

	rinex_clip( path_rinex = all_rinex[1] , dir_out= dir_out[1] , start_epoch=1 , finish_epoch=60*15 )
