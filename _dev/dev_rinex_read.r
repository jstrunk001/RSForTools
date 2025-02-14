#'@title
#'  read rinex 3.05 file
#'
#'@description
#'  read rinex 3.05 file
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
#'1.0 \tab 11/27/2024 Created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param path_rinex full path to single or multiple rinex file
#'@param rinex_txt instead of providing path, provide rinex text directly
#'@param version_rnx ...
#'@param returns ...
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


	rinex_read = function(
													  path_rinex =NA
													, rinex_txt = NA
													, version_rnx="3.05"
	                        , returns = c("header","body","raw")

		){

	  warning("generic header parser doesn't work very well; observation data not parsed at all")

		if(is.na(path_rinex[1]) & is.na(rinex_txt[1])) stop("rinex file not provided")

		if(version_rnx=="3.05" | version_rnx==3.05){

			#read data
  			if(!is.na(path_rinex)) rinex_in = readLines(	path_rinex)
        if(is.na(path_rinex[1]) & !is.na(rinex_txt[1])) rinex_in = rinex_txt

      #find header lines
        id_end_header = which(grepl("END OF HEADER",rinex_in))
        ids_header = 1:(id_end_header-1)
        ids_body = (id_end_header+1):length(rinex_in)

      #split into header and body
        header_text = rinex_in[ids_header]
        body_text = rinex_in[ids_body]

      #prep rinex data object for return
        rinex_data = list( )
        if(!is.na(path_rinex)) rinex_data[["path_rinex"]] = path_rinex

        #prep header
        if("header" %in% returns){
          header_in = .parse_header(header_text)
          #rinex_data[["header"]] = header_in
          rinex_data = c(rinex_data,header_in)
        }

        #prep body
        if("body" %in% returns){
          body_in = .parse_body(body_text)
          #rinex_data[["body"]] = body_in
          rinex_data = c(rinex_data,body_in)
        }
        #add raw text
        if("body" %in% returns){
          rinex_data[["all.raw"]] = rinex_in
          #rinex_data = c(rinex_data, rinex_in)
        }
		}
		if(version_rnx=="2" | version_rnx=="2" | version_rnx==2){
		  stop("cannot parse rinex version 2 yet - please use version 3.05")
		}

    return(rinex_data)

	}

	.parse_header = function(x){

    #extract PRN
	  ncharsi = sapply(x,nchar)
	  valuesi = sapply(x,substr,1,60)
	  idsi = mapply(substr,x,rep(61,length(x)),ncharsi)
    names(idsi) = NULL
    names(valuesi) = NULL

    #extract PRN records
    ids_prn = grep("PRN",idsi)
    ids_sys = grep("SYS",idsi)
    ids_tags = grep("PRN|SYS",idsi,invert=T)

    #extract tags, numbers of tags
    tagsi = lapply(idsi[ids_tags],function(...)gsub("^[[:space:]]|[[:space:]]$","",unlist(strsplit(...))),"/")
    ntags = sapply(tagsi,length)

    #extract values
    tag_valsi_A = lapply(valuesi[ids_tags],function(x)gsub("^[[:space:]]*|[[:space:]]*$","",x))

    #get values from rows with single tax
    res1 = tag_valsi_A[ntags==1]
    names(res1) = tagsi[ntags==1]

    #compute duration and number of epochs
    res1$time_start = strptime(res1$`TIME OF FIRST OBS`,"%Y %m %d %H %M %OS")
    res1$end_start = strptime(res1$`TIME OF LAST OBS`,"%Y %m %d %H %M %OS")
    res1$duration_sec = as.numeric(difftime (res1$end_start, res1$time_start, units="secs"))
    res1$epochs =  as.numeric(res1$duration_sec) / as.numeric(res1$INTERVAL)

    #parse multi-tag lines
    tagsi2 = tagsi[ntags>1]
    ntags2 = ntags[ntags>1]
    res2 = tag_valsi_A[ntags>1]
    res2b = lapply(unlist(res2),function(x)unlist(strsplit(x,'\\s{2,}')))

    #assign names to values
    for(i in 1:length(res2b)){

        tagi = tagsi2[[i]]
        ntagsi = length(tagi)
        nvalsi = length(res2b[[i]])
        #fix too many tax
        if(ntagsi > nvalsi){
          tagi = tagi[1:nvalsi]
          ntagsi = length(tagi)
        }
        #fix too many values
        if(ntagsi < nvalsi){
          tagi = c(tagi,rep("",10))[1:nvalsi]
          ntagsi = length(tagi)
        }
        #assign names
        names(res2b[[i]]) = tagi
    }
    parse_in = unlist(c(res1,res2b))

    list(header.parse = parse_in, header.raw = x)

	}

	.parse_body = function(x){

	  #parse header for each epoch
	  epochs = x[grepl(">",x)]
	  text_times = sapply(epochs,substr,3,30)
	  times_in = strptime(text_times,"%Y %m %d %H %M %OS")
	  text_flag = sapply(epochs,substr,31,31)
	  text_sat =  sapply(epochs,substr,34,35)
	  #text_offset = sapply(epochs,substr,42,57)
    df_headers = data.frame(time_sec=times_in,nsats=text_sat,flag=text_flag,full_string=epochs,row.names=NULL)

	  #split by epoch
	  spl_dat = split(x,cumsum(grepl(">",x)))

	  #return all epochs and epoch headers
	  list(epoch.headers = df_headers, epoch.raw = spl_dat)

	}

	# #test code
	# if(F){
	#
	# 	all_rinex = list.files(dir_out, pattern="[.]..o",ignore.case=T,full.names=T)
	# 	rinex_header_edit(file.path(dir_out, paste0("test1_",basename(all_rinex[1])) ), file.path(dir_out, paste0("test1_edit_",basename(all_rinex[1])) )  )
	#
	# }
