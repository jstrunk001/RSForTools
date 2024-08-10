#'@title
#'  tweak to lidR::catalog that enables file read failure
#'
#'@description
#'  tweak to lidR::catalog that enables file read failure
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
#'1.0 \tab 2024/03/28 started \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <something@@something.com>
#'
#'@param folder
#'@param  progress  ?
#'@param  select  ?
#'@param  filter  ?
#'@param  chunk_size  ?
#'@param  chunk_buffer  ?
#'@param  ... ?


#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import some_package some_package2
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

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
catalog1 = function (
    folder
    , progress = TRUE
    , select = "*"
    , filter = ""
    , chunk_size = 0
    , chunk_buffer = 30
    , ...
    ) 
{

  lidR:::assert_is_character(folder)
  finfo <- file.info(folder)
  if (all(is.na(finfo$isdir))) 
    stop(glue::glue("'{folder}' does not exist."), call. = FALSE)
  else if (all(!finfo$isdir)) 
    files <- normalizePath(folder)
  else {
    p <- list(...)
    p$path <- folder
    p$full.names <- TRUE
    if (is.null(p$pattern)) 
      p$pattern <- "(?i)\\.la(s|z)$"
    files <- do.call(list.files, p)
  }
  browser()  
  #verbose("Reading files...")
  if (length(files) == 1L && tools::file_ext(files) == "vpc") {
    headers = lidR:::read_vpc(files)
    files = sapply(headers, function(x) x$filename)
    crs = sf::st_crs(headers[[1]]$CRS)
  }
  else {
    header <- LASheader(rlas::read.lasheader(files[1]))
    crs <- st_crs(header)
    phblab <- make.names(names(phb(header)))
    phblab[4] <- "GUID"
    t0 <- Sys.time()
    pb <- NULL
    i <- 0

    headers <- lapply(files, function(x) {
      #if(x=="F:/naip3d/OR/2022/Infocloud_set04/PointCloud_03316.laz") browser()
      header <- try(rlas:::lasheaderreader(x))
      print(x)
      print(class(header))
      if(class(header) == "try-error") return(NULL)
      header <- try(LASheader(header))
      if(class(header) == "try-error") return(NULL)
      PHB <- header@PHB
      names(PHB) <- phblab
      #if (lidR:::use_wktcs(header)) 
      if (T) 
        PHB[["CRS"]] <- wkt(header)
      else PHB[["CRS"]] <- epsg(header)
      if (!is.null(PHB[["Number.of.points.by.return"]])) {
        PHB[["Number.of.1st.return"]] <- PHB[["Number.of.points.by.return"]][1]
        PHB[["Number.of.2nd.return"]] <- PHB[["Number.of.points.by.return"]][2]
        PHB[["Number.of.3rd.return"]] <- PHB[["Number.of.points.by.return"]][3]
        PHB[["Number.of.4th.return"]] <- PHB[["Number.of.points.by.return"]][4]
        PHB[["Number.of.5th.return"]] <- PHB[["Number.of.points.by.return"]][5]
        PHB[["Number.of.points.by.return"]] <- NULL
        PHB[["Global.Encoding"]] <- NULL
      }
      if (progress && Sys.time() - t0 > getOption("lidR.progress.delay")) {
        if (is.null(pb)) 
          pb <<- utils::txtProgressBar(min = 0, max = length(files), 
                                       initial = i, style = 3)
        utils::setTxtProgressBar(pb, i)
      }
      i <<- i + 1
      PHB$filename = x
      return(PHB)
    })
  }
 
  headers <- data.table::rbindlist(headers)
  #headers$filename <- files
  data.table::setDF(headers)
  xmin <- headers$Min.X
  xmax <- headers$Max.X
  ymin <- headers$Min.Y
  ymax <- headers$Max.Y
  ids <- as.character(seq_along(headers$filename))

  geom <- lapply(seq_along(ids), function(xi) {
    mtx <- matrix(c(xmin[xi], xmax[xi], ymin[xi], ymax[xi])[c(1, 
                                                              1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
    sf::st_polygon(list(mtx))
  })
  geom <- sf::st_sfc(geom)
  sf::st_crs(geom) <- crs
  headers <- sf::st_set_geometry(headers, geom)
  res <- new("LAScatalog")
  res@data <- headers
  opt_filter(res) <- filter
  opt_select(res) <- select
  opt_chunk_size(res) <- chunk_size
  opt_chunk_buffer(res) <- chunk_buffer
  opt_progress(res) <- progress
  if (is.overlapping(res)) 
    message("Be careful, some tiles seem to overlap each other. lidR may return incorrect outputs with edge artifacts when processing this catalog.")
  return(res)
}