#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
merge_las=function(list_las, keep_1st_header=T){

      require(lidR)
      points_merge = do.call(rbind,lapply(list_las,function(x)x@data))
      if(keep_1st_header) return(LAS(points_merge,list_las[[1]]@header))
      if(!keep_1st_header) return(LAS(points_merge))

  }
