# Author: Robert J. Hijmans, Paul Hiemstra, Steven Mosher
# Date :  January 2009
# Version 0.9
# Licence GPL v3

#repurposed by Jacob Strunk for multivariate responses - May 31, 2017

rasterizeLAS <- function(las, r, fun=compute_metrics2, background=NA, mask=FALSE, update=FALSE, updateValue='all', filename="", na.rm=TRUE, ...) {

  require("data.table")

  las[,"cells"] <- cellFromXY(r, las[,c("X","Y")])
  las_in=data.table(las)
  las_in[,"cells"] <- cellFromXY(r, las_in[,c("X","Y")])

#   #decide whether outputs are data.frame or vector or list
#   test1=fun(subset(las,subset=1:nrow(las)<5))
# 	d1=dim(test1)
# 	if(is.null(d1)) ncols_in = max(fun(subset(las,subset=1:nrow(las)<2)),fun(subset(las,subset=1:nrow(las)<5)))
#   if(!is.null(d1)) ncols_in <- ncol(test1)

	ag1=las_in[,fun(.SD),by=list("cells")]


	ag1=aggregate(cbind("X","Y","Z"),by=list("cells"),data=las_in,FUN=fun)

	#
	# ag1=las_in[,fun(.SD),by=list("cells")]
	#
	# las_in[,{fun(.SD,...)},by=cells]
	#
	# f1=function(x){c(sum(x$X),sum(x$Y))}
	# las_in[,{f1(.SD)},by=cells]
	#
	# las_in[,{fun(.SD)},by=cells]

	return(ag1)

}

