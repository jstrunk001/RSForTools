#'@title
#'  create polygons from dataframe of vertices and ids
#'
#'@description
#'  create polygons from dataframe of vertices and ids
#'
#'@details
#'  create polygons from dataframe of vertices and ids
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
#'@param idxy dataframe with 3 columns - id, x, y - representing the id, x, and y of vertices of polygons
#'
#'@return
#'  SpatialPolygons object
#'
#'@examples
#'  none yet
#'
#'@import sp
#'
#'@export
#
#'@seealso \code{\link{xy2FIApoly}}\cr

#Desired upgrades to this function:
#
#
points2polys=function(idxy){

  spl_idxy=split(idxy[,2:3], idxy[,1])

  polys <- sp::SpatialPolygons(

    mapply(
      function(poly, id) {
        sp::Polygons(list(sp::Polygon(poly)), ID=id)
      }
      , spl_idxy
      , names(spl_idxy))

  )

  polys

}

