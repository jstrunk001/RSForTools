clip_roi1 = function (las, geometry, ...) {
    if (is.character(geometry)) 
        geometry <- sf::st_as_sfc(geometry, crs = sf::st_crs(las))
    if (is(geometry, "Polygon")) 
        geometry <- sf::st_sfc(sf::st_polygon(list(geometry@coords)), 
            crs = st_crs(las))
    if (inherits(geometry, "Spatial")) 
        geometry <- sf::st_as_sf(geometry)
    if (is(geometry, "sfg")) 
        geometry <- sf::st_sfc(geometry, crs = st_crs(las))
    if (is(geometry, "sf") | is(geometry, "sfc")) {
        if (all(sf::st_is(geometry, "POLYGON") | sf::st_is(geometry, 
            "MULTIPOLYGON"))) {
            return(lidR:::clip_sf(las, geometry))
        }
        else if (all(sf::st_is(geometry, "POINT"))) {
            p <- list(...)
            if (is.null(p$radius)) 
                stop("Clipping using sfc_POINT or SpatialPoints* requires addition of parameter 'radius'.", 
                  call. = FALSE)
            centers <- sf::st_coordinates(geometry)
            ycenter <- centers[, 2]
            xcenter <- centers[, 1]
            radius <- p$radius
            return(clip_circle(las, xcenter, ycenter, radius, 
                data = geometry))
        }
        else stop("Incorrect geometry type. POINT, POLYGON and MULTIPOLYGON are supported.", 
            call. = FALSE)
    }
    else if (is(geometry, "Extent")) {
        xmin <- geometry@xmin
        xmax <- geometry@xmax
        ymin <- geometry@ymin
        ymax <- geometry@ymax
        return(clip_rectangle(las, xmin, ymin, xmax, ymax))
    }
    else if (is.matrix(geometry)) {
        if (!all(dim(geometry) == 2)) 
            stop("Matrix must have a size 2 x 2")
        xmin <- geometry[1]
        xmax <- geometry[3]
        ymin <- geometry[2]
        ymax <- geometry[4]
        return(clip_rectangle(las, xmin, ymin, xmax, ymax))
    }
    else if (is(geometry, "bbox")) {
        xmin <- geometry$xmin
        xmax <- geometry$xmax
        ymin <- geometry$ymin
        ymax <- geometry$ymax
        return(clip_rectangle(las, xmin, ymin, xmax, ymax))
    }
    else {
        stop(paste0("Geometry type ", paste0(class(geometry), 
            collapse = " "), " not supported"))
    }
}