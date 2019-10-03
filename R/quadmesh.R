#' Quadmesh for lazyrasters
#'
#' Provides a re-exported generic quadmesh and a method for lazyraster.
#'
#' A quadmesh is a 'rgl::mesh3d' extension, and can be plotted in 3D
#' with 'rgl::shade3d'.
#' @inheritParams quadmesh::quadmesh
#' @param ... arguments passed to as_raster, for both `x` and `z` if necessary
#' @importFrom quadmesh quadmesh
#' @export quadmesh
#' @name quadmesh
#' @return a quadmesh, derived from 'rgl::mesh3d'
#' @export
quadmesh.lazyraster <- function (x, z = x, na.rm = FALSE, ...,
                                 texture = NULL,
                                 texture_filename = NULL)  {
  arglist <- list(...)
  ## deal with a sensible size for web context
  if (!"dim" %in% names(arglist)) {
    dim <- grDevices::dev.size("px")
    if (!is.null(getOption("rgl.useNULL"))) {
      dim <- dim/4
    }
    arglist$dim <- dim
  }
  arglist$x <- x
  if (!"resample" %in% names(arglist)) {
    arglist$resample <- "NearestNeighbour"
  }
  r <- do.call(as_raster, arglist)
  if (inherits(z, "lazyraster")) {
    arglist$x <- z
    z <- do.call(as_raster, arglist)
  }
  quadmesh::quadmesh(r, z = z, na.rm = na.rm,
                     texture = texture,
                     texture_filename = texture_filename)

}
