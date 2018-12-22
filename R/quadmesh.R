#' @inheritParams quadmesh::quadmesh
#' @param ... arguments passed to as_raster, for both `x` and `z` if necessary
#' @importFrom quadmesh quadmesh
#' @export quadmesh
#' @export
quadmesh.lazyraster <- function (x, z = x, na.rm = FALSE, ...,
                                 texture = NULL,
                                 texture_filename = NULL)  {
  r <- as_raster(x, ...)
  if (inherits(z, "lazyraster")) {
    z <- as_raster(z, ...)
  }
  quadmesh::quadmesh(r, z = z, na.rm = na.rm,
                     texture = texture,
                     texture_filename = texture_filename)

}
