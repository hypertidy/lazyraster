.onLoad <- function(libname, pkgname)  {
  if (requireNamespace("rgdal", quietly = TRUE)) {
   .warn <- rgdal::get_rgdal_show_exportToProj4_warnings()
   rgdal:::set_rgdal_show_exportToProj4_warnings(FALSE)
  }
}
.onUnload <- function(libpath) {
  if (requireNamespace("rgdal", quietly = TRUE)) {
    rgdal:::set_rgdal_show_exportToProj4_warnings(.warn)
  }
}
