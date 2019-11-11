do_package_checks()

if (ci_on_travis() && !tolower(Sys.info()[["sysname"]]) == "darwin") {
  do_pkgdown()
}
