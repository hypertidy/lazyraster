if (!tolower(Sys.info()[["sysname"]]) == "darwin") {
  do_package_checks()

if (ci_on_travis()) {
  do_pkgdown()
}
}
