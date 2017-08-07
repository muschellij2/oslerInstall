# } else {
#   require("neurocInstall")
#   pkgs = devtools::session_info()
#   pkgs = pkgs$packages
#   pkg_ver = pkgs$version[ pkgs$package %in% "neurocInstall"]
#   message(paste0("Using neurocInstall version: ", pkg_ver,
#           ", using neurocInstall::neurocLite for installation.")
#   )
# }
