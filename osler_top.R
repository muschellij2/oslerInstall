source("https://bioconductor.org/biocLite.R")
biocLite(suppressUpdates = TRUE,
         suppressAutoUpdate = TRUE,
         ask = FALSE)
# if (!require("neurocInstall")) {
#########################################
# Checking devtools version
#########################################
get_devtools_version = function() {
  ipacks = installed.packages()
  dtools = ipacks[ ipacks[, "Package"] %in% "devtools", "Version"]
  return(dtools)
}
# needed for bioc stuff
req_version = "1.12.0.9000"
dtools = get_devtools_version()
install_from_cran = FALSE
if (length(dtools) == 0 ) {
  install_from_cran = TRUE
} else {
  # too low of a version
  install_from_cran = compareVersion(dtools, req_version) < 0
}
if (install_from_cran) {
  install.packages("devtools")
}

# now we assume devtools is installed
dtools = get_devtools_version()
if (length(dtools) == 0 ) {
  stop(paste0("devtools tried to install but could not ",
              "- try to install devtools manually.",
              "Version >= ", req_version, " required."
  )
  )
} else {
  comparison = compareVersion(dtools, "1.12.0.9000")
  if (comparison < 0) {
    devtools::install_github("hadley/devtools")
  }
}
message(paste("Using neurocLite version:", pkg_ver))

