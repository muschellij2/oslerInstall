# oslerInstall package version: 0.1
pkg_ver = '# oslerInstall package version: 0.1'
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

	#' @title OSLER Installer
	#' @description Install function for OSLER packages
	#' @param repo Package name in OSLER
	#' @param release Stable or development version
	#' @param upgrade_dependencies Should dependencies be updated?
	#' passed to \code{\link[devtools]{install}}
	#' @param ... additional arguments passed to
	#' \code{\link[devtools]{install_github}}
	#' @return Result from \code{\link[devtools]{install_github}}
	#' @export
	#' @importFrom devtools install_github
	#' @importFrom utils read.csv
	#' @importFrom utils compareVersion install.packages installed.packages
	osler_install = function(repo,
	                         release = c("stable", "current"),
	                         upgrade_dependencies = FALSE,
	                         ...){
	
	  #############################
	  # Create a data.frame for merging
	  #############################
	  release = match.arg(release)
	
	  df = data.frame(repo = repo, stringsAsFactors = FALSE)
	
	  tab = osler_package_table(long = TRUE)
	  ## import list of packages
	  # error if pkg not in list of packages
	  check_install = df$repo %in% tab$repo
	  if (!all(check_install)) {
	    bad_pkgs = df$repo[!check_install]
	    bad_pkgs = paste(bad_pkgs, collapse = ", ")
	    message(paste0("Available Packages on OSLER are ",
	            paste(unique(tab$repo), collapse = ", ")))
	    stop(paste0("Package(s) ", bad_pkgs,
	                " are not in OSLER"))
	  }
	  tab = merge(df, tab, by = "repo", all.x = TRUE)
	  tab$version = numeric_version(tab$version)
	
	  # pkg = tab$pkg
	  # tab$commit_id = tab[, "commit_id"]
	  tab = split(tab, tab$repo)
	  tab = lapply(tab, function(x) {
	    x$version = x[, "version"]
	    max_version = max(x$version)
	    x = x[ x$version %in% max_version,, drop = FALSE]
	    return(x)
	  })
	  tab = do.call("rbind", tab)
	  tab = data.frame(tab, stringsAsFactors = FALSE)
	  tab$repo = paste0("oslerinhealth/", tab$repo, "@", tab$commit_id)
	
	  if (!upgrade_dependencies) {
	    res = try({
	      results = devtools::install_github(
	        tab$repo,
	        upgrade_dependencies = upgrade_dependencies,
	        ...)
	    })
	    if (inherits(res, "try-error") || any(!results)) {
	      stop("Installation failed, please try with upgrade_dependencies = TRUE")
	    }
	  } else {
	    devtools::install_github(tab$repo,
	                             upgrade_dependencies = upgrade_dependencies,
	                             ...)
	  }
	}
	
	
	#' @rdname osler_install
	#' @aliases oslerLite
	#' @export
	oslerLite = function(...) {
	  osler_install(...)
	}
	
	#' @title Make Full Package Version
	#' @description Makes a package version to have all the same length.
	#' This is helpful when using \code{\link{compareVersion}}.
	#'
	#' @param x Character vector of package versions
	#'
	#' @return Character vector of versions, each with the same length.
	#' @export
	#'
	#' @examples
	#' x = c("1.6", "1.6.0")
	#' compareVersion(x[1], x[2])
	#' x2 = make_full_version(x)
	#' compareVersion(x2[1], x2[2])
	#' x = c("1.6", "1.6.0")
	#' compareVersion(x2[1], x2[2])
	make_full_version = function(x) {
	  nx = names(x)
	  x = as.character(x)
	  r <- lapply(strsplit(x, "[.-]"), as.integer)
	  lx = sapply(r, length)
	  mlx = max(lx)
	  r <- lapply(r, function(ver) {
	    c(ver, rep(0, length = mlx - length(ver)))
	  })
	  x = sapply(r, paste, collapse = ".")
	  names(x) = nx
	  return(x)
	}
	
	
	

	#' @title OSLER Package Table
	#' @description Returns the table of OSLER packages
	#' @return \code{data.frame} of packages with commit IDs
	#' @param path Path to the table of package
	#' @param long Should the data be "long" (with respect to stable/current)
	#' @export
	#'
	#' @note Package information is obtained from
	#' \url{"https://oslerinhealth.org/oslerPackages"}
	#'
	#' @importFrom stats reshape
	#' @examples
	#' osler_package_table()
	osler_package_table = function(
	  path = "https://oslerinhealth.org/oslerPackages",
	  long = FALSE
	) {
	  #############################
	  ## grab list of current OSLER packages
	  #############################
	  args = list(file = path,
	             stringsAsFactors = FALSE, header = TRUE,
	             na.strings = "")
	  suppressWarnings({
	    tab = try( {
	      do.call("read.csv", args)
	    } , silent = TRUE)
	  })
	  if (inherits(tab, "try-error")) {
	    args$file = gsub("^https", "http", args$file)
	    tab = do.call("read.csv", args)
	  }
	
	  colnames(tab) = c("repo",
	                    "version.stable",
	                    "osler_version.stable",
	                    "commit_id.stable",
	                    "version.current",
	                    "osler_version.current",
	                    "commit_id.current")
	
	  tab$v = package_version(tab$version.stable)
	  ss = split(tab, tab$repo)
	  ss = lapply(ss, function(x) {
	    x = x[ order(x$v, decreasing = TRUE), ]
	    x = x[1,,drop = FALSE]
	    x$v = NULL
	    x
	  })
	  tab = do.call("rbind", ss)
	  tab = as.data.frame(tab, stringsAsFactors = FALSE)
	
	  rownames(tab) = NULL
	  if (long) {
	    cn = colnames(tab)
	    varying = cn[ cn != "repo"]
	    tab = reshape(data = tab, direction = "long", idvar = "repo", varying = varying,
	            times = c("current", "stable"), timevar = "release")
	    rownames(tab) = NULL
	  }
	  return(tab)
	}
	
	
	
	#' @title OSLER Packages
	#' @description Returns the vector of OSLER packages
	#' @return \code{vector} of packages available on OSLER
	#' @param ... Arguments passed to \code{\link{osler_package_table}}
	#'
	#' @export
	#'
	#' @examples
	#' osler_packages()
	osler_packages = function(...) {
	  tab = osler_package_table(...)
	  tab = tab$repo
	  tab = unique(tab)
	  return(tab)
	}
# } else {
#   require("neurocInstall")
#   pkgs = devtools::session_info()
#   pkgs = pkgs$packages
#   pkg_ver = pkgs$version[ pkgs$package %in% "neurocInstall"]
#   message(paste0("Using neurocInstall version: ", pkg_ver,
#           ", using neurocInstall::neurocLite for installation.")
#   )
# }
