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

