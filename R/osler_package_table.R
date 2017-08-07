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
