###
###
###
###   Purpose:   Collection of misc helper functions
###   started:   2018-03-22 (pvr)
###
### ################################################### ###
#
#
#' Get name of OS we are running on
#'
#' @description
#' This function returns a simple string depending whether
#' the current instance of R runs on windows, mac osx or
#' linux.
#'
#' @details
#' The function was proposed on the blog post at
#' \url{http://conjugateprior.org/2015/06/identifying-the-os-from-r/}
#' found on R-bloggers.
#'
#' The case of windows was not included in the original
#' version from the blog post. It was included here
#' at the beginning to filter them out at the beginning
#' based on the value of \code{.Platform$OS.type}
#'
#' @return name of operating system which is either
#'   "windows", "osx" or "linux".
#' @export get_os
get_os <- function(){
  ### # added special case of windows which is not
  ### #  in origina version
  if (.Platform$OS.type == "windows")
    return("windows")

  ### # here we are just separating mac osx and linux
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}


#' Get the name of the current rmd file
#'
#' @description
#' Depending on whether this function is called from
#' inside of RStudio or from outside, the rstudioapi
#' is used or rprojroot is used to determine the
#' name of the current rmdfile
#'
#' @return name of current rmd file
#' @export get_this_rmd_file
get_this_rmd_file <- function(){
  # return the current rmd file depending on usage mode
  return(ifelse(rstudioapi::isAvailable(),
                rstudioapi::getSourceEditorContext()$path,
                rprojroot::thisfile()))
}

## --- Document meta-information --------------------------------------
#' Write data-time and user that has done latest changes to a document
#'
#' The information about who has done the latest changes to a document
#' is usually added at the end of a website. This allows the reader
#' of a website to check whether the site was recently updated. For
#' the author, it is an easy way to check whether the most recent
#' version has been uploaded to the server.
#'
#' The information that is returned by this function consists of the
#' time returned by \code{Sys.time()} and the username that is returned
#' by \code{Sys.info()}. The intended way to use this function is to
#' \code{cat()} the result of the function in the last code-junk of
#' a document. See examples for more details.
#'
#' @examples
#' \dontrun{
#' ```{r}
#' cat(rmddochelper::get_latest_change(), "\n")
#' ```
#' }
#'
#' @param ps_msg special message to be used in front of time and user info, defaults to Latest Changes
#' @return st_result containing time and user-part of system info
#' @export get_latest_change
get_latest_change <- function(ps_msg = "Latest Changes"){
  st_result <- paste0("\n---\n\n"," _", ps_msg, ": ",
                      Sys.time(), " (", Sys.info()[["user"]], ")_", collapse = "")
  return(st_result)
}
