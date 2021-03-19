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

## --- Clean Rmarkdown Directory ----------------------------------------------
#'
#' @title Clean RMarkdown Directory
#'
#' @description
#' RMarkdown docments are often placed in separate directories to avoid cluttering
#' a given directory with output files of different source files. Output files
#' are almost never versioned, hence it is useful to have a function to remove
#' all RMarkdown output files. This function helps to clean up directories from
#' RMarkdown output files.
#'
#' @examples
#' \dontrun{
#' clean_rmd_dir(ps_rmd_dir = 'rmd')
#' }
#'
#' @param ps_rmd_dir RMarkdown source directory
#'
#' @export clean_rmd_dir
clean_rmd_dir <- function(ps_rmd_dir){
  # vector of elements to be cleaned
  vec_element_path <- list.files(path = ps_rmd_dir, pattern = '[^Rmd]$', full.names = 'TRUE')
  # exclude all directories from list of element paths
  vec_element_path <- vec_element_path[!fs::is_dir(path = vec_element_path)]
  # file names from complete paths
  vec_element_files <- basename(vec_element_path)
  # choice for user
  cat("\n * List of elements to be deleted in directory: ", ps_rmd_dir, "\n", paste0(vec_element_files, collapse = ', '), '\n')
  cat("\n * Select on of the following options\n\n")
  cat("\t 1 Delete all files\n")
  cat("\t 2 Delete selected files\n")
  cat("\t 3 Delete no files\n")
  answer <- readline(prompt = " * Specifiy your input: ")
  # action according to users answer
  if (answer == "1"){
    cat ("  ==> Deleting all of the following files: ", paste0(vec_element_files, collapse = ', '), "\n")
    fs::file_delete(path = vec_element_path)
  } else if(answer == "2"){
    cat ("  ==> Delete selected files from: ", paste0(vec_element_files, collapse = ', '), "\n")
    for (p in vec_element_path){
      sel_answer <- readline(prompt = paste0(" ** Delete file ", p, " [y/n] ", collapse = ''))
      if (sel_answer == 'y'){
        cat("    ==> Delete file: ", p, "\n")
        fs::file_delete(path = p)
      }
    }

  } else {
    cat ("  ==> Delete no files\n")
  }

  return(invisible(NULL))
}

