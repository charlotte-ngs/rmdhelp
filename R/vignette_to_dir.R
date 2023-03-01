


#' Vignette Documents to Directories
#'
#' @description
#' Vignettes are documentation of packages in a paper-like format. The vignettes
#' can be created using the function \code{usethis::use_vignettes()}. This creates
#' a new RMarkdown (Rmd) document in the subdirectory 'vignettes'. So far it is not possible
#' to group different documents in separate directories. This function creates for
#' each Rmd document a separate directory and moves the Rmd document into that subdirectory.
#'
#' @details
#' The creation of the new subdirectory and the move of the created Rmd document
#' done using functions from package 'fs'.
#'
#' @param pvec_rmd_doc Rmd document to be moved. By default all Rmd files in directory 'vignettes' are moved
#' @param ps_trg_dir alternative root target directory, by default: file.path(here::here(), "vignettes")
#' @import here
#' @import fs
#' @export vig_to_dir
#'
#' @examples
#' \dontrun{
#' usethis::use_vignettes("example-vignette")
#' vig_to_dir("vignettes/example-vignette.Rmd")
#' }
vig_to_dir <- function(pvec_rmd_doc = list.files(file.path(here::here(), "vignettes"), pattern = ".Rmd$"),
                       ps_trg_dir   = file.path(here::here(), "vignettes")){
  # loop over documents
  for (d in pvec_rmd_doc){
    s_dir_name <- fs::path_ext_remove(basename(d))
    fs::dir_create(file.path(ps_trg_dir, s_dir_name))
    fs::file_move(file.path(here::here(), "vignettes", d), file.path(ps_trg_dir, s_dir_name))
  }
}



#' Create Vignette Document in Sub-directory
#'
#' @description
#' A vignette is created in a separate subdirectory. The subdirectory by default is
#' the same as the file-name of the vignette. If the vignette name contains a
#' directory path, then it is saved under that directory.
#'
#' @param ps_name Base of vignette file name
#' @param ps_title Title of vignette
#'
#' @import fs
#' @import here
#' @export use_vignette_in_dir
#'
#' @examples
#' \dontrun{
#' use_vignette_in_dir("example-vignette", "Example Vignette")
#' }
use_vignette_in_dir <- function(ps_name,
                                ps_title = fs::path_ext_remove(ps_name)){
  s_name <- basename(fs::path_ext_remove(ps_name))
  # convert name of vignette to a path
  if (basename(s_name) == s_name){
    s_vig_dir <- file.path(here::here(), "vignettes", s_name)
  } else {
    s_vig_dir <- dirname(ps_name)
  }
  # create vignette
  usethis::use_vignette(name = s_name, title = ps_title)
  # move vignette to sub directory
  vig_to_dir(s_name, ps_trg_dir = s_vig_dir)
}
