


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
    s_vig_name <- d
    if (fs::path_ext(s_vig_name) != "Rmd") fs::path_ext(s_vig_name) <- "Rmd"
    fs::dir_create(file.path(ps_trg_dir, s_dir_name))
    fs::file_move(file.path(here::here(), "vignettes", s_vig_name), file.path(ps_trg_dir, s_dir_name))
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
#' @param ps_vig_dir alternative target directory for document root (default: vignettes)
#' @param ps_title Title of vignette
#' @param ps_template Name of template to be used
#' @param pb_open Directly open the vignette
#'
#' @import fs
#' @import usethis
#' @import glue
#' @import rmarkdown
#' @export use_vignette_in_dir
#'
#' @examples
#' \dontrun{
#' use_vignette_in_dir("example-vignette", "Example Vignette")
#' }
use_vignette_in_dir <- function(ps_name,
                                ps_vig_dir  = NULL,
                                ps_title    = fs::path_ext_remove(ps_name),
                                ps_template = "vignette",
                                ps_pkg      = "rmdhelp",
                                pb_open     = TRUE){
  s_name <- basename(ps_name)
  # convert name of vignette to a path
  if (is.null(ps_vig_dir)){
    s_vig_dir <- file.path("vignettes", fs::path_ext_remove(s_name))
  } else {
    s_vig_dir <- file.path(ps_vig_dir, fs::path_ext_remove(s_name))
  }
  s_vig_file <- s_name
  if (fs::path_ext(s_vig_file) != ".Rmd") fs::path_ext(s_vig_file) <- "Rmd"
  s_vig_path <- file.path(s_vig_dir, s_vig_file)
  # check whether s_vig_dir exists
  if (!dir.exists(s_vig_dir)) dir.create(s_vig_dir, recursive = TRUE)
  # create vignette document using rmarkdown::draft
  rmarkdown::draft(s_vig_path, template = ps_template, package = ps_pkg, edit = FALSE)
  # replace template
  con_vig <- file(s_vig_path)
  vec_vig <- readLines(con_vig)
  close(con_vig)
  s_cur_date <- format(Sys.time(), '%Y-%m-%d')
  s_result_vig <- glue::glue(paste0(vec_vig, collapse = "\n"),
                             vignette_title        = ps_title,
                             braced_vignette_title = ps_title,
                             start_date = s_cur_date,
                             .open  = "[[[",
                             .close = "]]]")
  # write output
  cat(s_result_vig, "\n", sep = "", file = s_vig_path)
  # open file for editing, if specified
  if (pb_open) usethis::edit_file(s_vig_path)
  # return invisible
  invisible()
}
