###
###
###
###    Purpose:   Create Gh-root project
###    started:   2019-09-08 (pvr)
###
### ######################################### ###


#' @title Create a gh-root project
#'
#' @description
#' GH-root projects that have a master branch and a gh-pages
#' branch in different subdirectories under a common root
#' which is called the gh-root.
#'
#' @param ps_project   projectname
#' @param ps_path      path under which project is created
#'
#' @export create_ghroot_project
create_ghroot_project <- function(ps_project, ps_path = '.', pb_verbose = FALSE){
  ### # check that path already exists
  if (!dir.exists(ps_path)) stop(" * ERROR: Project path does not exist: ", ps_path)
  ### # add _ghroot to project name
  s_ghroot_project <- paste0(ps_project, '_ghroot')
  s_ghroot_path <- file.path(ps_path, s_ghroot_project)
  ### # check that directory does not exist yet
  if (dir.exists(s_ghroot_path)) stop (" * ERROR: Cannot create ghroot project in already existing directory: ", s_ghroot_path)
  ### # create branch subdirectories
  for (b in c('master', 'gh-pages')){
    cur_dir <- file.path(s_ghroot_path, b)
    if (pb_verbose) cat(" * create_ghroot_project: create branchdir: ", curdir, "\n")
    dir.create(path = cur_dir, recursive = TRUE)
  }
  return(invisible(TRUE))
}
