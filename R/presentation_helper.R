###
###
###
###   Purpose:   Helper Functions for Presentations
###   started:   2019-09-19 (pvr)
###
### ##################################################### ###

#' Render beamer presentation with notes
#'
#' @description
#' Use the option to add notes to beamer presentations via an
#' included header file that tells latex to include the notes.
#'
#' @param ps_input_path rmarkdown source file with note sections
#' @param ps_output_file pdf output file
#' @param ps_in_header latex header file
#'
#' @export beamer_with_notes
#'
#' @examples
#' \dontrun{
#' beamer_with_notes(ps_input_path = 'sl/w01/lbgfs2019_slides_w01_intro-lbg.Rmd')
#' }
beamer_with_notes <- function(ps_input_path,
                              ps_output_file = paste0(tools::file_path_sans_ext( basename(ps_input_path) ), '_with_notes.pdf'),
                              ps_in_header   = 'header_show_notes.tex'){
  ### # check whether ps_input_path exists
  if (!file.exists(ps_input_path))
    stop("[beamer_with_notes] -- ERROR: CANNOT find input file: ", ps_input_path, "\n")
  ### # get directory of input file
  s_input_dir <- dirname(ps_input_path)
  ### # check, if header file exists, if not create it
  s_in_header_path <- file.path(s_input_dir, ps_in_header)
  if (!file.exists(s_in_header_path)){
    cat("\\setbeameroption{show notes}\n\\setbeamertemplate{note page}[plain]\n", file = s_in_header_path)
  }
  ### # render the rmarkdown input file using an header include file that shows the notes
  rmarkdown::render(input         = ps_input_path,
                    output_format = rmarkdown::beamer_presentation(includes = rmarkdown::includes(in_header = ps_in_header)),
                    output_file   = ps_output_file)
  ### # not returning anything
  return(invisible(TRUE))
}


#' Create latex header file to hide notes
#'
#' @param ps_input_dir rmarkdown input directory
#' @param ps_in_header name of the latex header file
#'
#' @export create_header_hide_notes
#'
#' @examples
#' \dontrun{
#' create_header_hide_notes(ps_input_dir = 'sl/w01')
#' }
create_header_hide_notes <- function(ps_input_dir,
                                     ps_in_header = 'header_hide_notes.tex'){
  ### # check whether ps_input_dir exists
  if(!dir.exists(ps_input_dir))
    stop("[create_header_hide_notes] -- ERROR: CANNOT find input directory: ", ps_input_dir, "\n")
  ### # path for header containing the hide note instruction
  s_in_heaer_path <- file.path(ps_input_dir, ps_in_header)
  ### # write the hide note command
  if (!file.exists(s_in_heaer_path)){
    cat("\\setbeameroption{hide notes}\n\\setbeamertemplate{note page}[plain]\n", file = s_in_header_path)
  }
  ### # not returning anything
  return(invisible(TRUE))

}
