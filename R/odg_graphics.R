###
###
###
###   Purpose:   Function related to creation and usage of odg-graphics
###   started:   2019-09-09 (pvr)
###
### #################################################################### ###

## ---- Creation and drafting of  odg graphics -------------------------------------------

#' @title Create New ODG Graphics Object
#'
#' @description
#' Similar to the \code{devtools::use_*} functions, we want to
#' create a new ODG graphics object. This new graphics object
#' will be stored in a file which is copied from
#' a template from a given R-package. By default, the template
#' used is called odg_figure from the rmdhelp package.
#' After copying the template, it can be modified using a
#' pre-defined tool. By default this tool is LibreOffice draw.
#'
#' @details
#' After an odg graphic is created, the tags of the r-code-chunk
#' are changed to invoke the hook-convert function that converts
#' the odg file into pdf or a png graphic format. The hook
#' function must be registered using a call to
#' `knitr::knit_hooks$set()`. See the examples section for more
#' details.
#'
#' @examples
#' \dontrun{
#' # register hook-convert function at beginning of document
#' knitr::knit_hooks$set(hook_convert_odg = rmdhelp::hook_convert_odg)
#' # ... more document content in between
#' # use the following call in a code-junk and directly evaluate the function call
#' rmdhelp::use_odg_graphic(ps_odg_file = "my_odg_graphic.odg")
#' }
#'
#' @param ps_path             file name including path of ODG graphics object file
#' @param ps_cwd              current working directory
#' @param ps_rmd_src          explicit file name of rmd-source file
#' @param ps_odg_template     name of the template to be used
#' @param ps_template_package package from which template should be taken from
#' @param pb_recursive        flag whether missing directory should be created
#' @param pb_edit             flag to indicate whether odg file should be opened
#' @param pb_insert_include   flag indicating whether graphic include command should be inserted into rmd source file
#' @return s_odg_trg          name of and path to the created odg graphics file
#' @export use_odg_graphic
use_odg_graphic <- function(ps_path,
                            ps_cwd              = getwd(),
                            ps_rmd_src          = NULL,
                            ps_odg_template     = "odg_figure",
                            ps_template_package = "rmdhelp",
                            pb_recursive        = TRUE,
                            pb_edit             = TRUE,
                            pb_insert_include   = TRUE){
  ### # extract basename and dirname from ps_odg_file
  s_odg_dir <- dirname(ps_path)
  s_odg_base <- basename(ps_path)
  ### # in recursive mode, if s_odg_dir does not exist, create it
  if (!dir.exists(s_odg_dir)) {
    if (pb_recursive){
      dir.create(s_odg_dir)
    } else {
      stop(" *** ERROR: Cannot find s_odg_dir: ", s_odg_dir, ".\n",
           " ***        If it should be created set pb_recusive = TRUE")
    }
  }

  ### # copying the draft from the template, if it does not exist
  ### # use the local function odg_draft to copy the template
  if ( !file.exists(ps_path) ) {
    s_odg_trg <- odg_draft( file        = ps_path,
                            template    = ps_odg_template,
                            package     = ps_template_package )

  } else {
    s_odg_trg <- ps_path
  }

  ### # in case pb_edit is TRUE, open the created draft file
  if (pb_edit){
    s_odg_tool_path <- get_odg_prog_path()
    s_odg_edit_cmd <- paste(s_odg_tool_path, "--draw", s_odg_trg)
    system(s_odg_edit_cmd)
  }

  ### # try to insert include_graphics command into rmd
  if (pb_insert_include)
    insert_include_command(ps_path = ps_path, ps_rmd_src = ps_rmd_src, ps_cwd = ps_cwd)

  ### # return name of odg target
  return(s_odg_trg)

}


#' @title Copy a draft template file for a odg graphics
#'
#' @description
#' This function \code{odg_draft} works analogously to
#' \code{rmarkdown::draft}, but for ODG graphics files.
#' The template can either be specified with an explicit
#' path or in connection with the parameter package where
#' the latter assumes that package is installed and contains
#' a directory \code{rmarkdown/templates/<template_name>}.
#' In both options the template-directory must contain a file
#' called template.yaml with meta information about the template
#' and a subdirectory skeleton with all the files that
#' are to be copied to the directory where the target file
#' is expected to be. When the parameter \code{create_dir} is
#' specified either as function parameter or as meta-information
#' a separate directory for the target file is created. In case
#' it is needed, the appropriate file extension is pasted to the
#' name of the target file. In this specific case the extension
#' we are using here is .odg. The list of files in the
#' skeleton subdirectory of the template directory is copied
#' to the path where the target file is supposed to be.
#' The copying of the files can be specified with an
#' option that indicates whether existing files should be
#' overwritten. The last step consists of renaming the
#' skeleton-file to the basename given in the file
#' parameter.
#'
#' @param   file          name of the and path to the new document
#' @param   template      name of the template
#' @param   package       package where template can be found
#' @param   create_dir    whether or not to create a new directory for this document
#' @param   pbOverwrite   should existing files be overwritten
#' @return  file          name of the and path to the new document
odg_draft <- function(file,
                      template    = "odg_figure",
                      package     = NULL,
                      create_dir  = "default",
                      pbOverwrite = FALSE){
  ### # determine the template path which is contained
  ### #  in package "package"
  if (!is.null(package)) {
    template_path = system.file("rmarkdown", "templates",
                                template, package = package)
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ",
           package, " package")
    }
  } else {
    template_path <- template
  }
  ### # read info in template.yaml
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '",
         template,"'")
  }
  ### # read yaml info from file into variable
  template_meta <- rmarkdown:::yaml_load_file(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }
  ### # check whether function parameter or meta info specify whether a
  ### #  separate new directory for file must be created
  if (identical(create_dir, "default"))
    create_dir <- isTRUE(template_meta$create_dir)
  if (create_dir) {
    file <- tools::file_path_sans_ext(file)
    if (dir.exists(file))
      stop("The directory '", file, "' already exists.")
    dir.create(file, recursive = TRUE)
    file <- file.path(file, basename(file))
  }
  ### # error, in case file itself already exists
  if (!identical(tolower(tools::file_ext(file)), "odg"))
    file <- paste(file, ".odg", sep = "")
  if (file.exists(file))
    stop("The file '", file, "' already exists.")
  ### # generate a list of skeleton files
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  to <- dirname(file)
  for (f in skeleton_files) {
    file.copy(from = f, to = to, overwrite = pbOverwrite, recursive = TRUE)
  }
  ### # rename skeleton to final name
  file.rename(file.path(dirname(file), "skeleton.odg"), file)

  ### # return result file to caller
  return(file)

}

#' @title Insert Graphics Inclusion Command Into Rmd-Source
#'
#' @description
#' When creating a new graphics object, this function tries to automatically
#' generate the associated command to include the generated graphics object
#' into the respective Rmd-source document. This process needs as input
#' the current working directory where of the Rmd-source document and the
#' name of the file in which the graphics object is stored.
#'
#' @param ps_path    path to the odg graphics file
#' @param ps_rmd_src name of and path to the rmd-source file
#' @param ps_cwd     current working directory of the rmd-source file
insert_include_command <- function(ps_path,
                                   ps_rmd_src = NULL,
                                   ps_cwd     = get_wd()){
  ### # in case rmd-source file is not given by ps_rmd_src parameter, try
  ### #  to determine it via a search of ps_path in all files in ps_cwd
  if (is.null(ps_rmd_src)){
    # s_rmd_src <- get_current_rmd_src(ps_path = ps_path, ps_cwd = ps_cwd)
    l_rmd_src <- get_rmd_src_with_pos(ps_path = ps_path, ps_cwd = ps_cwd)

  } else {
    l_rmd_src <- list(name = ps_rmd_src,
                      position = get_pat_pos(ps_file = ps_rmd_src, ps_pattern = ps_path))
  }

  ### # if rmd-source file was specified or was found, we try and insert
  if (!is.null(l_rmd_src$position)){
    con_rmd <- file(description = l_rmd_src$name)
    vec_rmd_src <- readLines(con = con_rmd)
    close(con_rmd)
    ### # loop over positions and insert the include command
    for (pids in seq_along(l_rmd_src$position)){
      p <- l_rmd_src$position[pids]
      ### # change chunk options
      vec_rmd_src[p-1] <- paste0('```{r ', tools::file_path_sans_ext(basename(ps_path)),
                                 ', echo=FALSE, hook_convert_odg=TRUE, fig_path="',
                                 dirname(ps_path), '", out.width="100%"}')
      vec_rmd_src[p] <- paste0('#', vec_rmd_src[p])
      vec_rmd_src[p+1] <- paste0('knitr::include_graphics(path = "',
                                 gsub(pattern = "odg$", replacement = "png", x = ps_path),
                                 '")\n',
                                 vec_rmd_src[p+1], collapse = '')

    }
    ### # write output back again
    cat(paste0(vec_rmd_src, collapse = "\n"), "\n", file = l_rmd_src$name)

  } else {
    stop(" * ERROR in insert_include_command: No position in rmd-source: ", l_rmd_src$name, "\n")
  }

  return(invisible(TRUE))
}

#' Return rmd source file with positions where search pattern occurs
#'
#' Given a search pattern in ps_path and given the path of the current
#' working directory, all files with extension .Rmd are searched whether
#' the string in ps_path occurs in the rmd-source file. If the pattern
#' is found in exactly one Rmd-source file, the name of the Rmd-source
#' file and the positions where the pattern was found is returned. If
#' the pattern is not found or the pattern occurs in more than one
#' Rmd-source file, NULL is returned.
#'
#' @param ps_path name of and path to odg-graphics file
#' @param ps_cwd current working directory
#'
#' @return l_rmd_src_result list with name of rmd source file and positions where search pattern occurs
get_rmd_src_with_pos <- function(ps_path, ps_cwd){
  ### # initialize the result to be NULL
  l_rmd_src_result <- NULL
  ### # get the name of all Rmd-source documents in ps_cwd
  vec_rmd_src <- list.files(path = ps_cwd, pattern = "Rmd$", full.names = TRUE)
  ### # in case any Rmd-source files are found search through them
  if (length(vec_rmd_src) > 0){
    l_rmd_src <- lapply(vec_rmd_src, function(x) get_pat_pos(x, ps_pattern = ps_path))
    ### # get indices of l_rmd_src of entries which are not NA
    n_rmd_src_idx <- which(!is.na(l_rmd_src))

    ### # result is only used, if s_pattern occurs in just one file
    if (length(n_rmd_src_idx) == 1)
      l_rmd_src_result <- list(name = vec_rmd_src[n_rmd_src_idx], position = l_rmd_src[[n_rmd_src_idx]])
  }
  ### # return result
  return(l_rmd_src_result)
}

#' Get position where search pattern occurs in a file
#'
#' Given a search pattern and given a file that is specified
#' by its complete path, the positions where the search
#' pattern occurs is returned. Positions correspond to
#' vector indices when the content of the file is read
#' using the function \code{readLines}. These vector indices
#' are equivalent to line numbers of the text. In case
#' when the search pattern is not found, NA is returned.
#'
#' @param ps_pattern search pattern for which we search in the file
#' @param ps_file name of the file to search for pattern
#' @return vec_pos_found vector of positions where pattern occurs,
#'                       NA if pattern was not found
get_pat_pos <- function(ps_file, ps_pattern){
  ### # check wheter ps_file is found
  if (!file.exists(ps_file)) stop(" *** * ERROR[has_file_search_pat]: cannot find file ", ps_file)
  ### # open connection to ps_file
  con <- file(description = ps_file)
  ### # read content into character vector
  vec_rmd_src <- readLines(con = con)
  ### # close connection con
  close(con)
  ### # search for pattern and return the positions
  ### #  where the pattern was found
  vec_pos_found <- grep(pattern = ps_pattern, x = vec_rmd_src, fixed = TRUE)
  ### # return the result, if pattern was not found
  ### #  return NA
  if (length(vec_pos_found) == 0) return(NA)
  return(vec_pos_found)
}


# Odg Conversion functions ---------------------------------------------------------

#' Odg Graphics File Conversion Hook
#'
#' @description
#' Graphics objects in odg-format are converted to png
#' or pdf format by this knitr-hook-function. We assume
#' that this hook-function is included in the chunk where
#' the graphics object is included using a call to
#' \code{knitr::include_graphics()}, hence the conversion
#' of the odg-file must be run before the code in the chunk.
#'
#' @param before   flag whether hook is run before or after chunk
#' @param options  list of options passed from chunk to hook
#' @param envir    environment
#'
#' @examples
#' \dontrun{
#' # registration of hook function
#' knitr::knit_hooks$set(hook_convert_odg = rmdhelp::hook_convert_odg)
#' ...
#' }
#'
#' @export hook_convert_odg
hook_convert_odg <- function(before, options, envir){
  ### # in the chunk that contains the call to knitr::include_graphics,
  ### #  the conversion must be run before the code in the chunk
  if (before){
    ### # set some defaults for parameter or take them from options
    odg_path <- paste(options$label, "odg", sep=".")
    ### # take only fig_path from options and file name from label
    if (!is.null(options$fig_path)){
      odg_path <- file.path(options$fig_path, paste(options$label, "odg", sep="."))
    }
    if (!is.null(options$odg_path)){
      odg_path <- options$odg_path
    }
    ### # check that file specified by odg_path exists, o/w stop
    if (!file.exists(odg_path))
      stop(" *** * ERROR [rmdhelp::hook_convert_odg]: Cannot find odg-file: ", odg_path)

    ### # determine to which output formats we want to convert the odg file
    out_format <- c("pdf", "png")
    if (!is.null(options$out_format)){
      out_format <- options$out_format
    }
    ### # loop over formats and convert
    for(fmt in out_format){
      convert_odg(ps_odg_path = odg_path, ps_out_format = fmt)
    }

  }
  return(invisible(NULL))

}

#' Converter Function from Odg to Other Formats
#'
#' @description
#' The conversion is done using the tool returned by \code{get_odg_prog_path()}.
#' The current version of this function is not vectorized, hence the arguments
#' can only be single odg-files in ps_odg_path and single formats in ps_out_format.
#' The conversion is only done, if the result file does not exist or if the
#' result file has an older modification time as found by \code{file.mtime()}.
#'
#' @param ps_odg_path   path to odg file to be converted
#' @param ps_out_format output format into which ps_odg_path should be converted to
convert_odg <- function(ps_odg_path, ps_out_format){
  ### # check restriction of only one odg-file and only
  ### #  one format
  if (length(ps_odg_path) > 1 | length(ps_out_format) > 1)
    stop(" *** * ERROR [rmdhelp:::convert_odg] works only on single arguments")

  ### # check whether conversion result already exists
  s_out_file <- paste(tools::file_path_sans_ext(basename(ps_odg_path)), ps_out_format, sep = ".")
  s_out_path <- file.path(dirname(ps_odg_path), s_out_file)
  ### # return here if result file s_out_path exists
  # cont here
  if (file.exists(s_out_path) && (file.mtime(ps_odg_path) < file.mtime(s_out_path)))
    return(invisible(TRUE))

  ### # get the path to the conversion tool
  s_odg_prog_path <- get_odg_prog_path()
  ### # add options and format to conversion command
  s_conv_cmd <- paste0(s_odg_prog_path, " --headless --convert-to ", ps_out_format, " ", ps_odg_path)
  ### # do the conversion
  system(command = s_conv_cmd)
  ### # put the generated result file in the same directory as ps_odg_path
  file.rename(from = s_out_file, to = s_out_path)

  ### # do not return anything
  return(invisible(TRUE))

}

## --- Helper functions related to Odg-graphics -------------------------------------------------
#
#' @title Return Path To Program Used To Create ODG-graphics
#'
#' @description
#' The path to the program used to create odg-graphics depends
#' on the OS. We use the function get_os() to determine on which
#' OS, we are running and based on that, we are returning a
#' fixed string corresponding to the path to the program for
#' odg-graphics. In most cases,
#' LibreOffice draw is used to create the odg-graphics files.
#'
#' @examples
#' \dontrun{
#' get_odg_prog_path()
#' # returns /Applications/LibreOffice.app/Contents/MacOS/soffice on OSX
#' }
#' @export get_odg_prog_path
get_odg_prog_path <- function(){
  ### # first we have to know the os
  s_os <- get_os()
  ### # fix the path according to the os
  if (s_os == "windows"){
    return("c/Program Files/LibreOffice/program/soffice")
  } else if (s_os == "osx"){
    return("/Applications/LibreOffice.app/Contents/MacOS/soffice")
  } else {
    return("soffice")
  }
}

## --- Show knit hook command
#'
#' @title Show Knit Hook Command
#'
#' @description
#' In every document that contains a graphic or diagram, we have to add the
#' odg-conversion function as knit-hook. This statement is not easy to remember
#' and therefore, we want a short cut with a simple function
#'
#' @examples
#' \dontrun{
#' show_knit_hook_call()
#' }
#' @export show_knit_hook_call
show_knit_hook_call <- function(){
  return("knitr::knit_hooks$set(hook_convert_odg = rmdhelp::hook_convert_odg)")
}

