#' Spin a bash script into Rmd
#'
#' @description
#' This function is derived from \code{knitr::spin()} for R-scripts and
#' is adapted to do a similar conversion of bash scripts into Rmd files.
#' The conversion is done in a simple way and almost no features of the
#' original are provided. This is just a very crude way to have a look
#' at documentation of a shell script in a nicer format such as html
#' or pdf.
#'
#' @param ps_sh_hair path to the bash scirpt
#' @param ps_out_rmd output Rmd file
#'
#' @export spin_sh
#'
#' @examples
#' s_test_script <- system.file('extdata', 'test_script.sh', package = 'rmdhelp')
#' s_test_out <- file.path(tempdir(), 'test_script.Rmd')
#' spin_sh(ps_sh_hair = s_test_script, ps_out_rmd = s_test_out)
#' unlink(s_test_out)
spin_sh <- function (ps_sh_hair, ps_out_rmd = paste0(fs::path_ext_remove(ps_sh_hair), ".Rmd")){
  if (!file.exists(ps_sh_hair))
    stop(" * ERROR cannot find input script: ", ps_sh_hair)

  # read the script content
  vec_bsrc <- readLines(con = ps_sh_hair)

  #' In case the script starts with the path to bash remove it.
  vec_out_src <- vec_bsrc
  if (vec_out_src[1] == '#!/bin/bash')
    vec_out_src <- vec_out_src[-1]

  #' Insert the chunk borders
  #' There are two types of chunks:
  #'
  #' 1. text chunks where text chunks are all lines that start with "#'"
  #' 2. code chunks are all lines that are not text chunks.
  #'
  #' From the definitions given above it seams easier to start with text chunks
  vec_txt_chunks <- grep("^#\\'", vec_out_src)


  #' Start with the beginning of a code chunk
  vec_code_chunk_start_pos <- grep(pattern = '^#\\+', vec_out_src)

  #' End of code chunks
  vec_txt_chunks <- c(vec_txt_chunks, length(vec_out_src))
  vec_code_chunk_end_pos <- sapply(1:length(vec_code_chunk_start_pos),
                                   function(x) vec_txt_chunks[which(vec_code_chunk_start_pos[x]<vec_txt_chunks)[1]]-1,
                                   USE.NAMES = FALSE)
  if (length(vec_code_chunk_start_pos) != length(vec_code_chunk_end_pos))
    stop("ERROR: number of code start pos not the same as code end pos")


  #' ## Replacements to get to a rmd document
  #' The code chunks are augmented with the tiks and engine specs
  vec_out_src[vec_code_chunk_start_pos] <- gsub(pattern = '^#\\+(.*)$', replacement = '```{bash,\\1}', vec_out_src[vec_code_chunk_start_pos])

  #' The code chunk ends are symbolised with three tiks which are added at each beginning of the line
  for (i in seq_along(vec_code_chunk_end_pos)){
    if (vec_out_src[vec_code_chunk_end_pos[i]] == ""){
      vec_out_src[vec_code_chunk_end_pos[i]] <- "```\n"
    } else {
      vec_out_src <- c(vec_out_src[1:(vec_code_chunk_end_pos[i])], "```\n", vec_out_src[(vec_code_chunk_end_pos[i]+1):length(vec_out_src)])
    }
  }

  #' Remove all doxygen comment signs from text
  vec_out_src <- gsub(pattern = "^#\\'[ ]*", replacement = "", vec_out_src )

  cat(paste0(vec_out_src, collapse = "\n"), "\n", file = ps_out_rmd)

  return(invisible(NULL))
}

