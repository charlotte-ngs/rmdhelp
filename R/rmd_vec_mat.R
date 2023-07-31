#' ---
#' title: Vectors and Matrices in Rmarkdown
#' date:  2020-05-25
#' ---
#'
#'
#' @title Generic Converter of Matrices To Rmarkdown
#'
#' @description
#' The generic conversion takes a matrix and converts it into one
#' of the constructs recognized by Rmarkdown as described on
#' https://bookdown.org/yihui/rmarkdown/markdown-syntax.html.
#'
#' @details
#' This function is not called directly, but is used by different wrappers
#' for the different output types.
#'
#' @param pmat matrix to be converted to Rmarkdown
#' @param ps_name variable name for the matrix
#' @param ps_equal_sign equal sign to be used when a variable name is given
#' @param ps_env math environment in which matrix should be presented
#' @param ps_out_type one of the output types available in Rmarkdown
#' @param ps_out_format output format produced
#' @return sresult String containing Rmarkdown representation of given vector or matrix
rmd_vec_mat <- function(pmat,
                        ps_name       = NULL,
                        ps_equal_sign = '=',
                        ps_env        = NULL,
                        ps_out_type   = 'array',
                        ps_out_format = 'html') {
  # initialise result variables
  sresult <- NULL
  sfooter <- NULL
  # put env start, if specified by ps_env
  if (!is.null(ps_env)){
    if (substr(ps_env, 1, 1) == '$'){
      sresult <- ps_env
      sfooter <- ps_env
    } else {
      sresult <- paste0('\\begin{', ps_env, '} ', collapse = '')
      sfooter <- paste0('\\end{', ps_env, '} ', collapse = '')
    }
  }
  # put content of matrix, determine dimension of matrix
  ncol <- ncol(pmat)
  nrow <- nrow(pmat)
  # bmatrix with more than 10 columns requires setting max column counter,
  # according to: https://tex.stackexchange.com/questions/3519/how-to-use-more-than-10-tab-stops-in-bmatrix-or-other-amsmath-matrix-environment
  if (ps_out_type == "bmatrix" && ncol > 10L && ps_out_format == "latex"){
    sresult <- paste0(sresult, '\\setcounter{MaxMatrixCols}{', ncol, '} ', collapse = '')
  }
  # name if one is specified
  if (!is.null(ps_name)){
    sresult <- paste0(sresult, ps_name, ' ', ps_equal_sign, ' ', collapse = '')
  }

  # put beginning of environment of specifified output type
  sresult <- paste0(sresult, '\\begin{', ps_out_type, '} ', collapse = '')
  # loop over first to second to last rows
  if (nrow > 1){
    for (idx in 1:(nrow-1)){
      if (ncol > 1){
        sresult <- paste0(sresult, paste0(paste0(pmat[idx,], collapse = " & "), " \\\\", sep = ""), collapse = '')
      } else {
        sresult <- paste0(sresult, paste0(pmat[idx,], " \\\\", sep = ""), collapse = '')
      }
    }

  }
  # add last row
  if (ncol > 1){
    sresult <- paste0(sresult, paste0(pmat[nrow,], collapse = " & "), collapse = '')
  } else {
    sresult <- paste0(sresult, pmat[nrow,], collapse = '')
  }

  # put end of bmatrix
  sresult <- paste0(sresult, '\\end{', ps_out_type, '}', collapse = '')
  # put footer
  if (!is.null(sfooter)){
    sresult <- paste0(sresult, sfooter, collapse = '')
  }

  return(sresult)
}

#'                                                                           #
#' ---- bmatrix Wrapper ---------------------------------------------------- #
#'
#' @title Convert Matrix To bmatrix in Rmarkdown
#'
#' @description
#' The specified matrix is converted to a bmatrix in Rmarkdown
#'
#' @param pmat matrix to be converted to Rmarkdown
#' @param ps_name variable name for the matrix
#' @param ps_equal_sign equal sign to be used when a variable name is given
#' @param ps_env math environment in which matrix should be presented
#' @param ps_out_format output format produced
#' @return String containing bmatrix representation of given matrix
#'
#' @examples
#' \dontrun{
#' eye3 <- diag(1, nrow = 3)
#' bmatrix(pmat = eye3, ps_name = 'I', ps_env = '$$')
#' }
#'
#' @export bmatrix
bmatrix <- function(pmat,
                    ps_name = NULL,
                    ps_equal_sign = '=',
                    ps_env = NULL,
                    ps_out_format = 'html'){
  # check type of pmat
  if (! is.matrix(pmat))
    stop(" *** ERROR: specified pmat is not a matrix")
  # Use bmatrix as output type
  return(rmd_vec_mat(pmat          = pmat,
                     ps_name       = ps_name,
                     ps_equal_sign = ps_equal_sign,
                     ps_env        = ps_env,
                     ps_out_type   = 'bmatrix',
                     ps_out_format = ps_out_format))
}

#'                                                                           #
#' ---- bcolumn_vector Wrapper --------------------------------------------- #
#'
#' @title Convert Column Vector To bmatrix in Rmarkdown
#'
#' @description
#' The specified column vector is converted to a bmatrix in Rmarkdown
#'
#' @param pvec vector to be converted to Rmarkdown
#' @param ps_name variable name for the vector
#' @param ps_equal_sign equal sign to be used when a variable name is given
#' @param ps_env math environment in which matrix should be presented
#' @param ps_out_format output format produced
#' @return String containing bmatrix representation of given vector
#'
#' @examples
#' \dontrun{
#' example_vec <- c(1:5)
#' bcolumn_vector(pvec = example_vec, ps_name = 'v', ps_env = '$$')
#' }
#'
#' @export bcolumn_vector
bcolumn_vector <- function(pvec,
                           ps_name = NULL,
                           ps_equal_sign = '=',
                           ps_env = NULL,
                           ps_out_format = 'html'){
  # check type of pmat
  if (! is.vector(pvec))
    stop(" *** ERROR: specified pmat is not a vector")
  # Use bmatrix as output type
  return(rmd_vec_mat(pmat          = matrix(pvec, ncol = 1),
                     ps_name       = ps_name,
                     ps_equal_sign = ps_equal_sign,
                     ps_env        = ps_env,
                     ps_out_type   = 'bmatrix',
                     ps_out_format = ps_out_format))
}

#'                                                                           #
#' ---- brow_vector Wrapper ------------------------------------------------ #
#'
#' @title Convert Row Vector To bmatrix in Rmarkdown
#'
#' @description
#' The specified row vector is converted to a bmatrix in Rmarkdown
#'
#' @param pvec vector to be converted to Rmarkdown
#' @param ps_name variable name for the vector
#' @param ps_equal_sign equal sign to be used when a variable name is given
#' @param ps_env math environment in which matrix should be presented
#' @param ps_out_format output format produced
#' @return String containing bmatrix representation of given vector
#'
#' @examples
#' \dontrun{
#' example_vec <- c(1:5)
#' brow_vector(pvec = example_vec, ps_name = 'v', ps_env = '$$')
#' }
#'
#' @export brow_vector
brow_vector <- function(pvec,
                        ps_name = NULL,
                        ps_equal_sign = '=',
                        ps_env = NULL,
                        ps_out_format = 'html'){
  # check type of pmat
  if (! is.vector(pvec))
    stop(" *** ERROR: specified pmat is not a vector")
  # Use bmatrix as output type
  return(rmd_vec_mat(pmat          = matrix(pvec, nrow = 1),
                     ps_name       = ps_name,
                     ps_equal_sign = ps_equal_sign,
                     ps_env        = ps_env,
                     ps_out_type   = 'bmatrix',
                     ps_out_format = ps_out_format))
}

