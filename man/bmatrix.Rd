% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmd_vec_mat.R
\name{bmatrix}
\alias{bmatrix}
\title{Convert Matrix To bmatrix in Rmarkdown}
\usage{
bmatrix(
  pmat,
  ps_name = NULL,
  ps_equal_sign = "=",
  ps_env = NULL,
  ps_out_format = "html"
)
}
\arguments{
\item{pmat}{matrix to be converted to Rmarkdown}

\item{ps_name}{variable name for the matrix}

\item{ps_equal_sign}{equal sign to be used when a variable name is given}

\item{ps_env}{math environment in which matrix should be presented}

\item{ps_out_format}{output format produced}
}
\value{
String containing bmatrix representation of given matrix
}
\description{
The specified matrix is converted to a bmatrix in Rmarkdown
}
\details{
#
---- bmatrix Wrapper ---------------------------------------------------- #
}
\examples{
\dontrun{
eye3 <- diag(1, nrow = 3)
bmatrix(pmat = eye3, ps_name = 'I', ps_env = '$$')
}

}
