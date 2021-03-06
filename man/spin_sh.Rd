% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spin_sh.R
\name{spin_sh}
\alias{spin_sh}
\title{Spin a bash script into Rmd}
\usage{
spin_sh(
  ps_sh_hair,
  ps_out_rmd = paste0(fs::path_ext_remove(ps_sh_hair), ".Rmd")
)
}
\arguments{
\item{ps_sh_hair}{path to the bash scirpt}

\item{ps_out_rmd}{output Rmd file}
}
\description{
This function is derived from \code{knitr::spin()} for R-scripts and
is adapted to do a similar conversion of bash scripts into Rmd files.
The conversion is done in a simple way and almost no features of the
original are provided. This is just a very crude way to have a look
at documentation of a shell script in a nicer format such as html
or pdf.
}
\examples{
s_test_script <- system.file('extdata', 'test_script.sh', package = 'rmdhelp')
s_test_out <- file.path(tempdir(), 'test_script.Rmd')
spin_sh(ps_sh_hair = s_test_script, ps_out_rmd = s_test_out)
unlink(s_test_out)
In case the script starts with the path to bash remove it.
Insert the chunk borders
There are two types of chunks:

1. text chunks where text chunks are all lines that start with "#'"
2. code chunks are all lines that are not text chunks.

From the definitions given above it seams easier to start with text chunks
Start with the beginning of a code chunk
End of code chunks
## Replacements to get to a rmd document
The code chunks are augmented with the tiks and engine specs
The code chunk ends are symbolised with three tiks which are added at each beginning of the line
Remove all doxygen comment signs from text
}
