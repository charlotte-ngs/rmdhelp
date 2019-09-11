stext <- "#' ---
#' title:   Testing knitr::spin
#' date:    2019-09-11
#' ---
#' 
#' ## Disclaimer
#' This is a test for knitr::spin. We are using some R-code to test
#+ data-gen
nobs <- 10
btrue <- c(-3.12, 0.09)
x <- c(1:nobs)
y <- btrue[1] + btrue[2] * x + rnorm(nobs)

#' The data can be used for plots."
