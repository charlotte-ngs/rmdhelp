#' ---
#' title: Experiments With Matrices
#' date:  2020-05-24
#' ---
#' 
#' ## Data
tbl_breed_kb <- tibble::tibble(Variable = c("Mean Blood Serum BHB, mmol/L",
                                            "Standard deviation Blood Serum BHB, mmol/L",
                                            "Mean Acetone, mmol/L",
                                            "Standard deviation Acetone, mmol/L",
                                            "Intercept",
                                            "Regression Coefficient"),
                               Holstein = c(0.80,
                                            0.03,
                                            0.101,
                                            0.01,
                                            -2.38,
                                            1.22),
                               Jersey   = c(0.92,
                                            0.03, 
                                            0.159,
                                            0.01,
                                            1.47,
                                            0.85))
# Jersey
n_nr_point <- 10
vec_milk_ace_jer <- rnorm(n_nr_point, mean = tbl_breed_kb$Jersey[3], sd = tbl_breed_kb$Jersey[4])
vec_blood_bhb_jer <- tbl_breed_kb$Jersey[5] + 
  vec_milk_ace_jer * tbl_breed_kb$Jersey[6] + 
  rnorm(n_nr_point, mean = 0, sd = tbl_breed_kb$Jersey[2])
tbl_jer_reg <- tibble::tibble(`Milk Aceton` = vec_milk_ace_jer,
                              `Blood Serum BHB` = vec_blood_bhb_jer)

# Add genomic data
n_nr_snp <- 5
n_anz_snp_tiere <- 4
bBHB <- round(vec_blood_bhb_jer[1:n_anz_snp_tiere], digits = 3)
SNP1 <- c(-1,1,-1,1)
SNP2 <- c(0,1,0,1)
SNP3 <- c(1,0,-1,-1)
SNP4 <- c(1,1,1,1)
SNP5 <- c(1,0,-1,0)
farm <- c("1", "2","1","1")
tbl_herd_bhb_gen_data <- tibble::tibble(Tier = 1:n_anz_snp_tiere, 
                                        bBHB = bBHB,
                                        Herd = farm,
                                        SNP1 = SNP1,
                                        SNP2 = SNP2,
                                        SNP3 = SNP3,
                                        SNP4 = SNP4,
                                        SNP5 = SNP5)


vec_y_bbhb <- tbl_herd_bhb_gen_data$bBHB
vec_y_bbhb

ff <- bBHB ~ 0 + Herd
mf <- model.frame(ff, tbl_herd_bhb_gen_data)
mm <- model.matrix(ff, mf)
dimnames(mm) <- NULL
model_matrix_bhb <- mm[1:nrow(mm),1:ncol(mm)]
model_matrix_bhb

#' Convert R-matrix to a bmatrix 
#' 
#' According to https://bookdown.org/yihui/rmarkdown/markdown-syntax.html 
#' bmatrix is the environment that encloses the matrix into brackets
#' 
bmatrix <- function(pmat, ps_name = NULL, ps_env = NULL){
  sresult <- NULL
  sfooter <- NULL
  # put env start
  if (!is.null(ps_env)){
    if (substr(ps_env, 1, 1) == '$'){
      sresult <- ps_env
      sfooter <- ps_env
    } else {
      sresult <- paste0('\\begin{', ps_env, '} ', collapse = '')
      sfooter <- paste0('\\end{', ps_env, '} ', collapse = '')
    }
  }
  # name if one is specified
  if (!is.null(ps_name)){
    sresult <- paste0(sresult, ps_name, ' = ', collapse = '')
  }
  # put beginning of bmatrix
  sresult <- paste0(sresult, '\\begin{bmatrix} ')
  # put content of matrix
  ncol <- ncol(pmat)
  nrow <- nrow(pmat)
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
  sresult <- paste0(sresult, '\\end{bmatrix}')
  # put footer
  if (!is.null(sfooter)){
    sresult <- paste0(sresult, sfooter, collapse = '')
  }
  
  return(sresult)
}


#' ## Testing
cat(bmatrix(model_matrix_bhb, ps_name = 'X', ps_env = '$$'))

# y
cat(bmatrix(as.matrix(vec_y_bbhb, ncol = 1), ps_name = 'y', ps_env = '$$'))

# b
vec_b <- sapply(1:ncol(model_matrix_bhb), function(x) paste0('b_{', x, '}', collapse = ''), USE.NAMES = FALSE)
vec_b
cat(bmatrix(as.matrix(vec_b, ncol = 1), ps_name = 'b', ps_env = '$$'))

# W
mat_w <- as.matrix(tbl_herd_bhb_gen_data[,c("SNP1","SNP2","SNP3","SNP4","SNP5")])
mat_w
cat(bmatrix(mat_w, ps_name = 'W', ps_env = '$$'))

#q 
vec_q <- sapply(1:ncol(mat_w), function(x) paste0('q_{', x, '}', collapse = ''), USE.NAMES = FALSE)
cat(bmatrix(as.matrix(vec_q, ncol = 1), ps_name = 'q', ps_env = '$$'))

# e
vec_e <- sapply(1:length(vec_y_bbhb), function(x) paste0('e_{', x, '}', collapse = ''), USE.NAMES = FALSE)
cat(bmatrix(as.matrix(vec_e, ncol = 1), ps_name = 'e', ps_env = '$$'))

# g
vec_g <- sapply(1:length(vec_y_bbhb), function(x) paste0('g_{', x, '}', collapse = ''), USE.NAMES = FALSE)
cat(bmatrix(as.matrix(vec_g, ncol = 1), ps_name = 'g', ps_env = '$$'))

# Z
mat_Z <- diag(1,nrow = length(vec_y_bbhb))
cat(bmatrix(mat_Z, ps_name = 'Z', ps_env = '$$'))
