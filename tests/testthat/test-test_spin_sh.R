context("Spinning bash scripts")
library(rmdhelp)

test_that("Convert Bash Script", {
  # test bash script to be converted to rmd
  s_test_sh <- system.file('extdata', 'test_script.sh', package = 'rmdhelp')
  # verified output from previous run of script conversioin
  vec_out_rmd <- readLines(con = file(system.file('extdata', 'test_script.Rmd', package = 'rmdhelp')))
  # convert s_test_sh to rmd
  s_tmp_rmd <- file.path(tempdir(), 'test_script.Rmd')
  rmdhelp::spin_sh(ps_sh_hair = s_test_sh, ps_out_rmd = s_tmp_rmd)
  vec_tmp_rmd <- readLines(con = file(s_tmp_rmd))
  # remove temp rmd file
  unlink(s_tmp_rmd)
  # compare
  expect_equal(vec_tmp_rmd, vec_out_rmd)
})
