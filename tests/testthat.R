Sys.setenv('R_TESTS' = '')

library(abayes)
testthat::test_check('abayes')