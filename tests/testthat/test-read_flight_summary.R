test_that("multiplication works", {
  
  fpRead = testthat::test_path('fixtures','read_inputs','flight-summary','flight-sum_faam_20240417_r0_c383.txt')
  fpExp = testthat::test_path('fixtures','read_expected','flight-summary','flight-sum_faam_20240417_r0_c383.RDS')
  
  datExp = readRDS(fpExp)
  datRead = read_flight_summary(fpRead)
  
  expect_equal(datRead, datExp)
  
})
