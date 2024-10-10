test_that("read nitrates v5", {
  
  "tests/testthat/fixtures/read_expected/nitrates/core_faam_20240417_v005_r0_c383_1hz.td.RDS"
  
  fpRead = testthat::test_path('fixtures','read_inputs','nitrates','core-nitrates_faam_20190711_v001_r0_c179.td.nc')
  fpExp = testthat::test_path('fixtures','read_expected','nitrates','core-nitrates_faam_20190711_v001_r0_c179.td.RDS')
  
  datExp = readRDS(fpExp)
  datRead = read_faam_nitrates(fpRead)
  
  expect_equal(datRead, datExp)
  
  
})
