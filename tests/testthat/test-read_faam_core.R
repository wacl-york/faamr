test_that("read core sps v4", {
  
  fpRead = testthat::test_path('fixtures','read_inputs','core','core_faam_20190711_v004_r1_c179.td.nc')
  fpExp = testthat::test_path('fixtures','read_expected','core','core_faam_20190711_v004_r1_c179.td.RDS')
  
  datExpList = readRDS(fpExp)
  
  for(sps in c(2,4,32, 64)){
    datRead = read_faam_core(fpRead, sps = sps)
    expect_equal(datRead, datExpList[[paste0("sps", sps)]])
  }
  
})

test_that("read core 1hz v4", {
  
  fpRead = testthat::test_path('fixtures','read_inputs','core_1hz','core_faam_20190711_v004_r1_c179_1hz.td.nc')
  fpExp = testthat::test_path('fixtures','read_expected','core_1hz','core_faam_20190711_v004_r1_c179_1hz.td.RDS')
  
  datExp = readRDS(fpExp)
  datRead = read_faam_core(fpRead)
  
  expect_equal(datRead, datExp)

  
})

test_that("read core sps v5", {
  
  fpRead = testthat::test_path('fixtures','read_inputs','core','core_faam_20240417_v005_r0_c383.td.nc')
  fpExp = testthat::test_path('fixtures','read_expected','core','core_faam_20240417_v005_r0_c383.td.RDS')
  
  datExpList = readRDS(fpExp)
  
  for(sps in c(2,4,32, 64)){
    datRead = read_faam_core(fpRead, sps = sps)
    expect_equal(datRead, datExpList[[paste0("sps", sps)]])
  }
  
})

test_that("read core 1hz v5", {
  
  fpRead = testthat::test_path('fixtures','read_inputs','core_1hz','core_faam_20190711_v004_r1_c179_1hz.td.nc')
  fpExp = testthat::test_path('fixtures','read_expected','core_1hz','core_faam_20190711_v004_r1_c179_1hz.td.RDS')
  
  datExp = readRDS(fpExp)
  datRead = read_faam_core(fpRead)
  
  expect_equal(datRead, datExp)
  
  
})

