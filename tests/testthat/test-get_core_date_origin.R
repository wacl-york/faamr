test_that("get origin v4", {
  expect_equal(
    get_core_date_origin(testthat::test_path('fixtures','read_inputs','core','core_faam_20190711_v004_r1_c179.td.nc')),
    nanotime::nanotime("2019-07-11T00:00:00+00:00")
  )
})

test_that("get origin v5", {
  expect_equal(
    get_core_date_origin(testthat::test_path('fixtures','read_inputs','core','core_faam_20240417_v005_r0_c383.td.nc')),
    nanotime::nanotime("2024-04-17T00:00:00+00:00")
  )
})

test_that("get origin 1hz v4", {
  expect_equal(
    get_core_date_origin(testthat::test_path('fixtures','read_inputs','core_1hz','core_faam_20190711_v004_r1_c179_1hz.td.nc')),
    nanotime::nanotime("2019-07-11T00:00:00+00:00")
  )
})

test_that("get origin 1hz v5", {
  expect_equal(
    get_core_date_origin(testthat::test_path('fixtures','read_inputs','core_1hz','core_faam_20240417_v005_r0_c383_1hz.td.nc')),
    nanotime::nanotime("2024-04-17T00:00:00+00:00")
  )
})

