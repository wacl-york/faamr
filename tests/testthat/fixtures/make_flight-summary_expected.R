devtools::load_all()

if(!dir.exists(testthat::test_path('fixtures','read_expected','flight-summary'))){
  dir.create(testthat::test_path('fixtures','read_expected','flight-summary'), recursive = T)
}

fs = read_flight_summary(testthat::test_path('fixtures','read_inputs','flight-summary','flight-sum_faam_20240417_r0_c383.txt'))
saveRDS(fs, testthat::test_path('fixtures','read_expected','flight-summary','flight-sum_faam_20240417_r0_c383.RDS'))
