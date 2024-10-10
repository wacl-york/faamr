devtools::load_all()

if(!dir.exists(testthat::test_path('fixtures','read_expected','core'))){
  dir.create(testthat::test_path('fixtures','read_expected','core'), recursive = T)
}

if(!dir.exists(testthat::test_path('fixtures','read_expected','core_1hz'))){
  dir.create(testthat::test_path('fixtures','read_expected','core_1hz'), recursive = T)
}

if(!dir.exists(testthat::test_path('fixtures','read_expected','nitrates'))){
  dir.create(testthat::test_path('fixtures','read_expected','nitrates'), recursive = T)
}

# core --------------------------------------------------------------------

sps = rlang::set_names(c(2,4,32,64), \(x) paste0("sps", x))

for(vCore in c("core_faam_20190711_v004_r1_c179.td.nc","core_faam_20240417_v005_r0_c383.td.nc")){
  
  filePathCore = testthat::test_path('fixtures','read_inputs','core',vCore)
  fileNameCore = basename(filePathCore)
  fileOutRootCore = testthat::test_path('fixtures','read_expected','core')
  fileOutPathCore = file.path(fileOutRootCore, stringr::str_replace(fileNameCore, ".nc", ".RDS"))
  datList = purrr::map(sps, \(x) read_faam_core(filePathCore, sps = x))
  
  saveRDS(datList, fileOutPathCore)
  
}


# core 1hz ----------------------------------------------------------------

for(vCore1hz in c("core_faam_20190711_v004_r1_c179_1hz.td.nc", "core_faam_20240417_v005_r0_c383_1hz.td.nc")){
  
  filePathCore1hz = testthat::test_path('fixtures','read_inputs','core_1hz', vCore1hz)
  fileNameCore1hz = basename(filePathCore1hz)
  fileOutRootCore1hz = testthat::test_path('fixtures','read_expected','core_1hz')
  fileOutPathCore1hz = file.path(fileOutRootCore1hz, stringr::str_replace(fileNameCore1hz, ".nc", ".RDS"))
  
  datCore1hz = read_faam_core(filePathCore1hz)
  saveRDS(datCore1hz, fileOutPathCore1hz)
}


# nitrates ----------------------------------------------------------------

filePathNitrates = testthat::test_path('fixtures','read_inputs','nitrates','core-nitrates_faam_20190711_v001_r0_c179.td.nc')
fileOutRootNitrates = testthat::test_path('fixtures','read_expected','nitrates')
fileNameNitrates = basename(filePathNitrates)
fileOutPathNitrates = file.path(fileOutRootNitrates, stringr::str_replace(fileNameNitrates, ".nc", ".RDS"))
datNitrates = read_faam_nitrates(filePathNitrates)
saveRDS(datNitrates, fileOutPathNitrates)


