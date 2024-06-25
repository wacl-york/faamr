#' FAAM File Lookup
#' 
#' Table of file name identifiers, and if it exists the name of the corresponding faamr function to use to read it.
#' 
#' @export

faam_file_lookup = function(){
  
  tibble::tribble(
    ~fileType, ~fileRegex, ~readFunction,
    "core.nc",                     "core_faam(.?.{22}).nc",                        "read_faam_core",
    "core_1hz.nc",                 "core_faam(.*?)1hz.nc",                         "read_faam_core",
    "corerawdlu.zip",              "core_faam(.*?)rawdlu.zip",                     NA,
    "core-cloud-phy.nc",           "core-cloud-phy_faam(.?.{22}).nc",              NA,
    "core-cloud-phy_cip100.nc",    "core-cloud-phy_faam(.*?)cip100.nc",            NA, 
    "core-cloud-phy_cip15.nc",     "core-cloud-phy_faam(.*?)cip15.nc" ,            NA,
    "core-cloud-phy_rawpads.zip",  "core-cloud-phy_faam(.*?)rawpads.zip",          NA,
    "core-cloud-phy_aimms.nc",     "core-cloud-phy_faam(.*?)aimms.nc",             NA,
    "core-cloud-phy_pcasp.nc",     "core-cloud-phy_faam(.*?)pcasp([1-2]).nc",      NA,
    "core-cloud-phypcasp_cal.nc",  "core-cloud-phy_faam(.*?)pcasp-([1-2])_cal.nc", NA,
    "core-cloud-phy_cdp-1_cal.nc", "core-cloud-phy_faam(.*?)cdp-1_cal.nc",         NA,
    "core-cloud-phy_raw.zip",      "core-cloud-phy_faam(.*?)raw.zip",              NA,
    "core-nitrates.nc",            "core-nitrates_faam",                           "read_faam_nitrates_data",
    "core-thermistor_.nc",         "core-thermistor_faam(.*?).nc",                 NA, 
    "faam-ccnrack.nc",             "faam-ccnrack_faam_(.*?).nc",                   NA,
    "faam-fgga.na",                "faam-fgga_faam(.*?).na",                       "read_nasa_ames",
    "faam-wcm2000.nc",             "faam-wcm2000_faam(.*?).nc",                    NA,
    "faam-wcm2000_raw.zip",        "faam-wcm2000_faam(.*?)raw.zip",                NA,
    "faam-dropsonde_raw.nc",       "faam-dropsonde_faam(.*?)raw.nc",               NA,
    "faam-dropsonde(.*?)proc.nc",  "faam-dropsonde_faam(.*?)proc.nc",              NA,
    "flight-sum.csv",              "flight-sum_faam(.*?).csv",                     NA,
    "flight-cst.yaml",             "flight-cst_faam(.*?).yaml",                    NA, 
    "flight-cst.txt",              "flight-cst_faam(.*?).txt",                     NA,
    "flight-sum.txt",              "flight-sum_faam(.*?).txt",                     NA,
    "flight-sum.html",             "flight-sum_faam(.*?).html",                    NA,
    "flight-sum.kml",              "flight-sum_faam(.*?).kml",                     NA,
    "flight-sum.json",             "flight-sum_faam(.*?)json",                     NA,
    "flight-track.kml",            "flight-track_faam(.*?).kml",                   NA,
    "asmm_fm1.xml",                "asmm_faam(.*?)fm1.xml",                        NA,
    "instrument-report.json",      "instrument-report_faam(.*?).json",             NA,
    "instrument-report.txt",       "instrument-report_faam(.*?).txt",              NA,
    "man-ams.na",                  "man-ams_faam(.*?).na",                         "read_nasa_ames",
    "man-sp2.na",                  "man-sp2_faam(.*?).na",                         "read_nasa_ames",
    "man-tildas.na",               "man-tildas_faam(.*?).na",                      "read_nasa_ames",
    "man-cims.na",                 "man-cims_faam(.*?).na",                        NA,
    "man-filters.na",              "man-filters_faam(.*?).na",                     NA,
    "man-smps.na",                 "man-smps_faam(.*?).na",                        NA,
    "metoffice-aimms.nc",          "metoffice-aimms_faam(.*?).nc",                 NA,
    "york-tildas.na",              "york-tildas_faam(.*?).na",                     "read_nasa_ames",
    "york-aqdnox.na",              "york-aqdnox_faam(.*?).na",                     "read_yorknox_data",
    "york-gc-fid-lab.na",          "york-gc-fid-lab_faam(.*?).na",                 NA,
    "00README",                    "00README",                                     NA
  )
}


