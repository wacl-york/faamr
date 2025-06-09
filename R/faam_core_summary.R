#' FAAM Core Summary 
#' 
#' Collects metadata from the faam core and core_1hz NCDF file for browsing of 
#' varaibles. \cr
#' Collates the variable name, long_name and units with the dimention name for 
#' selection of the sps in \code{read_core_famm()}
#' 
#' @param filepath path to file
#' 
#' @author W. S. Drysdale
#' 
#' @export
 
faam_core_summary = function(filepath){
  
  dat_meta = ncmeta::nc_meta(filepath)  
  
  dimentionInfo = dat_meta$dimension |> 
    dplyr::mutate(did = paste0("D",.data$id),
                  did = ifelse(.data$did == "D0", .data$did, paste0(.data$did, ",D0"))) |> 
    dplyr::select("grid" = "did", "dimentionName" = "name")
  
  attributeInfo = dat_meta$attribute |> 
    dplyr::filter(.data$name %in% c("long_name", "units")) |> 
    tidyr::unnest(.data$value) |> 
    dplyr::select(-"id") |> 
    tidyr::pivot_wider()
  
  dat_meta$grid |> 
    tidyr::unnest(.data$variables) |> 
    dplyr::select(-"ndims", -"nvars") |> 
    dplyr::left_join(dimentionInfo, "grid") |> 
    dplyr::left_join(attributeInfo, "variable")
  
}
