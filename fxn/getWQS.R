getWQS <- function(layer){
  
  wqs_final <- layer %>%
  as_tibble(.) %>%
  dplyr::left_join(.,
                   # remove duplicate/unnecessary columns:
                   dplyr::select(attains_assmnt_parms, -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
                   by = "assessmentunitidentifier", multiple = "all")

  return(wqs_final)

}
