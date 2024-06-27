patient_donor_rhot <- function(patient_donor_data){
  patient_donor_data <- patient_donor_data |> 
    select(
      "Pair ID" = id,
      "Patient Name" = patient,
      "Donor Name" = donor,
      "Preferred Donor (Pair No.)" = preference
    )
  
  rhot <- patient_donor_data |> 
    rhandsontable(rowHeaders = FALSE) |> 
    hot_col(col = "Pair ID", width = 100)
  
  return(rhot)
}

matchings_roster <- function(ttcc_results, patient_donor_data){
  exchange_roster <- annotate_extracted_chains_with_names(ttcc_results, patient_donor_data)
  exchange_roster <- map_chr(exchange_roster, list(tags$li, as.character)) |> 
    str_c(collapse = "")
  return(exchange_roster)
}

