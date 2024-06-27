

empty_patient_donor_df <- function(){
  
  patient_donor_df <- tibble(
    id = integer(),
    patient = character(),
    donor = character(),
    preference = list()
  )
  
  return(patient_donor_df)
  
}

instantiate_patient_donor_df <- function(num_pairs = 20, min_num_preferences = 1, max_num_preferences = floor(num_pairs / 2), seed = NULL){
  set.seed(seed)
  people_names <- read_lines("data/people/names.txt")
  num_pairs <- min(2*num_pairs, length(people_names))
  people_names <- sample(people_names, num_pairs, replace = FALSE)
  
  is_patient <- sample(seq_along(people_names),size = length(people_names)/2)
  patients <- people_names[is_patient]
  donors <- people_names[-is_patient]
  ids = seq_along(patients)
  
  prefs = list()
  min_num_preferences = max(1, min_num_preferences)
  max_num_preferences = min(max_num_preferences, num_pairs - 1)
  for (i in ids) {
    rand_size = round(runif(1, min = min_num_preferences, max_num_preferences))
    prefs[i] = list(sample(ids[-i], size = rand_size))
  }
  
  patient_donor_df <- empty_patient_donor_df() |> 
    add_patient_donor_pairs(patients, donors, prefs)
  
  return(patient_donor_df)
}

add_patient_donor_pairs <- function(patient_donor_df, patient_names, donor_names, preferences = NA){
  # TODO: Vectorize
  if(nrow(patient_donor_df) == 0){
    ids <- seq_along(patient_names)
  } else {
    start <- nrow(patient_donor_df) + 1
    ids <- seq(from = start, end = start + len(patient_names),by = 1)
  }
    
  df <- tibble(
    id = ids,
    patient = patient_names,
    donor = donor_names,
    preference = preferences
  )
  
  patient_donor_df <- patient_donor_df |> 
    bind_rows(df)
  
  return(patient_donor_df)
}

update_patient_preference <- function(patient_donor_df, patient_id, preferred_donor_id){
  patient_donor_df <- patient_donor_df |> 
    mutate(preference = if_else(id == patient_id, preferred_donor_id, preference))
  
  return(patient_donor_df)
}

get_name_from_id <- function(patient_donor_df, id, name_of = c("patient", "donor")){
  match.arg(name_of, c("patient", "donor"))
  
  name_of_person <- patient_donor_df |> 
    arrange(id) |> 
    slice({{id}}) |> 
    pull({{name_of}})
  
  return(name_of_person)
}
