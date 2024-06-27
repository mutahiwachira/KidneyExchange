

make_exchange_matrix <- function(patient_donor_df){
 # make a matrix with ncol = number of patients; nrow = maximum number of preferences in set
  
  num_pairs = nrow(patient_donor_df)
  
  max_num_preferences = patient_donor_df |> 
    pull(preference) |> 
    map_dbl(length) |> 
    max()
  
  preferences <- patient_donor_df$preference |> 
    map(function(x){
      x <- c(x, num_pairs + 1) # waiting list
      return(x)
    }) |> 
    map(magrittr::extract, seq_len(max_num_preferences)) |> 
    unlist()
  
  pref_mat <- matrix(preferences, ncol = num_pairs, nrow = max_num_preferences)
  
  # for(i in seq_len(num_pairs)){
  #   preference_of_patient_i <- preferences[[i]]
  #   for (j in seq_along(preference_of_patient_i)){
  #     pref_mat[i,j] <- preference_of_patient_i[j]
  #   }
  #   pref_mat[i,j+1] <- num_pairs + 1
  # }
  
  return(pref_mat)
}

get_name_from_ind <- function(patient_donor_df, id, name_of = c("patient", "donor")){
  match.arg(name_of, c("patient", "donor"))
  
  name_of_person <- patient_donor_df |> 
    filter(id == {{id}}) |> 
    pull({{name_of}})
  
  return(name_of_person)
}

annotate_exchange_matrix_with_donor_names <- function(patient_donor_df, exchange_matrix){
  donor_names <- patient_donor_df |> 
    pull(donor)
  
  for (i in seq_along(exchange_matrix)){
    if(is.na(exchange_matrix[i])) {
      next
    } else {
      exchange_matrix[i] = get_name_from_id(patient_donor_df, id == exchange_matrix[i], name_of = "donor")
    }
  }
  
  return(exchange_matrix)
}

extract_chains <- function(kidney_chain){
  chains <- kidney_chain$matching
  num_pairs <- nrow(kidney_chain$matching) + length(kidney_chain$waiting_list)
  chain_for_i = vector("list", num_pairs)
  for (i in chains$ind){
    j = chains$obj[chains$ind == i]
    while(j != i){
      if(!j %in% chains$ind) {
        chain_for_i[[i]] <- append(chain_for_i[[i]], -1)  
        break
      }
      chain_for_i[[i]] <- append(chain_for_i[[i]], j)
      j = chains$obj[chains$ind == j]
    }
  }
  
  chain_for_i <- chain_for_i |> 
    map(function(x){
      if(is.null(x)) x <- -1
      return(x)
    })
  
  return(chain_for_i)
}

annotate_extracted_chains_with_names <- function(kidney_chain, patient_donor_df){
 matchings <- kidney_chain$matching
 matchings_copy <- matchings

 listed_in_order <- c()
 while(nrow(matchings) > 0) {
   start_j = matchings[1, "ind"]
   next_j = matchings[1, "obj"]
   this_chain = c(start_j)
   while(start_j != next_j){
     this_chain <- append(this_chain, next_j)
     next_j <- matchings[matchings$ind == next_j, "obj"]
     if(length(next_j) == 0) break
   }
   matchings <- matchings[!matchings$ind %in% this_chain,]  
   listed_in_order <- c(listed_in_order, this_chain)
 }
 
 listed_in_order <- listed_in_order[!listed_in_order %in% kidney_chain$waiting_list]
 
 matched <- patient_donor_df[match(listed_in_order, patient_donor_df$id),]
 wait_listed <- patient_donor_df[patient_donor_df$id %in% kidney_chain$waiting_list,]
 matchings <- matchings_copy[match(listed_in_order, matchings_copy$ind),]
 rm(matchings_copy)

 patient <- get_name_from_id(patient_donor_df, matchings$ind,name_of = "patient")
 donor <- get_name_from_id(patient_donor_df, matchings$obj,name_of = "donor")
 donors_patient <- get_name_from_id(patient_donor_df, matchings$obj,name_of = "patient")
 who_gets_from_whom <- str_glue("{patient} ({matchings$ind}) gets a kidney from {donor} ({matchings$obj}), who joined with {donors_patient}.") |> 
   as.character()
 wait_listed_patients <- wait_listed$patient
 if(length(wait_listed_patients) == 0){
   who_was_wait_listed <- str_glue("All patients found a match and none were placed on the waitlist.")
 } else if (length(wait_listed_patients) == 1) {
   who_was_wait_listed <- str_glue("While {wait_listed_patients} was placed on a priority waiting list.")
 } else {
   wait_listed_patients <- str_c(head(wait_listed_patients, length(wait_listed_patients) - 1),
                              " and ",
                              tail(wait_listed_patients, 1),
                              collapse = ", ")
   
   who_was_wait_listed <- str_glue("While {wait_listed_patients} were placed on a priority waiting list.")
 }
 
 who_gets_from_whom <- c(who_gets_from_whom, who_was_wait_listed)
 # making the groups - all the pairs in one cycle
 # TODO
 
 return(who_gets_from_whom)
}

run_ttcc <- function(exchange_matrix){
  seed = runif(1, 100, 999)
  n = ncol(exchange_matrix)
  
  res <- purrr::safely(ttcc)(prefs = exchange_matrix, priority = seq_len(n), seed = seed)
  if(is.null(res$error)){
    kidney_chain <- res$result
    return(kidney_chain)
  } else {
    return(list(error = "No matching was found"))
  }
}


