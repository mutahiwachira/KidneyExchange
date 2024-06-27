match_couples <- function(men_preferences, women_preferences){
  men_preferences <- sortIndex(men_preferences)
  women_preferences <- sortIndex(women_preferences)
  matches <- galeShapley.marriageMarket(proposerPref = men_preferences, reviewerPref = women_preferences)
  
  men <- colnames(men_preferences)
  women <- colnames(women_preferences)
  
  proposals <- matches$proposals
  
  proposals <- matrix(proposals,
                      dimnames = list(men, "Proposes to Woman"))
  
  women[proposals]
  
  result <- matrix(data = c(men, women[proposals]),
                   nrow = 3, 
                   ncol = 2,
                   dimnames = list(c("Pair 1", "Pair 2", "Pair 3"), c("Man", "Woman")))
  
  result 
}
