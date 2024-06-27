library('tidyverse')
library('matchingR')

n_pairs <- 3
n_men <- n_pairs
n_women <- n_pairs

men <- paste0("Man ", LETTERS[1:n_pairs])
women <- paste0("Woman ", LETTERS[1:n_pairs])

uM = matrix(c(1.0, 0.5, 0.0,
              0.5, 0.0, 0.5,
              0.0, 1.0, 1.0), 
            nrow = 3, 
            ncol = 3, 
            byrow = TRUE,
            dimnames = list(women, men))

uW = matrix(c(0.0, 1.0, 0.0,
              0.5, 0.0, 0.5,
              1.0, 0.5, 1.0), 
            nrow = 3, 
            ncol = 3, 
            byrow = TRUE,
            dimnames = list(men, women))

matches <- galeShapley.marriageMarket(uM, uW)

proposals <- matches$proposals

proposals <- matrix(proposals,
       dimnames = list(men, "Proposes to Woman"))

women[proposals]

result <- matrix(data = c(men, women[proposals]),
       nrow = 3, 
       ncol = 2,
       dimnames = list(c("Pair 1", "Pair 2", "Pair 3"), c("Man", "Woman")))

result
