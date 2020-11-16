###################################
## Aman Pruthi
## R as a Research Tool
## Homework 4
## Due 9/22/20
###################################


# test_probability(prob):
# Returns a boolean vector for the probability
#
#    Args:
#          prob   = the probability of selecting a random parent
# 
#  Returns:
#           a boolean vector stating TRUE or FALSE if the condition is 
#           met or not


test_probability <- function(prob) {
  return(runif(length(prob)) < prob)
}


# mateVectors(x, y, r):
# Returns a child vector created by "mating" two parent
# vectors x and y and supplying a per-adjacent-locus recombination rate.
#
#     Args:
#         - x, y    = the two parent vectors.  These must be of same
#                     type (numeric, character, or logical) and same
#                     length.
#         - r       = the per-locus recombination rate
#
#  Returns:
#         - A vector of the same length as the parents


mateVectors <- function(x, y, r) {
  
  stopifnot(length(x)==length(y))
  
  offspring <- x
  readingY <- test_probability(0.5)
  
  for (i in seq_along(y)) {
    if (readingY) {
      offspring[i] <- y[i]
    }
    if (test_probability(r)) {
      readingY <- !readingY
    }
  }
  return(offspring)
}

x <- rep(0,10)
y <- rep(1,10)
r <- 0.2

mateVectors(x, y, r)

## One of the possible offspring output for r <- 0.2 is:
## [1] 0 1 1 1 1 0 0 0 1 1

## Testing other examples of the same function using vectors 
## and characters

mother <- rep(0,50)
father <- rep(1,50)
mateVectors(mother, father, 0.02)

## [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##[34] 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

mother <- unlist(strsplit("I am the mother", split=""))
father <- unlist(strsplit("i AM THE FATHER", split=""))
child <- mateVectors(mother,father, 0.05)
paste(child,collapse="")

## [1] "i AM THe mother"

######################################################################

## test if distribution of run lengths looks like a bernoulli process
## (geometric distribution). As length -> infinity this should be true.

# Mean should be 1 / p
# Variance should be (1 - p) / p^2

mother <- rep(0,10000)
father <- rep(1,10000)
child <- mateVectors(mother, father, 0.05)

runs <- rle(child)
runlengths <- runs$lengths
mean(runlengths) 
# [1] 20.96436
var(runlengths)
# [1] 376.8198
