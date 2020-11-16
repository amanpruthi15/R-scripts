## R as a Research Tool
## HW 05: Genetic algorithm functions II
## Due September 29 2020

# Question 1 : explain the code in these functions:

## I have removed most of my comments so you can add in yours. You only need to
## add them to the functions, not the test code. But make sure you understand
## the tests.

# Function: randomChance
# Args:
#    - x   = a vector of probabilities (each value in range 0-1)
# Returns:
#    - A boolean vector of same length as x indicating successes

randomChance <- function(x) {
  return(runif(length(x)) <= x)
}
# This function generates by taking an argument vector and then returning 
# another vector of random deviates of a uniform distribution between0 and 1. 
# It returns a boolean vector: TRUE - if argument vector is greater than or 
# equal to its random deviates, i.e. the returned vector


# Function: mateVectors
# Args:
#    - x, y = Two vectors each representing a "genotype". The vectors should be
#             atomic vectors, but this is not enforced. These vectors are the
#             two parent genotypes and must be of the same length and type.
#    - r    = The per-locus recombination rate (scalar). A rate between 0 and 0.5
#             which indicates the probability of a crossover event between any 
#             two adjacent genes (vector elements).
# Returns
#        - vector of the same length and type as the parents.

mateVectors1 <- function(x, y, r) {
  stopifnot(length(x)==length(y)) # telling the function to stop if length of 
                                  # both the parents is unequal
  
  child <- y # making the child vector and then assigning it to parent y
             # such that we only worry about the locations where parent x's 
             # allele should come
  usingX <- randomChance(0.5) # this serves as a coin flip and will return a 
                              # boolean vector to decide which parent we are
                              # reading from
  
  for (i in seq_along(x)) {
    if(usingX) {
      child[i] <- x[i] # if we are reading from x then assign that allele of x 
                       # child[i] and if we are not then do not do anything as 
                       # it is already has y allele's
    }
    if(randomChance(r)) {
      usingX <- !usingX # flipping the coin to switch the parents, if it returns
                        # TRUE then keep reading from y and if it is FALSE then
                        # switch and start reading from x
    }
  }
  return(child)
}


# vectorized implementation of the above without explicit loops
mateVectors <- function(x, y, r) {
  stopifnot(length(x) == length(y)) # telling the function to stop if length of 
                                    # both the parents is unequal

  crossovers <- randomChance(rep(r, length(x))) 
  # this will give a boolean vector of length(x) and will be TRUE wherever
  # crossover occurs at each subsequent location
  
  useFirstParent  <- cumsum(crossovers) %% 2 == 0 
  # returns a boolean vector if cumulative sum of crossovers is divisible
  # by 2 or not. If it is then it will give TRUE and otherwise FALSE.
  
  if(randomChance(0.5)) {
    child <- ifelse(useFirstParent, x, y) # coin flip. If useFirstParent holds
  } else {                                # TRUE then use X's allele in child[i] 
    child <- ifelse(useFirstParent, y, x) # and if it is FALSE then use Y's 
  }                                       # allele in child[i]
  return(child)
}

## END region to which students must add comments ##


############################
##   Examples and Tests   ##
############################

# examples

mother <- rep(0,100)
father <- rep(1,100)
child1 <-mateVectors(mother,father, 0.05)
child2 <-mateVectors1(mother,father, 0.05)

child1
child2

## it works with vectors of single char strings.
alph <- unlist(strsplit("abcdefghijklmnopqrstuvwxyz",split=""))
mother <- alph #sample(alph,100,replace=TRUE)
father <- sample(alph,length(alph),replace=TRUE)
child1 <-mateVectors(mother,father, 0.05)
child2 <-mateVectors1(mother,father, 0.05)

## print the child vector as a string rather than a vector:
paste(child1,collapse="")
paste(child2,collapse="")

# Using vectors of words as the genotype:
words <- "It was the best of times, it was the worst
of times, it was the age of wisdom, it was the age of foolishness, it was the
epoch of belief, it was the epoch of incredulity, it was the season of Light,
it was the season of Darkness, it was the spring of hope, it was the winter of
despair, we had everything before us, we had nothing before us, we were all
going direct to heaven, we were all going direct the other way - in short, the
period was so far like the present period, that some of its noisiest
authorities insisted on its being received, for good or for evil, in the
superlative degree of comparison only."

words <- unlist(strsplit(words, split="[[:space:][:punct:]]+"))
mother <- words
father <- rep("____", length(words))
child1 <-mateVectors(mother,father, 0.05)
child2 <-mateVectors1(mother,father, 0.05)
paste(child1,collapse=" ")
paste(child2,collapse=" ")

##
mother <- unlist(strsplit("I am the mother", split=""))
father <- unlist(strsplit("i AM THE FATHER", split=""))
child <- mateVectors(mother,father, 0.05)
paste(child,collapse="")

# tests:

## test if distribution of run lengths looks like bernoulli process (geometric
## distribution). As length of vectors goes to infinity, this distribution of
## parental run lengths should approach a geometric distribution. So mean
## should be 1 / p and variance should be (1 - p) / p^2
set.seed(100)
N_ALLELES <- 1000
mother <- rep(0,N_ALLELES)
father <- rep(1,N_ALLELES)
child <- mateVectors(mother, father, 0.05)

runs <- rle(child)
runlengths <- runs$lengths

# when r is 0.05 then
# Mean should be 1 / r = 20
# Variance should be (1 - r) / r^2  = 380
mean(runlengths)
var(runlengths)
# Hm hard to tell for a single trial. Try this a bunch and check the mean?

runLengthStats <- function(v) {
  runs <- rle(v)
  return(c(mean = mean(runs$lengths), var = var(runs$lengths)))
}

children <- matrix(replicate(10000, mateVectors(mother, father, 0.05)), nrow=N_ALLELES)
rl_stats <- apply(children, MARGIN=2, runLengthStats)
rowMeans(rl_stats) # should be ~20 and ~380


children <- matrix(replicate(10000, mateVectors(mother, father, 0.1)), nrow=N_ALLELES)
# should be 50% 0s vs 1s so easy test:
rl_stats <- apply(children, MARGIN=2, runLengthStats)
rowMeans(rl_stats) # should be 10 and 90

###########################################################
# Question 2 : Add mutation to your mateVectors() function

# mateNumericVectors(x, y, r, mu=0):
#
# Returns a child vector created by "mating" two parent vectors x and y and
# supplying a per-adjacent-locus recombination rate and a per-locus mutation
# rate. During mutation, an allele is randomly assigned a value between 1 and
# 100, inclusive.
#
#   Args:
#    - x, y = Two vectors each representing a "genotype". The vectors should be
#             atomic vectors containing integer values between 1 and 100
#             inclusive. These vectors are the two parent genotypes and must be
#             of the same length.
#    - r =    The per-locus recombination rate. A rate between 0 and 0.5 which
#             indicates the probability of a crossover event between any two
#             adjacent genes (vector elements).
#   - mu      The per-locus mutation rate. A probability between 0 and 1.

x <- sample(1:100,10)
y <- sample(1:100,10)
min.allele <- 1   # defining the min and max outside the function so that I
max.allele <- 100 # do not have to use numbers inside the function

mateNumericVectors <- function(x, y, r, mu=0) {
  stopifnot(length(x)==length(y))
  
  child <- y
  usingX <- randomChance(0.5)
  
  for (i in seq_along(x)) {
    if(usingX) {
      child[i] <- x[i]
    }
    if(randomChance(r)) {
      usingX <- !usingX
    }
    if(randomChance(mu))  {
       child[i] <- sample(min.allele:max.allele,1)
    } 
    # if mu is greater than or equal to the random deviates than select one 
    # number at random from 1:100 and put that allele(number) in child[i]
  }
  return(child)
}

mateNumericVectors(x, y, 0.2, 0) # There should be no mutation with mu=0

##### x <- 33 43 46 94 24 40 51 76 66 97
##### y <- 86  1 37 72 20 55 57 66 11 82

# child <- 33 43 46 94 20 55 57 66 66 97 (There is no mutation)

mateNumericVectors(x, y, 0.2, 1) # Every allele should be mutated with mu=1

##### x <- 33 43 46 94 24 40 51 76 66 97
##### y <- 86  1 37 72 20 55 57 66 11 82

# child <- 26 88 64 46 72 54 96 13 21 33 (Each allele is mutated)

mateNumericVectors(x, y, 0.2, 0.3) # There should be some mutation with mu=0.3

##### x <- 33 43 46 94 24 40 51 76 66 97
##### y <- 86  1 37 72 20 55 57 66 11 82

# child <- 86  1 46 94 66 55 50 76 66 97

# there are mutations in 5th and 7th allele of the child

###########################################
# Question 3 : Defining "fitness"

# testFitness(genotype, targetGenotype):
#
# Returns a scalar number representing how well the genotype (a vector of
# numbers) matches the target genotype. The matching is measured as the
# reciprocal sum of the squared deviations from the target (1 / (sse+1) ).
# 
#     Args:
#         - genotype = the chromosome to be tested.
#         - targetGenotype = The optimum genotype. These must be numeric
#                            vectors of the same length.
#
#  Returns:
#         - A scalar: the reciprocal of the sum of the squared deviations of
#           the genotype from the target (1 / (sse+1) )

genotype <- c(1,0,4)
targetGenotype <- c(1,2,3)

testFitness <- function(genotype, targetGenotype) {
  stopifnot(length(genotype)==length(targetGenotype))
  
  sse <- sum((genotype - targetGenotype)^2) # simple mathematics function
  fitness <- (1/(sse+1))
  
  return(fitness)
}

testFitness(genotype, targetGenotype)
# [1] 0.1666667 - fitness