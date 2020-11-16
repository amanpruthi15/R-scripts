###################################
## Aman Pruthi
## R as a Research Tool
## Homework 6 - Genetic algorithm functions III
## Due 10/13/20
###################################
## Instructions
## ------------

## This file contains two complete functions you've seen before:
## mateNumericVectors() and testFitness(). In addition, I've written a new
## function called simpleEvolve(), which creates a haploid population, creates
## a random optimal genotype, and then simulates evolution over a fixed number
## of generations. This new function depends upon two other functions. One
## simple one I've supplied (getRandChrom), but the other you will write
## yourself: getNextGeneration.


## Your tasks:

## 1. Write the getNextGeneration function (see comment header I supply). Hint:
## look up the "prob" argument to the sample() function to see how to choose
## among a list or vector according to weighted probabilities (fitnesses).

## 2. Comment all of the new code (e.g. everything except mateNumericVectors()
## and testFitness().

## 3. Write code at the end of the script to search through a reasonable number
## of values of mu and r and report which values result in the most rapid
## convergence of the population mean fitness to > 0.9. For each set of
## paramenters, you will extract the num.generations element from the result of
## simpleEvolve(). Report your findings. How do mu and r influence time to
## convergence? To explore a wide parameter space could be time consuming, you
## will have to decide how many combinations to test. I'd suggest looking at r
## values between 0 and 0.3 and mu values between 0 and 0.03.


###############################################################################
## simple-evolve.R:
## A simple simulation of haploid evolution.

## global constants:
ALLELES <- 1:10           # possible allele values
CHROM_LENGTH <- 10        # length of "chomosome" vectors
POP_SIZE <- 100           # population size
FITNESS_CRITERION <- 0.9  # default mean population fitness at which to end
# simulation


# returns a random chromosome vector with alleles sampled uniformly among the
# allowed alleles
getRandChrom <- function(lngth=CHROM_LENGTH){
  return(sample(ALLELES, lngth, replace=TRUE))
}
# It gives a vector(genotype) of length=10 as the valueof CHROM_LENGTH is 10 and
# returns a random chromosome vector with alleles sampled uniformly among the 
# allowed alleles.

# mateNumericVectors(x, y, r, mu=0):
# Returns a child vector created by "mating" two parent
# vectors x and y and supplying a per-adjacent-locus recombination rate and a per-locus
# mutation rate. During mutation, an allele is randomly assigned a value between 1 and
# 100, inclusive.
#
#   Args:
#    - x, y = Two vectors each representing a "genotype". The vectors should be
#             atomic vectors containing integer values between 1 and 100 inclusive.
#             These vectors are the two parent genotypes and must be of the same 
#             length.
#    - r    = The per-locus recombination rate. A rate between 0 and 1 which
#             indicates the probability of a crossover event between any two
#             adjacent genes (vector elements). Rates between 0 and 0.5 make the 
#             most sense biologically.
#   - mu      The per-locus mutation rate.
#
#  Returns:
#         - A vector of the same length as the parents
mateNumericVectors <- function(x, y, r, mu) {
  n <- length(x)
  stopifnot(n==length(y))
  
  crossovers <- runif(n) <= r
  useFirstParent  <- cumsum(crossovers) %% 2 == 0
  
  if(runif(1) < 0.5) useFirstParent <- !useFirstParent
  child <- ifelse(useFirstParent,x,y)
  
  mutations <- runif(n) <= mu
  child[mutations] <- sample(ALLELES, sum(mutations), replace=TRUE)
  
  return(child)
}

# testFitness(genotype, targetGenotype):
# Returns a scalar number representing how well the genotype (a vector of
# numbers) matches the target genotype. The matching is measured as the 
# recipricol sum of the squared deviations from the target (1 / (sse+1) ).
#
#     Args:
#         - genotype       = the chromosome to be tested.
#         - targetGenotype = The optimum genotype. These must be numeric
#                            vectors of the same length.
#  Returns:
#         - A scalar: the reciprocal of the sum of the squared deviations 
#           of the genotype from the target  (1 / (sse+1) ).
testFitness <- function(genotype, targetGenotype){
  stopifnot(length(genotype)==length(targetGenotype))
  return(1.0 / (sum((genotype-targetGenotype)^2) + 1.0 ) )
}

# getNextGeneration(population, r, mu, fitnesses):
#
# Accepts a list of genotypes ("population") and returns the new population
# replacing each individual with the result of mating two vectors chosen
# according to their fitnesses. Note that these are non-overlapping
# generations: the parents should be chosen from the existing generation and
# not from any newly selected offspring. The basic algorithm: 1) for each spot
# in the new population select two parents from the current population. These
# are chosen with weighted probability based on their fitnesses (see fitnesses
# argument and look at using the built in sample() function. 2) Use mateVectors
# to produce an offspring and add the offspring to the new population 4) When
# we have a new population of the same size as the original, return the new
# population.
#
#   Args:
#     - population     = a list of chromosomes (a list of numeric vectors)
#     - r              = per-adjacent-locus reocmbination rate
#     - mu             = a per locus mutation rate
#     - fitnesses = a vector of real numbers corresponding to the fitness
#                   values of each individual in the populations. This must
#                   be of the same length as population.
#
#  Returns:
#         - A list of vectors: the new population
getNextGeneration <- function(population, r, mu, fitnesses) {
  stopifnot(length(population) == length(fitnesses))
  # stop if the length of population is not equal to length of fitnesses
  
  nextGeneration <- vector("list", length(population))
  # creating an empty list of vectors in which the newGeneration will be created
  
  for (i in seq_along(population)) {
    
    mother <- sample(population, 1, prob = fitnesses, replace = TRUE)
    father <- sample(population, 1, prob = fitnesses, replace = TRUE)
    # Selecting two parents randomly from the population based on their fitness
    # value using prob argument in sample
    
    nextGeneration[[i]] <- mateNumericVectors(mother[[1]], father[[1]], r, mu)
  } # Each vector in the list will be an offspring made by mating two parents
    # using mateNumericVectors function. The parents are selected again and
    # again for each index and thus providing us with a nextGeneration list
    # having same the length to that of population
  
  return(nextGeneration)
}
################################################################################
# Add your own comment header here. Figure out what this function does.

# simpleEvolve <- function(generations, r, mu, break.fit, do.hist)

#  Args:
#     - generations    = number of generations to be covered
#     - r              = per-adjacent-locus recombination rate
#     - mu             = per locus mutation rate
#     - break.fit      = Maximum required fitness criteria  
#     - do.hist        = argument for plotting the histogram when TRUE
#
#  Returns:            = A list of vectors including the optimal.genotype, 
#                        mean.fit, num.generations 
simpleEvolve <- function(generations, r, mu, break.fit = FITNESS_CRITERION,
                         do.hist=FALSE) {
  optimum <- getRandChrom()
  # creating a random optimum chromosome with the help of getRandChrom function
  
  population <- replicate(POP_SIZE, getRandChrom(), simplify=FALSE)
  # creates a population of 100 different vectors randomly using replicate
  
  for(gen in 1:generations){
    allfits <- sapply(population, testFitness, targetGenotype=optimum)
    # gives a fitness value for each genotype in a generation using the 
    # testFitness function which takes targetGenotype in its arguments which we
    # have assigned as the optimum which was selected previously
    
    meanfit <- mean(allfits)
    # it gives the mean of the fitness of a generation
    #       print(paste("generation", gen, "mean fitness", meanfit))
    
    if(do.hist) {
      hist(sqrt(allfits),
           freq=FALSE, breaks=seq(0,1,0.02),col="gray",
           xlab="Square root of fitness")
    } # Starting a for loop to calculate the mean fitness of each new generation
      # This loop will repeat itself for the number of generations provided in
      # the argument until the meanfit is less than the FITNESS_CRITERION = 0.9
      # (break.fit). It will plot a histogram of square root of fitness for each
      # individual in the last generation only if do.hist = TRUE in arguments
    
    if ( meanfit > break.fit) break
    population <- getNextGeneration(population, r, mu, allfits)
  } # Stop the loop when meanfit is greater than FITNESS_CRITERION (break.fit)
    # Plot a histogram of square root of fitness for  each individual for which
    # the mean is greater than the FITNESS_CRITERION
  
  return(list("optimal.genotype" = optimum, "mean.fit"=meanfit,
              "num.generations"=gen))
}

### examples:
simpleEvolve(500, r=0.1, mu=0.0001, do.hist=TRUE)
simpleEvolve(500, r=0.1, mu=0.001, do.hist=TRUE)
simpleEvolve(500, r=0.3, mu=0.0001, do.hist= TRUE)
simpleEvolve(500, r=0.3, mu=0.001,  do.hist= TRUE)

## after running the examples when r=0.1, 0.3 and mu=0.001, the convergence time
## is less and reached in less than 500 generations. That is, the mean value
## crosses 0.9 in less number of generations

################################################################################
r <- c(0, 0.1, 0.2, 0.3)
mu <- c(0, 0.01, 0.02, 0.03)
d <- NULL
for (i in seq_along(r)) {
  for (i in seq_along(mu)) {
    convergenceRate <- simpleEvolve(500, r[i], mu[i] ,do.hist = FALSE)
    d <- rbind(d, data.frame(gen=convergenceRate$num.generations,
                 r=r[[i]], mu=mu[[i]]))
    }
}
d

## Results
#    gen   r   mu
# 1  500 0.0 0.00
# 2   88 0.1 0.01
# 3  110 0.2 0.02
# 4  500 0.3 0.03
# 5  500 0.0 0.00
# 6  126 0.1 0.01
# 7   48 0.2 0.02
# 8  500 0.3 0.03
# 9  500 0.0 0.00
# 10  91 0.1 0.01
# 11 155 0.2 0.02
# 12 500 0.3 0.03
# 13 500 0.0 0.00
# 14  79 0.1 0.01
# 15  68 0.2 0.02
# 16 500 0.3 0.03

## When r <- 0.2 and m <- 0.02 then mean fitness of the 51st generation is 
## more than the FITNESS_CRITERION. We can say that having moderate values of r
## and m will get population having a mean fitness greater than 0.9 and it is 
## largely dependent on mu, as whenever mu=0 or more than 0.02, we can not get 
## an optimal genotype with mean fitness > 0.9.