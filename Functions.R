###################################
## Aman Pruthi
## R as a Research Tool
## Homework 3
## Due 9/15/20
###################################


## euclideanDist: Returns the euclidean distance between two points, 
##                each specified by x and y coordinates.

##     Args:
##       p1 = 2-element vector [x1,y1] containing the coordinates of the 
##                        first point.
##       p2 = 2-element vector [x2,y2] containing the coordinates of the 
##                        second point.
##     Returns a scalar (in R, this is a vector of length 1):
##       distance between points.

euclideanDist <- function(p1, p2) {
  return(sqrt(sum((p1 - p2) ^ 2)))
}

p1 <- c(2,8)
p2 <- c(4,12)
euclideanDist(p1, p2)

# The distance between the 2 points p1 and p2 is 4.47 

################################################################

## lineParams: Given the (x,y) coordinates of two points, finds the
##              slope and intercept of the line connecting the points.

##     Args:
##       point1 = 2-element vector containing x,y coordinates for point 1.
##       point2 = 2-element vector containing x,y coordinates for point 2.
##     Returns a list containing:
##       slope = slope of the resulting line.
##       intercept = Y-intercept of the resulting line.

lineParams <- function(p1, p2) {
  slope <- ((p2[2] - p1[2])/(p2[1] - p1[1]))
  intercept <- (p1[2] - (slope * p1[1]))
  return(list(slope=slope, intercept=intercept))
}

p1 <- c(2,8)
p2 <- c(4,12)

lineParams(p1, p2)

# The slope of the line connecting the two points p1 and p2 is 2
# The y-intercept is 4

################################################################

#### decomposeTime to decompose a simple integer ####

# Number of seconds in 1 day = 60*60*24 = 86400
# Number of seconds in 1 hours = 60*60 = 3600
# Number of seconds in 1 minute = 60

decomposeTime <- function(seconds) {
  if (seconds >= 0) {
  days <- (seconds %/% 86400)
  hours <- ((seconds %% 86400) %/% 3600)
  minutes <- ((seconds %% 86400) %% 3600 %/% 60)
  residualSeconds <- ((seconds %% 86400) %% 3600 %% 60)
  return(list(days=days, hours=hours, minutes=minutes, 
              residualSeconds=residualSeconds))}
  else {return("Time cannot be negative")} 
}

decomposeTime(1000000)
decomposeTime(-1)

# 1000000 seconds = 11 days + 13 hours + 46 minutes + 40 seconds

################################################################