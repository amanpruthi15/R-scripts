# Aman Pruthi
# R as a research tool
# Homework 1
# Due 9/1/20
#######################

###################
##### Vectors #####
###################

x <- c(0,1,2,3,4,5,6,7,8,9,10)
x
# a - it concotanates the numbers 1-10 in a single line

x <- 0:10
x
# b - ":" to have a series of integers from 1 to 20

x <- seq(0,10,2)
x
# c - it will display integers from 0-10 but with a step of 2

x <- seq(10,0,-0.5)
x
# d - it will display numbers from 10-0 but subtracting 0.5 from the first number

x > 5
# e - if we define x as greater than 5 then it will display TRUE for the values greater than 5 and FALSE for the values smallers than 5

x[x>5]
# f - it will only dispay the numbers greater than five

x/2
# g - it will divide all the numbers previously defined as x by 2

x %% 2
# h - it will display the remainder from x/2 division

#########################
#### Create matrices ####
#########################

a <- matrix( c(1,-1,2,5,3,0,5,6,2), nrow=3, ncol=3)
a

b <- matrix (c(2,1,2,-3,0,3,5,6,1), nrow=3, ncol=3)
b

c <- matrix (c(5,4,3,1,0,-1,9,6,2,0,-2,4), nrow=3, ncol=4)
c

d <- matrix (c(3,4,0,-2,2,1,2,5,5,3,1,6), nrow=4, ncol=3)
d

#######################
##### Matrix math #####
#######################

a + b

#      [,1] [,2] [,3]
#[1,]    3    2   10
#[2,]    0    3   12
#[3,]    4    3    3

a - 2*b

#      [,1] [,2] [,3]
#[1,]   -3   11   -5
#[2,]   -3    3   -6
#[3,]   -2   -6    0

(a-2)*b

#      [,1] [,2] [,3]
#[1,]   -2   -9   15
#[2,]   -3    0   24
#[3,]    0   -6    0

a^2
# squaring each number in the matrix
#      [,1] [,2] [,3]
#[1,]    1   25   25
#[2,]    1    9   36
#[3,]    4    0    4

sqrt(a)

# Error
# Warning message: In sqrt(a) : NaNs produced
# "NaN" - Not a number, this happens when you try to take square root of a negative number

t(c)
# Converts the rows to columns
#      [,1] [,2] [,3]
#[1,]    5    4    3
#[2,]    1    0   -1
#[3,]    9    6    2
#[4,]    0   -2    4

c + d
# Error in c + d : non-conformable arrays
# It means that the number or rows and number of columns are not the same in two matrices

t(c) + d
# After transposing c, you can add c and d
#      [,1] [,2] [,3]
#[1,]    8    6    8
#[2,]    5    1    2
#[3,]    9    8    3
#[4,]   -2    3   10

c %*% d
# Matrix multiplication methods including different objects
#      [,1] [,2] [,3]
#[1,]   19   29   37
#[2,]   16   10   14
#[3,]   -3   29   38

a - 2 * diag(1,3,)
# Subtracts 2 from the diagonal values of matrix
#      [,1] [,2] [,3]
#[1,]   -1    5    5
#[2,]   -1    1    6
#[3,]    2    0    0

a - matrix(1,3,3)
# Subtracts 1 from all the matrix values
#      [,1] [,2] [,3]
#[1,]    0    4    4
#[2,]   -2    2    5
#[3,]    1   -1    1

########################
### More Matrix math ###
########################

a[1,]
# If you put "," after the number it displays that row of the matrix

b[,2]
# If you put "," before the number it displays that column of the matrix

rbind(a,b)
# It combines two matrices from row to row

cbind(a,b)
# It combines two matrices from column to column

a[c(1,2),] <- a[c(2,1),]
a
# It shifts row 1 and 2 and displays row 2 first and than row 1

###########################
### Order of Operations ###
###########################

#a) x = aa + bb * cc + dd / ee

#b) x = (aa + bb) * (cc + dd) / ee

#c) x = aa + (bb * cc) + (dd / ee)

#d) x = (aa + (bb * cc) + dd) / ee

# Lines (a) and (c) are equivalent according to BODMAS

#####################
##### Graphics ######
#####################

library(ggplot2)
head(diamonds)
ggplot(diamonds, aes(carat, cut, color=color)) + geom_boxplot()

# It is interesting that J color diamonds have the highest carat no matter what the cut is
# It is also intereting that the fair cut diamonds are highest in quality
       