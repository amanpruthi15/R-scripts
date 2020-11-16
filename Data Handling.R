###################################
## Aman Pruthi
## R as a Research Tool
## Homework 2
## Due 9/8/20
###################################

oak_wp <- read.csv(
  "http://r-research-tool.schwilk.org/data/oak-water-potentials-simple.csv", 
  stringsAsFactors = FALSE)
head (oak_wp)

#####################################################################
# 1. How many different species are recorded in these data?

x <- oak_wp$spcode
unique(x)
# There are 5 different species namely: "QUEM", "QUGA", "QUGR2", 
# "QUGR3", "QUHY"

###################################################################### 
# 2. Mid day water potential should always be at least as negative as 
#    pre-dawn water potential. Are there any days and plants for which
#    mid-day water potential is higher than pre-dawn?

x <- oak_wp$md.psi
y <- oak_wp$pd.psi
z <- (x > y)
n_date <- which(z)
date <- oak_wp$date[n_date]
unique(date)
# The mid-day water potential was higher than pre-dawn water potential 
# on 5 days for 9 plants.

######################################################################
# 3. What is the lowest (most negative) mid-day water potential in 
#   this data set? When and for which species was this value recorded?

min(x, na.rm = TRUE)
# Lowest value of mid-day water potential is -6.75
y = which.min(x)
y
oak_wp$date[y]
oak_wp$spcode[y]
# Lowest mid-day water potential of -6.75 was recorded on 04/10/2011 
# for "QUGR3"

######################################################################
# 4. For which year was the average mid-day water potential lowest 
#    (most negative)

x <- oak_wp$year
y <- oak_wp$md.psi

a <- y[c(x==10)]
mean(a, na.rm = TRUE)
# Average mid-day water potential for 2010: -1.75

b <- y[c(x==11)]
mean(b, na.rm = TRUE)
# Average mid-day water potential for 2011: -3.03

c <- y[c(x==12)]
mean(c, na.rm = TRUE)
# Average mid-day water potential for 2012: -2.13

d <- y[c(x==13)]
mean(d, na.rm = TRUE)
# Average mid-day water potential for 2013: -1.70


years <- unique(oak_wp$year)
mean_for_year <- function(year){
  return(mean(oak_wp$md.psi[oak_wp$year==year], na.rm=TRUE))
}
wp_means <- sapply(years, mean_for_year)
years[which.min(wp_means)]

# 2011 had the lowest average mid-day water potential of -3.03

##################################################################