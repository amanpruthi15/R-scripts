####################################
## Aman Pruthi
## R as a Research Tool
## Homework 10
## Due 11/10/20
###################################

## This script downloads a large zipfilefrom the social security
## administration, extracts a list of filenames in that file (eg
## "yob1901.txt"), extracts each of these data files fromt he zipfile, and runs
## calculations on name popularity for each year. Then the script stacks one
## huge data frame of baby names with percent by year and rank by year, then
## subsets the top 1000 for any year and saves the data as a csv file.

## data are available from: http://www.ssa.gov/oact/babynames/limits.html

library(stringr)
library(dplyr)
library(ggplot2)

# Return the year (eg 1900) from the filename including a possible path (eg
# ./names/yob1900.txt)
yearFromFilename <- function(s) {
  temp <- str_match(s,".+([0-9]{4})\\.[a-zA-Z]+$")
  return(temp[,2])
}

## function to read in one file (eg yob1900.txt) and calculate ranks and
## percentages by sex then return a data frame
readNameFile <- function(file.name, zipfile) {
  print(paste("Reading ", file.name))
  bd <- read.csv(unz(zipfile, file.name), header=FALSE, stringsAsFactors=FALSE)
  names(bd) <- c("name", "sex", "num")
  bd$year <- as.integer(yearFromFilename(file.name))
  ## data comes in form: all girls, then all boys. Within each group, names are
  ## in descending order of popularity, so we don't even need to calculate,
  ## rank, we can just assign it. This takes more lines of code, but is faster
  ## than running another ddply step.
  
  # so let's figure out ranks and proportions separately by sex
  girls <- subset(bd, sex=="F")
  boys  <- subset(bd, sex=="M")
  
  # Total num of boys and total num girls for this year
  gsum <- sum(girls$num)
  bsum <- sum(boys$num)
  
  ## assign ranks because data is already sorted in decreasing popularity
  girls$rank <- 1:length(girls$name)
  boys$rank <- 1:length(boys$name)
  
  girls$percent <- 100 * girls$num / gsum
  boys$percent <- 100 * boys$num /bsum
  return(rbind(girls,boys))
}


### main script:
################

## download zipfile as a temporary file. It is over 7 MB so can take a few
## seconds.
temp <- tempfile()
download.file("https://www.ssa.gov/oact/babynames/names.zip",temp)

# get list of all data files in the zipfile
year.files = unzip(temp, list=TRUE) # a file for each year in data set
year.files <- year.files[["Name"]]
# exclude other files such as pdf metadata
year.files <- grep("txt$", year.files, value=TRUE)

## obtain list of data frames:
year.data <- lapply(year.files, readNameFile, zipfile = temp)

# Now get our big data frame:
baby.names <- dplyr::bind_rows(year.data)

## we are done with the zipfile, so delete it:
# unlink(temp)

## ok, some of the later years are HUGE (2013 has 33072 names!) to allow
## further analyses in reasonable time, we will restrict to top 1000 names for
## each sex in each year
baby.names <- subset(baby.names, rank <= 1000)
baby.names$num <- NULL  # we don't need the raw counts anymore, reduce file
# size


## Question 1 

# Looking for names that sound similar to my name
# Using a function called "soundex" developed by Dr. Schwilk

# Function soundex helps find names with similar sounds by grouping different
# sounds together and then giving them a specific code. This way we can group 
# together all the names with a single specific code which is based on their
# sound.

soundex <- function(name) {
  name <- toupper(name)
  name <- unlist(strsplit(name,"")) # split into individual chars
  sdx <- name[1]
  ## this is specifically for english names 
  ## sound for the name as shorter code
  dict <- c("BFPV", "CGJKQSXZ", "DT", "L", "MN", "R", "AEIOUHWY")
  codes <- c("1","2","3","4","5","6",".")
  for (i in 2:length(name)) {
    code = codes[grep(name[i], dict, fixed=TRUE)]
    if (code != str_sub(sdx,-1,-1)) {
      sdx <- str_c(sdx, code)
    }
  }
  sdx <- gsub(".", "", sdx, fixed=TRUE)
  sdx <- str_pad(sdx, 3, pad = "0")
  return(sdx)
}

unique_names <- unique(baby.names$name)
soundex_unique_names <- sapply(unique_names, soundex)
baby.names$soundex <- soundex_unique_names[match(baby.names$name, unique_names)]

aman <- subset(baby.names, soundex==soundex("Aman"))
aman
# Unique code for my name according to the function is "A55"
unique(aman$name)
# [1] "Ammon"   "Amon"    "Anona"   "Annamae" "Amin"    "Amani"   "Amina"   "Aminah"

aman_trend <- group_by(aman, sex, year)
aman.sum <- summarize(aman_trend, percent = sum(percent))

axes <- list(
  scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)),
  scale_y_continuous("percent")
)

ggplot(aman.sum, aes(year, percent, color = sex)) + geom_line() + axes

# In the 1800s only boys were named Aman or something similar to that bit they 
# very few in number.
# Starting of 1900s, there were girls being named Aman or similar sounding names.
# No boys were being named Aman after 1980
# There was a big jump in girls being named Aman or similar sounding names
# starting from 2000 and reached its peak around 2017-18
# I expected this trend because when we consider other languages, there are more 
# girl names sounding similar to Aman as compared to boys.
# I have met a lot of girls named Aman or Amin than boys.

## Checking the trend for names featured in the popular TV show "Friends" which
## started in 1994 and ended in 2004.

chandler <- subset(baby.names, soundex==soundex("Chandler"))
chandler_trend <- group_by(chandler, sex, year)
chandler.sum <- summarize(chandler_trend, percent = sum(percent))
ggplot(chandler.sum, aes(year, percent, color = sex)) + geom_line()

## As expected, there is an increase in number of boys named "Chandler" during 
## the timeline of the show.

monica <- subset(baby.names, soundex==soundex("Monica"))
monica_trend <- group_by(monica, sex, year)
monica.sum <- summarize(monica_trend, percent = sum(percent))
ggplot(monica.sum, aes(year, percent, color = sex)) + geom_line()

## Similarly, there is an increase in number of girls named "Monica" during the
## timeline of the show.

###############################################################################

## Question 2

oldt <- scan("http://r-research-tool.schwilk.org/assignments/old-testament.txt",
             "character")

## Checking the trend of old testament names in US 

oldt_names <- subset(baby.names, baby.names$name %in% oldt)
oldt_names_trend <- group_by(oldt_names, year, sex)
oldt_names_sum <- summarize(oldt_names_trend, percent = sum(percent))
ggplot(oldt_names_sum, aes(year, percent, color = sex)) + geom_line() + axes

# The old testament names trend for the females has been nearly consistent with 
# drops around 1940s and 1970 but it declined rapidly after 2000s. It never 
# exceeded more than 6.5%.

# But for the males, the trend of biblical old testament names has been
# increasing starting from 1930s and reached saturation around 1980s-2000 which
# was around 20%. It again started decreasing after 2000s.

## Finding out top 20 old testament names in the whole data set

top20 <- group_by(oldt_names, name)
top20_mean <- summarize(top20, percent = mean(percent))
top20_mean <- top20_mean[order(top20_mean$percent, decreasing= TRUE),]
head(top20_mean, 20) 

# Top 20 names according to the mean percent are:

# A tibble: 20 x 2
#    name      percent
#    <chr>       <dbl>
#  1 Michael     1.29 
#  2 David       1.12 
#  3 Joseph      1.10 
#  4 Daniel      0.776
#  5 Elizabeth   0.760
#  6 Sarah       0.565
#  7 Ruth        0.531
#  8 Samuel      0.482
#  9 Joshua      0.434
# 10 Jacob       0.410
# 11 Benjamin    0.393
# 12 Zachary     0.378
# 13 Rebecca     0.351
# 14 Jonathan    0.311
# 15 Ethan       0.300
# 16 Rachel      0.269
# 17 Deborah     0.268
# 18 Nathan      0.242
# 19 Adam        0.238
# 20 Aaron       0.234

top20_by_rank <- summarize(top20, rank = unique(rank))
top20_by_rank <- top20_rank[order(top20_rank$rank, decreasing = FALSE),]
top20_rank <- unique(top20_rank$name)
head(as_tibble(top20_rank), 20)

# Top 20 names according to rank are:

# A tibble: 20 x 1
#  value    
#  <chr>    
# 1 David    
# 2 Jacob    
# 3 Michael  
# 4 Noah     
# 5 Deborah  
# 6 Ethan    
# 7 Hannah   
# 8 Elizabeth
# 9 Joshua   
# 10 Ruth     
# 11 Sarah    
# 12 Abigail  
# 13 Judith   
# 14 Daniel   
# 15 Elijah   
# 16 Joseph   
# 17 Benjamin 
# 18 Rachel   
# 19 Rebecca  
# 20 Zachary  

# Multiple names had the same rank. I tried to correct it but was not able to.
# That is why I had to pull out the unique names after sorting the ranks in
# ascending order.
