####################################
## Aman Pruthi
## R as a Research Tool
## Homework 7 - Strings
## Due 10/20/20
###################################
## Instructions
## ------------

## Add code to this file to answer the four multipart questions below. I've
## included some pre-made character vector objects to use.


###############################################################################
## 1: Using the following character vector, 'wordlist', write grep() function
## calls using regular expressions to match the words given in the subpart
## instructions A, B and C:

wordlist <- unlist(strsplit("The rain in Spain falls mainly on the plain",
                            split=" "))

## A) Match "rain, "Spain" , "mainly" and "plain"
## B) Match "rain", "Spain" and "plain"  only
## C) Match "plain" and "Spain" only

## Answer:

wordlist[grep("ain", wordlist)]
# [1] "rain"   "Spain"  "mainly" "plain"

wordlist[grep("ain\\b", wordlist)]
# [1] "rain"  "Spain" "plain"

wordlist[grep("[a-zA-Z]{2}ain\\b", wordlist)]
# [1] "Spain" "plain"

###############################################################################
## 2. Using the character vector below, write grep() function calls that
## extract just the entries that are telephone numbers formatted with area code
## in parentheses (for example, "(806) 742-2710" will match but "806-742-2710"
## should not.)

numbers <- c("(806) 555-5510", "567-00-1111", "(101) 747-5710",
             "(806)-555-0000", "(806) 742-2710", "(678)-56,00-000-1111",
             "(123)456789", "(806) 742-2227")
## Answer:

numbers[grep("(\\([0-9]{3})\\)[ ][0-9]{3}-[0-9]{4}", numbers)]
# [1] "(806) 555-5510" "(101) 747-5710" "(806) 742-2710" "(806) 742-2227"

###############################################################################
## 3. Using the character vector below, write a regular expression (and test
## with grep()) that

phrases <- c("Top of the morning", "good morning", "morning has broken",
             "morningtime")

## A) matches the first two elements
## B) matches the first three elements

# aim for the most general pattern that still matches just the specified
# elements

## Answers:

phrases[grep("morning\\b$", phrases)]
# [1] "Top of the morning" "good morning" 

phrases[grep("morning\\b", phrases)]
# [1] "Top of the morning" "good morning"       "morning has broken"

###############################################################################
## 4. Play around with the stringr package and show me three examples. Show me
## at least one example using str_match() to extract a group from a match. One
## idea: Try extracting just the digits from valid telephone numbers in
## Question 2, above.

library(stringr)

## Example 1 - Extracting a group from a match

phoneNumbers <- numbers[grep("(\\([0-9]{3}))[ ][0-9]{3}-[0-9]{4}", numbers)]
number_pattern <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [)- ]?   # optional closing parens, dash, or space
  [-|\\s]  # dash or space
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{4}) # four more numbers
  ", comments = TRUE)
str_match(phoneNumbers, number_pattern)
#      [,1]             [,2]  [,3]  [,4]  
# [1,] "(806) 555-5510" "806" "555" "5510"
# [2,] "(101) 747-5710" "101" "747" "5710"
# [3,] "(806) 742-2710" "806" "742" "2710"
# [4,] "(806) 742-2227" "806" "742" "2227"

## Example 2 - Matching patterns with regular expressions

x <- "Ferrari is one of the most expensive cars"
str_view(x, "Ferrari")
str_view_all(x, boundary("word"))

## Example 3 - Detecting letters using str_detect

y <- unlist(strsplit(x, split=" "))
str_detect(y, "e")
# [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE

# What proportion of words start with a vowel
mean(str_detect(y, "[aeiou]$"))
# [1] 0.5

## Example 4 - replacing words using str_replace

str_replace(y, "Ferrari", "Porsche")
# [1] "Porsche"   "is"        "one"       "of"        "the"       "most"     
# [7] "expensive" "cars"

str_replace_all(y, c("Ferrari" = "Hyundai", "expensive" = "economic"))
# [1] "Hyundai"  "is"     "one"      "of"      "the"      "most"     "economic"
# [8] "cars"
################################## END ########################################