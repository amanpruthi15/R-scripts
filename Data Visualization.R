####################################
## Aman Pruthi
## R as a Research Tool
## Homework 8 - clean-birdd.r
## Due 10/27/20
###################################

## This script downloads and cleans up morphological data from Galapagos
## finches, which is available from BIRDD: Beagle Investigation Return with
## Darwinian Data at http://bioquest.org/birdd/morph.php

library(ggplot2)

### Clean up data:
##################
birdd <- read.csv("http://bioquest.org/birdd/Morph_for_Sato.txt",
                  sep="\t", stringsAsFactors=FALSE, strip.white=TRUE)

names(birdd) <- tolower(names(birdd)) # make columns names lowercase

birdd <- subset(birdd, islandid == "Flor_Chrl") # take only one island
# only keep these columns:
birdd <- birdd[,c("islandid", "taxonorig", "sex", "wingl", "beakh", "ubeakl")]
names(birdd)[1:2] <- c("island", "taxon")
birdd <- data.frame(na.omit(birdd)) # remove all rows with any NAs
birdd$taxon <- factor(birdd$taxon) # remove extra remaining factor levels
birdd$sex <- factor(birdd$sex) # remove extra remaining factor levels
row.names(birdd) <- NULL # tidy up the row names
head(birdd)
# Quick look at wingl:
ggplot(birdd, aes(taxon, wingl)) + geom_boxplot() + coord_flip()

#############################################################################
## Question 1 - Use ggplot to show me how wing length ("wingl") is related to 
## taxon and sex. Decide what is the best geom and aesthetics. Explain what 
## other geoms you tried before you decided.

p <- ggplot(birdd, aes(wingl, taxon, color = sex)) + geom_boxplot() 
     
p + facet_wrap(~sex) + theme(axis.text.y = element_text(face = "italic")) +
  labs(x = "Wing Length (mm)", y = "Taxon of Galapagos finches", color = "Sex",
  title = "Relationship between sex and wing length of taxa of Galapagos finches") 

## Answer:

# Wing length of males is comparatively longer with a median between 70-75mm to
# that of females with a median between 65-70mm. Wings of males are longest for
# taxa "Geospiza fortis". The shortest wing length is of "Geospiza fuliginosa"
# and "Camarhynchus parvulus" in females.

# I tried geom_point() but it was not a good representation of the data. Then
# I tried geom_line but it was showing a continuous line showing just a range of
# of wing lengths and not the values. Finally, geom_point() makes the data more
# readable

#############################################################################
## Question 2 - Investigate the relationship between wing length and beak 
## height across taxa and sex. Is any one taxon morphologically distinct from 
## the others? Create a graph to illustrate your answer.


p1 <- ggplot(birdd, aes(wingl, beakh, color=sex)) + geom_point() + coord_flip()

## Applying linear model to see the trend of values 
p1 <- p1 + geom_smooth(aes(group = NULL), color="green", method="lm", se=FALSE)

p1 + facet_wrap( ~ taxon) + theme(strip.text.x = element_text(face = "italic")) +
    labs(x = "Wing Length (mm)", y = "Beak Height (mm)", color = "Sex", title = 
"Relationship between wing length and beak height across sex and taxa of Galapagos finches")

## Answer:

# Wing length is slightly proportional to beak height as there is a minute slope
# Beak height is slightly longer in males than females. But in "Geospiza fortis"
# beak height in males is evidently longer than female and we can say this as 
# there are more number of birds for that taxa.

# In taxa "Geospiza magnirostris" there is an abrupt increase in beak height 
# with a small increase in wing length and has the longest beak as well. But
# there are only 2 males representing the taxa so it is not that significant.

#############################################################################
## Question 3 - Go back and look at hw08-clean-birdd.R. Why did I use the 
## strip.white argument to read.csv? What problem(s) in these data show up if 
## you omit that argument? Try it out and explain.

birdd <- read.csv("http://bioquest.org/birdd/Morph_for_Sato.txt",
                  sep="\t", stringsAsFactors=FALSE)

names(birdd) <- tolower(names(birdd)) 
birdd <- subset(birdd, islandid == "Flor_Chrl") 
birdd <- birdd[,c("islandid", "taxonorig", "sex", "wingl", "beakh", "ubeakl")]
names(birdd)[1:2] <- c("island", "taxon")
birdd <- data.frame(na.omit(birdd)) 
birdd$taxon <- factor(birdd$taxon) 
birdd$sex <- factor(birdd$sex) 
row.names(birdd) <- NULL 

##  Answer:

# When there is a space before a data value, then R recognizes it as a unique 
# value/variable. Using strip.white = TRUE eliminates all white spaces added to 
# a data value. If strip.white = FALSE, it create new values in the data, thus 
# giving false results.

## When strip.white=TRUE, we get 12 taxon.

unique(birdd$taxon)

# [1] Camarhynchus parvulus              Geospiza prosthemelas prosthemelas
# [3] Camarhynchus pauper                Geospiza paupera                  
# [5] Camarhynchus psittacula            Geospiza fortis                   
# [7] Geospiza fortis fortis             Geospiza fuliginosa               
# [9] Geospiza fuliginosa parvula        Geospiza magnirostris             
# [11] Geospiza scandens                 Geospiza scandens scandens        
# 12 Levels: Camarhynchus parvulus Camarhynchus pauper ... Geospiza scandens scandens

## When the argument is omitted, we get 13 taxon.

unique(birdd$taxon)

# [1] Camarhynchus parvulus              Geospiza prosthemelas prosthemelas
# [3] Camarhynchus pauper                Geospiza paupera                  
# [5] Camarhynchus psittacula            Geospiza fortis                   
# [7] Geospiza fortis                    Geospiza fortis fortis            
# [9] Geospiza fuliginosa                Geospiza fuliginosa parvula       
# [11] Geospiza magnirostris             Geospiza scandens                 
# [13] Geospiza scandens scandens        
# 13 Levels: Camarhynchus parvulus Camarhynchus pauper ... Geospiza scandens scandens