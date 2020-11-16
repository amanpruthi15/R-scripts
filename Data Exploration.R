####################################
## Aman Pruthi
## R as a Research Tool
## Homework 9
## Due 11/03/20
###################################

## This file creates data summaries and graphs for the Ecological Strategies of
## Plants fall 2011 Big Bend National Park field trip.

## Your tasks:

## Make sure you understand the code I've written. I've left comments for you
## to help you. Then add your own code below to answer the questions 1-5 at the
## end of this script.

library(ggplot2)
library(dplyr)
library(stringr)

# Read transects
transects <- read.csv(
  "http://r-research-tool.schwilk.org/data/BBNP_transects_2011.csv",
  stringsAsFactors= FALSE)

# Turn transect column into globally unique ids. This is necessary for later
# expand.grid and gives us a single column that uniquely identifies a
# transect.
transects <- mutate(transects, transect = paste(site, transect, sep="."))

# Calculate the distance each plant covers for every entry
transects$dist <- transects$stop - transects$start

# Replace missing dieback values with zero. Field teams left the dieback cell
# blank if there was no dieback.
transects$dieback[is.na(transects$dieback)] <- 0

# Calculate living cover distance. Dieback was reported as an estimated
# percentage for every entry.
transects$live.dist <- transects$dist * transects$dieback/100

# Now just get the cover by species. We don't need the raw start/stop points.
# Note that each transect was 50 m long, so the proportional cover of any group
# (species, family, growth form, etc) is just the sum of the distances in that
# transect divided by 50. We can summarize by species now and then sum across
# other categories later.
cover <- group_by(transects, transect, spcode) %>% summarize(cover = sum(dist)/50)

# But, in theory every species in the whole data set should have a value for
# every transect. If I species did not occur there, it should be zero cover but
# of course instead is simply missing. We must add in zeroes for missing
# species.

# Use expand.grid to get all possible combinations of transect and species ids
all.transects.species <- expand.grid(transect = unique(cover$transect),
                                     spcode=unique(cover$spcode))
cover <- merge(cover, all.transects.species, all=TRUE)
# and NA should really be zero
cover$cover[is.na(cover$cover)] <- 0

# Now grab site again as its own column because we will need it for analyses:
cover <- mutate(cover, site = str_split_fixed(transect, "\\.", n=2)[,1])

# Read species list. Give explicit NA string for character vectors as otherise
# empty fields are considered the "" string.
species <- read.csv(
  "http://r-research-tool.schwilk.org/data/BBNP_species.csv",
  stringsAsFactors=FALSE, na.strings = "")

# Note that "species" that start with "(" and end with ")" are morphospecies
# for which we do not have genus and specific epithet.

# Extract genus from species name. Returns NA for unidentified species (names
# start with "(" in that case)
species <- mutate(species, genus=str_match(species,"(^[A-Z][a-z]+)( )")[,2])

# Merge species info with transect data. Use merge() function and remember that
# we want all rows from the transects data frame in the result. We don't need
# unmatched rows in species as the file contains the full species list over
# several years and some species were not found in 2011.
cover <- merge(cover, species, all.x=TRUE)

# Read in elevations of each site and merge with cover data
sites <- read.csv(
  "http://r-research-tool.schwilk.org/data/BBNP_sites.csv",
  stringsAsFactors=FALSE, na.strings = "")

# Although elevation is numeric, it is really a categorical variable with six
# values. Let's ensure that ggplot will treat it as a categorical variable.
sites$elev <- as.character(sites$elev)

cover <- merge(cover, sites)

# 'cover' is the result dataframe for all further use. We could delete the
# intermediate objects to keep our environment cleaner:
rm(all.transects.species, sites, species, transects)

###############################################################################
## Some example analyses.
###############################################################################

# Summarize proportional cover by transect and by growth form
by_elev_gf <- group_by(cover, elev, transect, gf)
gf.cover <- summarize(by_elev_gf, pcover = sum(cover))

# Just pull out trees
trees <- subset(gf.cover, gf=="tree") # gf is growth form

# and plot proportional tree cover by site
ggplot(trees, aes(elev, pcover)) + geom_boxplot() +
  xlab("Elevation (m)") +
  ylab("Proportional cover")


# Good, oaks only occur at 2 sites and are zero cover elsewhere

################################################################################
## Now, you do the rest:

## 1. Produce a new data frame called 'family_cover' that contains the
## proportional cover of every plant family by transect.

by_elev_family <- group_by(cover, elev, transect, family)
family_cover <- summarize(by_elev_family, pcover = sum(cover))
head(family_cover)

# A tibble: 6 x 4
# Groups:   elev, transect [1]
#   elev  transect family        pcover
#   <chr> <chr>    <chr>          <dbl>
# 1 1133  C1.BL1   Agavaceae     0.0884
# 2 1133  C1.BL1   Anacardiaceae 0     
# 3 1133  C1.BL1   Asteraceae    0.118 
# 4 1133  C1.BL1   Berberaceae   0     
# 5 1133  C1.BL1   Cactaceae     0     
# 6 1133  C1.BL1   Cupressaceae  0 

###############################################################################
## 2. subset out the cacti (family=="Cactaceae") from 'family_cover' as a new
## data frame called 'cacti_cover'

cacti_cover <- subset(family_cover, family=="Cactaceae")
head (cacti_cover)

# A tibble: 6 x 4
# Groups:   elev, transect [6]
#   elev  transect family    pcover
#   <chr> <chr>    <chr>      <dbl>
# 1 1133  C1.BL1   Cactaceae 0     
# 2 1133  C1.BL2   Cactaceae 0     
# 3 1133  C1.BL3   Cactaceae 0.0236
# 4 1133  C1.CT1   Cactaceae 0.022 
# 5 1133  C1.CT2   Cactaceae 0     
# 6 1133  C1.CT3   Cactaceae 0.01  

###############################################################################
## 3. and then plot cactus cover by elevation

cacti_plot <- ggplot(cacti_cover, aes(elev, pcover)) + geom_boxplot() +
              xlab("Elevation (m)") +
              ylab("Proportional cover") 
cacti_plot

# Cacti occur at all the sites but have highest occurance at 864m and 1406m and
# lowest occurance at 665m.

###############################################################################
## 4. Do the same thing for the oaks ("Fagaceae") as you did for the cacti

fagaceae_cover <- subset(family_cover, family=="Fagaceae")

fagaceae_plot <- ggplot(fagaceae_cover, aes(elev, pcover)) + geom_boxplot() +
                 xlab("Elevation (m)") +
                 ylab("Proportional cover") 
fagaceae_plot

# Beech family plants only occur at 1687m and 1911m and are zero elsewhere.

###############################################################################
## 5. Tell me something else interesting about these data. Show how you
## determined this! Take some time to explore the data.

gf.cover$gf[is.na(gf.cover$gf)] <- 0
new_gf_cover <- gf.cover[!(gf.cover$gf=="0"),]
new_gf_cover <- new_gf_cover[!(new_gf_cover$pcover=="0"),]

new_gf_cover$elev <- factor(new_gf_cover$elev, 
                     levels = c("665", "864", "1133", "1406", "1687", "1911"))

p <- ggplot(new_gf_cover, aes(elev, pcover, color=gf)) + 
  geom_point(size = 2, alpha = 0.5)

p + facet_grid(gf~.)  + geom_jitter(width = 0.2, height = 0.5) +
  labs(x = "Elevation (m)", y = "Proportional cover",
  color = "Growth form of species")


## Vines only occur at an elevation of 864 m.
## Trees occur at the highest elevation of 1687m and 1911m and they have the
## highest proportional cover as well.
## Succulents are present everywhere just not at 665m.
## Shrubs are present everywhere but their proportional cover decreases with
## increase in elevation.
## Interestingly, subshrubs are just not present at 1406m and 1911m but they 
## are present at 1687m.

###############################################################################