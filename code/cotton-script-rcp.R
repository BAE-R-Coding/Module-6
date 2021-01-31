########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
head(cotton)
tail(cotton)
#determine the amount of states listed in dataset
unique(cotton$state)
#Only North Carolina data have been provided
dim(cotton)


# 3.1. Create a NC data subset ----
#we only have NC data already
# select only the rows we need: year, ag_district, county, data_item, value
cotton %>%
  select(year, ag_district, county, data_item, value) -> ncc #ncc is nc cotton subset


# 3.2. Divide the data_item column ----
ncc2 <- ncc %>%
  separate(data_item,
           into = c("cotton_type", "measurement"),
           sep = " - "
  )

# 3.3. Convert the value column to numeric type ----
#test that as numeric works, it doesn't because of the (D)'s
as.numeric(ncc$value)
#filter out the (D)'s and rename
ncc3 <- ncc2 %>%
  filter(value !="(D)") 
#check if as.numeric works
as.numeric(ncc3$value)
#it works this time, make new assignment
ncc3$value <- as.numeric(ncc3$value)
#check to see if it worked
head(ncc3) #value is now of type dbl



# 4. Visualizing trends ----

ncc3 %>%
  ggplot(mapping = aes(x = year, y = value))+
    geom_point()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90))+
  facet_grid(rows = vars(measurement), cols = vars(ag_district), # facet rows by what type of value is being displayed, cols by                                                                   which ag district 
             scales = "free_y")

#Cotton production in terms of yield has increased dramtically in the past century



# 5. Summarize data from 2018 ----

#rename and spread ncc3 to have seperate columns for acres harvested and yield in lb/ac
#this will allow us to compute total cotton in lb.

ncc5 <- ncc3 %>%
  group_by(county) %>% #this was a suggestion I found online but was still given the same error message after running it this way
  spread(key = measurement, value = value)
  
  
  
  
  













