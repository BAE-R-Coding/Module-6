########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Homework Module 6
# October 2, 2019 
# Data includes annual observations of yield (lbs/acre) and area harvested (acres) of cotton 
# for each county and agricultural district across the US 
### Objectives: 
# 1. How have year and area harvested of cotton changed across all of NC agricultural districts
# over time? 
# 2. What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018?
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv") #load the package
str(cotton) # we have 11227 rows and 21 columns 
head(cotton)
tail(cotton)
dim(cotton)
summary(cotton)
# Value should be dbl but it is listed as chr because there is text in it. We will fix it next.

# 3.1. Create a NC data subset ----
cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value)

# 3.2. Divide the data_item column ----
# The data_item column contains 2 types of information: the type of cotton (COTTON, UPLAND) and
# the measurment type (ACRES HARVESTED and YIELD, MEASURED IN LB/ACRE)
cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value) %>%
  separate(data_item, sep = "-", into = c("cotton_type", "measurement")) -> cotton2

# 3.3. Convert the value column to numeric type ----
# The value column has "D" in it making it be chr instead of dbl 
cotton3 <- cotton2 %>%
  filter(value != "(D)") %>%
  drop_na()

as.numeric(cotton3$value)
cotton3$value <- as.numeric(cotton3$value)
str(cotton3)
head(cotton3)

# 4. Visualizing trends ----
cotton_viz <- cotton3 %>% 
  ggplot(mapping = aes(x = year, y = value)) + 
  geom_point() +
  theme_minimal()
cotton_viz + 
  facet_grid(measurement ~ ag_district, 
             scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Year",
       y = "", 
       title = "Cotton production in NC", 
       caption = "USDA NASS")
  
# 5. Summarize data from 2018 ----
# What were the top 3 cotton producing counties in NC in terms of lbs of cotton for 2018? 
# Multiply the yield (lb/acre) by the acres harvested 
# Need to spread() yield and acres harvested across two columns 
cotton4 <- cotton3 %>% 
  filter(year == 2018) %>% 
  spread(key = measurement, value = value)
cotton4 <- cotton4 %>%
  setNames(c("year", "state", "ag_district", "county", "cotton_type", "total_acres", "yield"))
cotton4 %>%
  mutate(total_lbs = total_acres*yield) %>%
  select(county, total_lbs) %>%
  top_n(3)
