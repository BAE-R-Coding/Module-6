########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
# Author: Jimmy Larson
# Primary objective: 
## How have yield and area harvested of cotton changed across all of the NC agricultural districts over time?
## What were the top 3 cotton producting counties in NC in terms of total lbs of cotton for 2018?
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
head(cotton)
tail(cotton)
dim(cotton)
summary(cotton)
# 3.1. Create a NC data subset ----
cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value) -> NC
# 3.2. Divide the data_item column ----
NC %>%
  separate(data_item,
           into = c("cotton_type", "measurement"),
           sep = "-") -> NC
# 3.3. Convert the value column to numeric type ----
NC %>%
  filter(! value == "(D)") -> NC
NC$value <- as.numeric(NC$value)
str(NC)
# 4. Visualizing trends ----
ggplot(data = NC, mapping = aes(x = year, y = value))+
  geom_point()+
  theme_minimal()+
  facet_grid(measurement ~ ag_district, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Year",
       title = "Cotton production in NC",
       caption = "Source: USDA NASS")
# 5. Summarize data from 2018 ----
NC %>% 
  filter(year == "2018") %>%
  spread(measurement, value) -> NC_2018
NC_2018 %>%
  mutate(total_lbs = NC_2018$` ACRES HARVESTED` * NC_2018$` YIELD, MEASURED IN LB / ACRE`) %>%
  select(county, total_lbs) %>%
  top_n(3) 
