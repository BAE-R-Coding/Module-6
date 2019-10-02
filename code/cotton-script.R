########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)


# 2. Read & inspect the dataset ----
cot <- read_csv("cotton-usda-nass.csv")
str(cot)
head(cot)
tail(cot)
dim(cot)
summary(cot)


# 3.1. Create a NC data subset ----

cot %>%
  select(year,  state , ag_district , county, data_item, value) %>%
  separate(data_item,
           into = c("cotton_type", "measurements"),
           sep=" - ") -> cot
cot %>%
  filter(value != "(D)") -> cot
cot$value <- as.numeric(cot$value)
view(cot)
head(cot)
ggplot(cot, aes(year,value))+
  geom_line()+
  geom_point()+
  facet_grid(measurements ~ ag_district, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

cot %>%
  filter(year == "2018") -> cot
view(cot)


 




  


# 3.2. Divide the data_item column ----
# 3.3. Convert the value column to numeric type ----
# 4. Visualizing trends ----
# 5. Summarize data from 2018 ----