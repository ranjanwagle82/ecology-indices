# Field Course Data Analysis Code: Analyzing Catch Numbers, Collection Tracking, and Diversity Metrics


library(readxl)
library(dplyr)

# Read Catch Numbers and Collection Tracking data for the year 2018
Catch_Numbers_2018 <- read_excel("Downloads/data for calculation/Catch Numbers 2018-2022.xlsx", sheet = "CN")
Collection_2018 <- read_excel("Downloads/data for calculation/Collection Tracking 2018-2022.xlsx", sheet = "CT")

# Filter and select relevant columns from Catch Numbers data
Firstdata <- Catch_Numbers_2018 %>%
  filter(`Gear type` %in% c("10 m beach seine", "25 m beach seine")) %>% 
  group_by(`Gear type`, `Site name`) %>%
  select(Year, `Gear type`, `Site name`, `Common name`, `Number caught`, `Unique Tow`)

# Filter and select relevant columns from Collection Tracking data
Collection_Firstdata <- Collection_2018 %>%
  filter(`Gear type` %in% c("10 m beach seine", "25 m beach seine")) %>% 
  group_by(`Gear type`, `Site name`) %>%
  select(Year, `Gear type`, `Site name`, `Unique Tow`, `Fish caught?`)

# Merge the filtered data from Catch Numbers and Collection Tracking
merge_data <- merge(Firstdata, Collection_Firstdata, by = c("Year", "Gear type", "Site name", "Unique Tow"), all.x = TRUE, all.y = TRUE)

# Count unique values and calculate diversity metrics
count <- merge_data %>%
  group_by(`Site name`, `Gear type`) %>%
  summarize(`S` = n_distinct(na.omit(`Common name`)), 
            `No. of tow` = n_distinct(na.omit(`Unique Tow`)),
            `S'` = (S / `No. of tow`),
            `Common names` = toString(unique(na.omit(`Common name`)))), 
.groups = 'drop')

# Calculate H' diversity metric
H_diversity_summary <- Catch_Numbers_2018 %>%
  filter(`Gear type` %in% c("10 m beach seine", "25 m beach seine")) %>% 
  group_by(`Gear type`, `Site name`, `Common name`) %>%
  summarize(`Total Number Caught` = sum(`Number caught`)) %>%
  ungroup() %>%
  group_by(`Gear type`, `Site name`) %>%
  mutate(`Pi` = `Total Number Caught` / sum(`Total Number Caught`),
         Pi_ln = Pi * log(Pi)) %>%
  group_by(`Gear type`, `Site name`) %>%
  summarize(`H'` = (-1) * sum(Pi_ln))

# Join the diversity metrics with the count data
Final_data <- H_diversity_summary %>%
  left_join(count, by = c("Site name", "Gear type"))













