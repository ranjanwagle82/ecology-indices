library(dplyr)

# Filter the data for the year 2018 and selected sites and gear types
Fourth_data <- Catch_Numbers_2018 %>%
  filter(Year == "2018") %>%
  filter(`Site name` %in% c('Deer Brook Barachois', 'Deer Brook Delta', 'Gadd\'s Harbour (inner)', 'Gadd\'s Harbour (outer)')) %>%
  filter(`Gear type` %in% c("10 m beach seine", "25 m beach seine")) %>% 
  group_by(`Gear type`, `Site name`, `Common name`) %>%
  summarize(`Total caught` = sum(`Number caught`)) %>%
  select(`Gear type`, `Site name`, `Common name`, `Total caught`)

# Filter the data for the desired gear type
filtered_data <- Fourth_data %>%
  filter(`Gear type` %in% c("10 m beach seine")) %>%
  group_by(`Gear type`, `Site name`) %>%
  mutate(`Total caught in site` = sum(`Total caught`)) %>%
  ungroup() %>%
  mutate(`Percentage abundance` = `Total caught` / `Total caught in site` * 100) %>%
  select(`Site name`, `Common name`, `Percentage abundance`)

# Get unique site names
site_names <- unique(filtered_data$`Site name`)

# Initialize an empty data frame to store the minimum abundance values
min_abundance <- data.frame(`Common name` = character(), `Min abundance` = numeric(), `Site1` = character(), `Site2` = character())

# Iterate through each site
for (i in 1:length(site_names)) {
  site1 <- site_names[i]
  
  # Iterate through other sites
  for (j in 1:length(site_names)) {
    site2 <- site_names[j]
    
    # Skip if same site
    if (site1 == site2) {
      next
    }
    
    # Filter data for the two sites
    site1_data <- filtered_data %>%
      filter(`Site name` == site1)
    site2_data <- filtered_data %>%
      filter(`Site name` == site2)
    
    # Merge the data for the two sites based on common name
    merged_data <- merge(site1_data, site2_data, by = "Common name", all = TRUE)
    
    # Calculate the minimum percentage abundance for each common name, considering missing values as 0
    min_data <- merged_data %>%
      mutate(`Min abundance` = pmin(`Percentage abundance.x`, `Percentage abundance.y`, na.rm = TRUE)) %>%
      select(`Common name`, `Min abundance`)
    
    # Add the minimum abundance values to the result data frame
    min_abundance <- rbind(min_abundance, data.frame(`Common name` = min_data$`Common name`, `Min abundance` = min_data$`Min abundance`, `Site1` = site1, `Site2` = site2))
  }
}

# Print the resulting data frame with the minimum abundance values
print(min_abundance)

