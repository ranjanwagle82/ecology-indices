library(readxl)

# Read the catch numbers data for the year 2018
Catch_Numbers_2018 <- read_excel("Downloads/data for calculation/Catch Numbers 2018-2022.xlsx", sheet = "CN")

# Filter the data for the desired gear type
Second_data <- Catch_Numbers_2018 %>%
  filter(`Gear type` %in% c("10 m beach seine")) %>% 
  group_by(`Gear type`, `Site name`) %>%
  distinct(`Common name`) %>%
  ungroup()

# Filter the data for the desired gear type
Third_data <- Catch_Numbers_2018 %>%
  filter(`Gear type` %in% c("25 m beach seine")) %>% 
  group_by(`Gear type`, `Site name`) %>%
  distinct(`Common name`) %>%
  ungroup()

# Get unique site names
site_names <- unique(Second_data$`Site name`)

# Create an empty data frame to store the intersection, union, and Jaccard similarity coefficients
count_data1 <- data.frame(
  Gear_Type = character(),
  Site1 = character(),
  Site2 = character(),
  Intersection = numeric(),
  Union = numeric(),
  Jaccard = numeric(),
  stringsAsFactors = FALSE
)

# Iterate over each unique pair of sites
for (i in 1:(length(site_names)-1)) {
  site1 <- site_names[i]
  
  for (j in (i+1):length(site_names)) {
    site2 <- site_names[j]
    
    # Filter the data for site1
    site1_data <- Second_data %>%
      filter(`Site name` == site1)
    
    # Get the unique common names for site1
    site1_common_names <- unique(site1_data$`Common name`)
    
    # Filter the data for site2
    site2_data <- Second_data %>%
      filter(`Site name` == site2)
    
    # Get the unique common names for site2
    site2_common_names <- unique(site2_data$`Common name`)
    
    # Calculate the intersection count between site1 and site2
    intersection_count <- length(intersect(site1_common_names, site2_common_names))
    
    # Calculate the union count between site1 and site2
    union_count <- length(union(site1_common_names, site2_common_names))
    
    # Calculate the Jaccard similarity coefficient
    jaccard_similarity <- intersection_count / union_count
    
    # Append the counts and Jaccard similarity to the data frame
    count_data1 <- rbind(count_data1, data.frame(
      Gear_Type = "10 m beach seine",  # Add the Gear Type column with the value
      Site1 = site1,
      Site2 = site2,
      Intersection = intersection_count,
      Union = union_count,
      Jaccard = jaccard_similarity,
      stringsAsFactors = FALSE
    ))
  }
}

# Create an empty data frame to store the intersection, union, and Jaccard similarity coefficients
count_data2 <- data.frame(
  Gear_Type = character(),
  Site1 = character(),
  Site2 = character(),
  Intersection = numeric(),
  Union = numeric(),
  Jaccard = numeric(),
  stringsAsFactors = FALSE
)

# Iterate over each unique pair of sites
for (i in 1:(length(site_names)-1)) {
  site1 <- site_names[i]
  
  for (j in (i+1):length(site_names)) {
    site2 <- site_names[j]
    
    # Filter the data for site1
    site1_data <- Third_data %>%
      filter(`Site name` == site1)
    
    # Get the unique common names for site1
    site1_common_names <- unique(site1_data$`Common name`)
    
    # Filter the data for site2
    site2_data <- Third_data %>%
      filter(`Site name` == site2)
    
    # Get the unique common names for site2
    site2_common_names <- unique(site2_data$`Common name`)
    
    # Calculate the intersection count between site1 and site2
    intersection_count <- length(intersect(site1_common_names, site2_common_names))
    
    # Calculate the union count between site1 and site2
    union_count <- length(union(site1_common_names, site2_common_names))
    
    # Calculate the Jaccard similarity coefficient
    jaccard_similarity <- intersection_count / union_count
    
    # Append the counts and Jaccard similarity to the data frame
    count_data2 <- rbind(count_data2, data.frame(
      Gear_Type = "25 m beach seine",  # Add the Gear Type column with the value
      Site1 = site1,
      Site2 = site2,
      Intersection = intersection_count,
      Union = union_count,
      Jaccard = jaccard_similarity,
      stringsAsFactors = FALSE
    ))
  }
}

# Merge count_data1 and count_data2
count_data <- rbind(count_data1, count_data2)

# Print the merged data frame
cat("Intersection, Union, and Jaccard similarity coefficients for each site with every other site:\n")
print(count_data)

# Write the merged data frame to the clipboard
clipr::write_clip(count_data)

# Do the same thing for the other code block you mentioned
# Make sure to update the necessary variables and adjust the code accordingly
