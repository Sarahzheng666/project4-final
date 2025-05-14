# === Project 4: Cafeteria Observation Visualisation ===

# Load necessary packages
library(tidyverse)
library(stringr)

# Read data from Google Sheet CSV link
data_url <- "https://docs.google.com/spreadsheets/d/1ehSSLBWXz5UVCQPIX5Tr0pa0Wz6GDoakDDjipf9M0F0/export?format=csv"
raw_data <- read_csv(data_url)

# Rename columns for convenience
cafeteria_data <- raw_data %>%
  rename(
    queue_length = `How many people are currently queuing in the cafeteria?`,
    food_type = `What type of food do most people appear to be ordering?`,
    comments = `Are there any other notable things you observed?`
  )

# View structure
glimpse(cafeteria_data)

# === Plot 1: Bar chart of food types ===
plot1 <- ggplot(cafeteria_data, aes(x = food_type)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Food Types",
       x = "Food Type", y = "Count") +
  theme_minimal()
ggsave("plot1.png", plot1)

# === Plot 2: Average queue length by food type ===
plot2 <- cafeteria_data %>%
  group_by(food_type) %>%
  summarise(avg_queue = mean(queue_length, na.rm = TRUE)) %>%
  ggplot(aes(x = food_type, y = avg_queue)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Queue Length by Food Type",
       x = "Food Type", y = "Average Queue Length") +
  theme_minimal()
ggsave("plot2.png", plot2)

# === Plot 3: Count of 'slow' or 'cut' keywords in comments ===
plot3 <- cafeteria_data %>%
  mutate(comment_type = case_when(
    str_detect(str_to_lower(comments), "slow") ~ "Mentions 'slow'",
    str_detect(str_to_lower(comments), "cut") ~ "Mentions 'cut'",
    TRUE ~ "No keyword"
  )) %>%
  count(comment_type) %>%
  ggplot(aes(x = comment_type, y = n, fill = comment_type)) +
  geom_col() +
  labs(title = "Occurrences of 'slow' or 'cut' in Comments",
       x = "Comment Type", y = "Count") +
  theme_minimal()
ggsave("plot3.png", plot3)
