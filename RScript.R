install.packages(c("tidyverse", "ggplot2", "lubridate", "ggpubr"))
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggpubr)

dataset <- read.csv("Final_Adjusted_Business_Cycles_Data.csv")

dataset <- dataset %>%
  rename(
    Week = Week,
    GDP_Ecuador = GDP_Ecuador....,
    GDP_US = GDP_US....,
    GDP_China = GDP_China....,
    Trade_US = Trade_US..Billion.USD.,
    Trade_China = Trade_China..Billion.USD.,
    Period = Period
  )

dataset$Week <- as.Date(dataset$Week)

fta_date <- as.Date("2024-05-01")

dataset <- dataset %>%
  mutate(Period = if_else(Week < fta_date, "Pre-FTA", "Post-FTA"))

pre_fta_data <- dataset %>% filter(Period == "Pre-FTA")
post_fta_data <- dataset %>% filter(Period == "Post-FTA")

cor_pre_china <- cor(pre_fta_data$GDP_Ecuador, pre_fta_data$GDP_China, use = "complete.obs")
cor_pre_us <- cor(pre_fta_data$GDP_Ecuador, pre_fta_data$GDP_US, use = "complete.obs")
cor_post_china <- cor(post_fta_data$GDP_Ecuador, post_fta_data$GDP_China, use = "complete.obs")
cor_post_us <- cor(post_fta_data$GDP_Ecuador, post_fta_data$GDP_US, use = "complete.obs")

cor_test_pre_china <- cor.test(pre_fta_data$GDP_Ecuador, pre_fta_data$GDP_China)
cor_test_pre_us <- cor.test(pre_fta_data$GDP_Ecuador, pre_fta_data$GDP_US)
cor_test_post_china <- cor.test(post_fta_data$GDP_Ecuador, post_fta_data$GDP_China)
cor_test_post_us <- cor.test(post_fta_data$GDP_Ecuador, post_fta_data$GDP_US)

ggplot(dataset, aes(x = Week)) +
  geom_line(aes(y = GDP_Ecuador, color = "Ecuador")) +
  geom_line(aes(y = GDP_China, color = "China")) +
  geom_line(aes(y = GDP_US, color = "US")) +
  geom_vline(xintercept = as.numeric(fta_date), linetype = "dashed", color = "red") +
  labs(title = "GDP Trends Over Time", y = "GDP Growth Rate (%)", color = "Country") +
  theme_minimal()

ggplot(pre_fta_data, aes(x = GDP_China, y = GDP_Ecuador)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Pre-FTA Ecuador vs. China GDP", 
       x = "China GDP Growth (%)", 
       y = "Ecuador GDP Growth (%)") +
  theme_minimal()

ggplot(post_fta_data, aes(x = GDP_China, y = GDP_Ecuador)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Post-FTA Ecuador vs. China GDP", 
       x = "China GDP Growth (%)", 
       y = "Ecuador GDP Growth (%)") +
  theme_minimal()

ggplot(pre_fta_data, aes(x = GDP_US, y = GDP_Ecuador)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Pre-FTA Ecuador vs. US GDP", 
       x = "US GDP Growth (%)", 
       y = "Ecuador GDP Growth (%)") +
  theme_minimal()

ggplot(post_fta_data, aes(x = GDP_US, y = GDP_Ecuador)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Post-FTA Ecuador vs. US GDP", 
       x = "US GDP Growth (%)", 
       y = "Ecuador GDP Growth (%)") +
  theme_minimal()

final_correlation_table <- tibble(
  Period = c("Pre-FTA", "Pre-FTA", "Post-FTA", "Post-FTA"),
  Correlation = c("Ecuador-China", "Ecuador-US", "Ecuador-China", "Ecuador-US"),
  R_Value = c(cor_pre_china, cor_pre_us, cor_post_china, cor_post_us),
  P_Value = c(cor_test_pre_china$p.value, cor_test_pre_us$p.value, cor_test_post_china$p.value, cor_test_post_us$p.value)
)

write.csv(final_correlation_table, "Final_Correlation_Table.csv", row.names = FALSE)

pretty_table <- ggtexttable(
  final_correlation_table,
  rows = NULL,
  theme = ttheme("classic", base_size = 12)
)

pretty_table

ggsave("Final_Correlation_Table.png", plot = pretty_table, width = 6, height = 4)