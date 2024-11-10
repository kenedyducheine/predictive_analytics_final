#remove.packages("rjson")
#install.packages('rjson', repos = "http://cran.us.r-project.org")

library(rjson)
library(dplyr)
library(lubridate)

data <- read.csv("https://raw.githubusercontent.com/ResidentMario/hurdat2/refs/heads/master/data/atlantic_storms.csv")
data_2 <- read.csv("HadSST.4.0.1.0_monthly_NHEM.csv")
data$date <- as.Date(data$date)

df <- data %>%
  mutate(
    year = year(date),
    month = month(date)
  )

# Convert date in df1 to Date type
df <- df %>%
  mutate(date = as.Date(paste0(year, '-', date)))

df <- df %>%
  mutate(year_month = paste(year, month))

data_2 <- data_2 %>%
  mutate(year_month = paste(year, month))

# Join on year and date
result <- df %>%
  inner_join(data_2, by = "year_month")


result <- result %>%
  select(-latitude, -longitude, -month.x, 
         -year_month, -year.y, -month.y, 
         -total_uncertainty, -uncorrelated_uncertainty, 
         -correlated_uncertainty, -bias_uncertainty, 
         -coverage_uncertainty, -lower_bound_95pct_bias_uncertainty_range)
result <- result[,-23]

result <- result %>%
  filter(year.x >= 2005 & year.x <= 2015)

result <- result[,-c(9:20)]

result <- result %>%
  mutate(status_of_system = if_else(status_of_system != "HU", "TS", status_of_system))

summary(result)

write.csv(result, "final_project_dataset.csv")
