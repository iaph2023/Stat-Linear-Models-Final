data <- read.csv(file.choose())
summary(data)
data <- data[!is.na(data$Series.Code) & nzchar(trimws(data$Series.Code)), ]
require(dplyr)
data$X2019..YR2019. <- as.numeric(
  na_if(trimws(data$X2019..YR2019.), "..")
)
library(tidyr)
indicator_names <- c(
  "Life expectancy at birth, total (years)" = "life_expectancy",
  "GDP per capita, PPP (constant 2021 international $)" = "gdp",
  "People using at least basic sanitation services (% of population)" = "sanitation",
  "Current health expenditure per capita, PPP (current international $)" = "health_expenditure",
  "People using at least basic drinking water services (% of population)" = "drinking_water",
  "Immunization, measles (% of children ages 12-23 months)" = "measles_immunization",
  "Prevalence of undernourishment (% of population)" = "undernourishment"
)
data$variable <- unname(indicator_names[data$Series.Name])
clean <- data[!is.na(data$variable),
             c("Country.Name", "Country.Code", "variable",
               "X2019..YR2019.")]
data <- pivot_wider(
  clean,
  names_from = variable,
  values_from = X2019..YR2019.
)
summary(data)
hist(data$life_expectancy, main = "Life Expectancy", xlab = "Years")
hist(data$undernourishment, main = "Prevalence of Undernourishment", 
     xlab = "Percentage of Population")
hist(data$measles_immunization, main = "Measles Immunization", 
     xlab = "Percentage of children ages 12-23 months")
hist(data$drinking_water, main = "Access to Basic Drinking Water",
     xlab = "Percentage of Population")
hist(data$health_expenditure, main = "Health Expenditure",
     xlab = "Expenditure per capita")
hist(data$sanitation, main = "Access to Basic Santiation",
     xlab = "Percentage of Population")
hist(data$gdp, main = "GDP", xlab = "GDP per capita")
lmod <- lm(life_expectancy ~ undernourishment + measles_immunization + drinking_water
     + health_expenditure + sanitation + gdp, data)
summary(lmod)
