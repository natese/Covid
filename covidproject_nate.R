getwd()

library(tidyverse)
library(lubridate)
library(scales)

deaths <- read_csv('CovidDeaths.csv')
vaccinations <- read_csv('CovidVaccinations.csv')

glimpse(deaths)
glimpse(vaccinations)

deaths$date <- mdy(deaths$date)
vaccinations$date <- mdy(vaccinations$date)
str(deaths$date)

#checking for missing data
colSums(is.na(deaths))
colSums(is.na(vaccinations))

#Looking for data where location AND date exist
deaths_clean <- deaths %>%
  filter(!is.na(location) & !is.na(date))

vaccinations_clean <- vaccinations %>%
  filter(!is.na(location) & !is.na(date))

#joining the two data sets to see covid deaths and vaccinations together for each place and date.

covid_data <- inner_join(deaths_clean, vaccinations_clean,
                         by = c('location', 'date'))

head(covid_data)

#How many total cases and deaths were there per country?

total_cases <- covid_data %>%
  group_by(location) %>%
  summarise(total_cases = max(total_cases, na.rm = TRUE),
            total_deaths = max(total_deaths, na.rm = TRUE)) %>%
  arrange(desc(total_cases))

print(total_cases)

#Plot: Daily New Cases Over Time for the United States

country_data <- deaths_clean %>%
  filter(location == "United States")

ggplot(country_data, aes(x = date, y = new_cases)) +
  geom_line(color = 'steelblue') +
  labs(title = 'Daily New COVID Cases in United States',
       x = 'Date',
       y = 'New Cases') +
  scale_y_continuous(labels = comma)


# Plot: Looking at the total cases vs total deaths for all countries to see which countries had higher death rates relative to total cases

library(ggrepel)

country_totals <- deaths_clean %>%
  group_by(location) %>%
  summarise(total_cases = max(total_cases, na.rm = TRUE),
            total_deaths = max(total_deaths, na.rm = TRUE))

# filtering out continents/global totals to focus on countries

country_totals_filtered <- country_totals %>%
  filter(!location %in% c('World', 'Europe', 'European Union', 
                          'Asia', 'South America', 'North America',
                          'Africa', 'Oceania'))

# plotting filtered data
ggplot(country_totals_filtered, aes(x = total_cases, y = total_deaths, label = location)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(size = 3) +
  labs(title = "Total Cases vs Total Deaths by Country (No aggregates)",
       x = "Total Cases",
       y = "Total Deaths") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Plot: Looking at vaccinations vs cases to see if countries with more vaccinations had fewer cases.

vacc_cases <- covid_data %>%
  filter(!location %in% c("World", "Europe", "European Union",
                          "Asia", "South America", "North America",
                          "Africa", "Oceania")) %>%
  group_by(location) %>%
  summarise(
    total_vaccinations = max(total_vaccinations, na.rm = TRUE),
    total_cases = max(total_cases, na.rm = TRUE)
  )

#plotting filtered data 

ggplot(vacc_cases, aes(x = total_vaccinations, y = total_cases, label = location)) +
  geom_point(color = "darkgreen") +
  geom_text_repel(size = 3) +
  labs(title = "Total Vaccinations vs Total Cases by Country (No aggregates)",
       x = "Total Vaccinations",
       y = "Total Cases") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)


# Comparing daily new cases for US, India, and Brazil

multi_country <- deaths_clean %>%
  filter(location %in% c('United States', 'India', 'Brazil'))

ggplot(multi_country, aes(x = date, y = new_cases, colour = location)) +
  geom_line() +
  labs(title = 'Daily New Cases: US vs India vs Brazil',
       x = 'Date',
       y = 'New Cases') +
  scale_y_continuous(labels = comma)

# Looking at deaths per capita. Total deaths alone can be misleading as big countries will have bigger totals. To look at the impact on smaller countries I will calculate deaths per million.
# Deaths per million = total deaths / population * 1000000

glimpse(deaths_clean)
countries_per_capita <- deaths_clean %>%
  filter(!location %in% c("World", "Europe", "European Union",
                          "Asia", "South America", "North America",
                          "Africa", "Oceania")) %>%
  group_by(location) %>%
  summarise(
    total_deaths = max(total_deaths, na.rm = TRUE),
    population = max(population, na.rm = TRUE)
  ) %>%
  mutate(
    deaths_per_million = (total_deaths / population) *1000000
  )

head(countries_per_capita)

#Plotting deaths per million in comparison to total cases

country_per_capita <- deaths_clean %>%
  filter(!location %in% c("World", "Europe", "European Union",
                          "Asia", "South America", "North America",
                          "Africa", "Oceania")) %>%
  group_by(location) %>%
  summarise(
    total_cases = max(total_cases, na.rm = TRUE),
    total_deaths = max(total_deaths, na.rm = TRUE),
    population = max(population, na.rm = TRUE)
  ) %>%
  mutate(
    deaths_per_million = (total_deaths / population) * 1000000
  )

ggplot(country_per_capita, aes(x = total_cases, y = deaths_per_million, label = location)) +
  geom_point(color = "purple") +
  geom_text_repel(size = 3) +
  labs(
    title = "Deaths per Million vs Total Cases by Country",
    x = "Total Cases",
    y = "Deaths per Million"
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)
