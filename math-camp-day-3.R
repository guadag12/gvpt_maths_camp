library(tidyverse)
library(here)
library(tidyverse)
library(skimr)
library(janitor)
library(countrycode)
rm(list = ls())
options(scipen = 999)

here::here()
here("data-raw")
here("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3403845.csv")
gdp_raw<- read_csv(here("data-raw", "API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3403845.csv"),
                    skip = 4, col_select = 1:66)
skimr::skim(gdp_raw)

gdp_df<- pivot_longer(gdp_raw, cols = `1960`:`2021`, names_to = "year", values_to = "gdp") |>
  janitor::clean_names() |>
  mutate(region = countrycode(country_name, "country.name", "region")) |>
  drop_na(region) |>
  relocate(region, .after = "country_code") |>
  transmute(country_name, country_code, region, year=as.integer(year), gdp)
glimpse(gdp_df)

ggplot(gdp_df, aes(x = year, y = gdp, color = region, group = country_name)) +
  geom_line() +
  theme_minimal()

gdp_df |> 
  group_by(region, year) |> 
  summarise(avg_gdp = mean(gdp, na.rm = TRUE)) |> 
  ggplot( aes(x = year, y = avg_gdp, color = region)) +
  geom_line() |> 
  theme_minimal()
