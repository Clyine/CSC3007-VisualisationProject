## -----------------------------------------------------------------------------
#| label: library
#| message: false

library(tidyverse)
library(knitr)
library(readxl)
library(zoo)


## -----------------------------------------------------------------------------
#| label: fig-wsj
#| echo: false
#| fig.cap: "Visualization of measles incidence in the United States from
#|   1928 to 2013 by @debold_battling_2015."

include_graphics("images/wsj.png")


## -----------------------------------------------------------------------------
#| label: input-data
#| message: false

measles <-
  "US.14189004.csv" |>
  read_csv() |>
  filter(is.na(Admin2Name), PartOfCumulativeCountSeries == 0) |>
  select(Admin1Name, Admin1ISO, PeriodStartDate, PeriodEndDate, CountValue)
measles


## -----------------------------------------------------------------------------
#| label: period-length

periods_in_days <-
  measles |>
  mutate(PeriodLength = PeriodEndDate - PeriodStartDate) |>
  distinct(PeriodLength) |>
  pull()
all(periods_in_days == 6)


## -----------------------------------------------------------------------------
#| label: weekly-mean

measles <-
  measles |>
  mutate(year = year(PeriodStartDate)) |>
  summarize(
    mean_weekly_count = mean(CountValue, na.rm = TRUE),
    .by = c(Admin1Name, Admin1ISO, year)
  )
measles


## -----------------------------------------------------------------------------
#| label: state-census

census <-
  "state-census-counts-2020.xlsx" |>
  read_xlsx(skip = 1, n_max = 52) |>
  filter(State != "United States") |>
  mutate(State = str_to_upper(State))
census


## -----------------------------------------------------------------------------
#| label: population-interpolation

census <-
  census |>
  pivot_longer(
    -State,
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "population",
    values_transform = list(population = as.numeric)
  ) |>
  complete(State, year = min(year):max(year)) |>
  mutate(population = na.approx(population), .by = State)
census


## -----------------------------------------------------------------------------
#| label: incidence

measles <-
  measles |>
  left_join(census, by = c("Admin1Name" = "State", "year")) |>
  mutate(
    incidence = mean_weekly_count / population * 100000,
    state = str_sub(Admin1ISO, start = 4)
  ) |>
  filter(year >= 1928) # Start of WSJ x-axis
measles


## -----------------------------------------------------------------------------
#| label: fig-wsj-with-infotip
#| echo: false
#| fig-cap: Screenshot of the original plot with activated infotip.

include_graphics("images/wsj_with_infotip.png")

