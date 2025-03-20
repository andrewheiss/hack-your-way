library(tidyverse)

pols <- read_csv("data/fivethirtyeight_datasets/pols-month.csv") |> 
  # Force all of these dates to be on the first of the month, for joining purposes
  mutate(date = update(mon, day = 1), .before = 1) |> 
  # From August to December 1974 there are 2 GOP presidents---it seems to be double counting Nixon and Ford, since Ford was sworn in on August 9, 1974. So we'll just count it as one, since Nixon wasn't running an Avignon papacy or anything
  mutate(prez_gop = ifelse(prez_gop > 1, 1, prez_gop)) |> 
  select(-mon)

cpi <- read_csv("data/fivethirtyeight_datasets/cpi.csv") |> 
  select(date = DATE, cpi = VALUE)

snp <- read_csv("data/fivethirtyeight_datasets/snp.csv") |> 
  mutate(date = mdy(date)) |> 
  rename(snp = close) |>
  # Force all of these dates to be on the first of the month, for joining purposes
  mutate(date = update(date, day = 1)) |> 
  arrange(date)

unemployment <- read_csv("data/fivethirtyeight_datasets/unemployment.csv") |> 
  pivot_longer(cols = !Year, names_to = "month", values_to = "unemp") |> 
  mutate(date = ymd(glue::glue("{Year}-{month}-01"))) |> 
  select(date, unemp)

gdp <- read_csv("data/fivethirtyeight_datasets/GDP.csv") |> 
  mutate(gdp = as.numeric(VALUE)) |> 
  mutate(quarter = quarter(DATE)) |> 
  select(date = DATE, quarter, gdp)

recessions <- read_csv("data/fivethirtyeight_datasets/recessions.csv") |> 
  mutate(across(everything(), \(x) mdy(x)))

recessions_long <- recessions |>
  mutate(
    date = map2(
      start,
      end,
      \(start, end) seq(from = start, to = end, by = "month")
    )
  ) |>
  unnest(date) |> 
  arrange(date) |> 
  mutate(recession = TRUE) |> 
  select(date, recession)


# Joining time!
date_skeleton <- tibble(
  date = seq(from = min(pols$date), to = max(pols$date), by = "month")
)

full_data <- date_skeleton |> 
  # Deal with inflation data
  left_join(cpi, by = join_by(date)) |> 

  # Deal with GDP data and fill in all the months in the quarter
  left_join(gdp, by = join_by(date)) |> 
  group_by(year(date), quarter(date)) |> 
  fill(gdp, .direction = "downup") |> 
  ungroup() |> 
  select(-`year(date)`, -`quarter(date)`, -quarter) |> 

  # Deal with S&P data
  left_join(snp, by = join_by(date)) |> 

  # Deal with unemployment data
  left_join(unemployment, by = join_by(date)) |> 

  # Calculate percent changes, since that's what 538 is doing
  mutate(
    inflation_by_month = (cpi / lag(cpi) - 1) * 100,
    inflation_by_year = (cpi / lag(cpi, n = 12) - 1) * 100,
    stocks = (snp / lag(snp) - 1) * 100
  ) |> 

  # Add recessions
  left_join(recessions_long, by = join_by(date)) |> 
  replace_na(list(recession = FALSE)) |> 

  # Add political stuff
  left_join(pols, by = join_by(date)) |> 

  # Reverse some economic things so that higher values are bad
  mutate(across(c(unemp, inflation_by_month, inflation_by_year), \(x) -x)) |> 
  mutate(inflation = inflation_by_year)

write_csv(full_data, "data/538_p_hacking.csv")
