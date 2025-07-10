#' Fetch FluSight ILI target data
#' Obtain influenza like illness from Fluview
library(dplyr)
library(lubridate)
library(epidatr)
nat_ili <- pub_fluview(regions = "nat", epiweeks = epirange(201542, 202009))

locations <- c("nat", "hhs1", "hhs2", "hhs3", "hhs4", "hhs5", "hhs6", "hhs7", "hhs8", "hhs9", "hhs10")
ili_target_data_raw <- locations |>
  purrr::map(pub_fluview, epiweeks = epirange(201542, 202009)) |>
  purrr::list_rbind()

# raw target data
write.csv(ili_target_data_raw, "target-data/target-data-raw.csv", row.names = FALSE)

# time series format
ili_time_series <- ili_target_data_raw |>
  select("issue", "region", "epiweek", "wili") |>
  rename(as_of = "issue", location = "region", observation = "wili") |>
  mutate(
    epiweek = as.Date(epiweek),
    date = epiweek - 1,
    target = "ili perc",
    origin_epiweek = paste(year(epiweek), sprintf("%02d", epiweek(epiweek)), sep = "-"),
    .before = "epiweek"
  ) |>
  select(-"epiweek")

write.csv(ili_time_series, "target-data/time-series.csv", row.names = FALSE)

# time series format
cdf_levels <- data.frame(output_type_id = c(seq(0.1, 13, 0.1), 100))
ili_oracle_output_mean <- ili_time_series |>
  rename(origin_date = "date", oracle_value = "observation") |>
  mutate(
    output_type = "mean",
    output_type_id = NA,
    .before = "oracle_value"
  ) |>
  select(-"as_of")

ili_oracle_output <- ili_oracle_output_mean |>
  select(-"output_type_id") |>
  rename(observation = "oracle_value") |>
  cross_join(cdf_levels, copy = TRUE) |>
  mutate(
    output_type = "cdf",
    oracle_value = ifelse(output_type_id >= observation, 1, 0),
    .before = "observation"
  ) |>
  select("location", "origin_date", "target", "origin_epiweek", "output_type", "output_type_id", "oracle_value") |>
  rbind(ili_oracle_output)

write.csv(ili_oracle_output, "target-data/oracle-output.csv", row.names = FALSE)
