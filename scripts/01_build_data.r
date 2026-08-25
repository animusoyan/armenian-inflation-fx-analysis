# ----------------------------------
# 1. CPI: cpi_armenia.csv processing
# ----------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

month_abbr <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sept","Oct","Nov","Dec")

# read CPI and coerce month columns to numeric
cpi_raw <- read_csv("data/cpi_armenia.csv", skip = 1, show_col_types = FALSE) %>%
  mutate(across(Jan:Dec, as.numeric))

cpi_long <- cpi_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to  = "month",
    values_to = "cpi_index"
  ) %>%
  mutate(
    year = as.integer(Years),
    month_num = match(month, month_abbr),
    date = ymd(sprintf("%d-%02d-01", year, month_num)),
    infl_pct = cpi_index - 100
  ) %>%
  arrange(date) %>%
  mutate(
    infl_lag = lag(infl_pct)
  ) %>%
  filter(!is.na(infl_lag)) %>%
  select(date, month_num, infl_pct, infl_lag)

head(cpi_long)

# ==========================
# Step 2: FX series
# ==========================

# USD
fx_usd_raw <- read_csv("data/fx_usd_amd.csv", show_col_types = FALSE) %>%
  mutate(across(Jan:Dec, as.numeric))

fx_usd_long <- fx_usd_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "usd_rate"
  ) %>%
  mutate(
    year = readr::parse_number(as.character(Years)),
    month_num = match(month, month_abbr),
    date = ymd(sprintf("%d-%02d-01", year, month_num))
  ) %>%
  arrange(date) %>%
  mutate(
    dlog_usd = 100 * (log(usd_rate) - lag(log(usd_rate)))
  ) %>%
  filter(!is.na(dlog_usd)) %>%
  select(date, dlog_usd)

# EUR
fx_eur_raw <- read_csv("data/fx_eur_amd.csv", show_col_types = FALSE) %>%
  mutate(across(Jan:Dec, as.numeric))

fx_eur_long <- fx_eur_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "eur_rate"
  ) %>%
  mutate(
    year = readr::parse_number(as.character(Years)),
    month_num = match(month, month_abbr),
    date = ymd(sprintf("%d-%02d-01", year, month_num))
  ) %>%
  arrange(date) %>%
  mutate(
    dlog_eur = 100 * (log(eur_rate) - lag(log(eur_rate)))
  ) %>%
  filter(!is.na(dlog_eur)) %>%
  select(date, dlog_eur)

# RUB
fx_rub_raw <- read_csv("data/fx_rub_amd.csv", show_col_types = FALSE) %>%
  mutate(across(Jan:Dec, as.numeric))

fx_rub_long <- fx_rub_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "rub_rate"
  ) %>%
  mutate(
    year = readr::parse_number(as.character(Years)),
    month_num = match(month, month_abbr),
    date = ymd(sprintf("%d-%02d-01", year, month_num))
  ) %>%
  arrange(date) %>%
  mutate(
    dlog_rub = 100 * (log(rub_rate) - lag(log(rub_rate)))
  ) %>%
  filter(!is.na(dlog_rub)) %>%
  select(date, dlog_rub)

# ==========================
# Step 3: merge CPI + FX
# ==========================

data_all <- cpi_long %>%
  inner_join(fx_usd_long, by = "date") %>%
  inner_join(fx_eur_long, by = "date") %>%
  inner_join(fx_rub_long, by = "date") %>%
  arrange(date)

nrow(data_all)
range(data_all$date)
head(data_all)

write_csv(data_all, "data/data_all_cpi_fx.csv")
print(nrow(data_all))
print(range(data_all$date))
print(head(data_all))

write_csv(data_all, "data/data_all_cpi_fx.csv")

