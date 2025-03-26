#############################################
# 1. Load Stock Prices and Compute Log Returns
#############################################
library(dplyr)
library(tidyr)
library(lubridate)

# Load the RData file containing daily stock prices and indices
load("StockPrices.TM3.RData")

# Compute daily log returns for each equity (columns ending with "_Equity") and for the market indices.
tsr <- tsr %>%
  arrange(date) %>%
  mutate(across(ends_with("_Equity"), 
                ~ log(.x) - log(dplyr::lag(.x)), 
                .names = "{.col}_lr")) %>%
  mutate(DJI_lr = log(DJI_Index) - log(dplyr::lag(DJI_Index)),
         SPX_lr = log(SPX_Index) - log(dplyr::lag(SPX_Index))) %>%
  # Remove the first row that produces NA returns
  filter(!is.na(DJI_lr))

#############################################
# 2. Read and Clean the Earnings Events Data
#############################################
library(readxl)

events <- read_excel("HistoryEarningsTM3.xlsx") %>%
  rename(
    Ann_Date = `Ann Date`,
    Per_End  = `Per End`
  ) %>%
  mutate(
    Ann_Date = as.Date(Ann_Date, format = "%m/%d/%Y")
  ) %>%
  # Remove events with missing Reported or Estimate values
  filter(!is.na(Reported) & !is.na(Estimate)) %>%
  # Compute percentage difference and classify as "good", "bad", or "no news"
  mutate(
    Diff = (Reported - Estimate) / Estimate,
    news = case_when(
      Diff > 0.025  ~ "good",
      Diff < -0.025 ~ "bad",
      TRUE          ~ "no news"
    )
  ) %>%
  # Create a unique event id
  mutate(event_id = row_number())

#############################################
# 3. Prepare Returns Data in Long Format
#############################################
# Convert the wide returns data (for each equity) into a long format with columns:
# Date, Ticker, and ret (the log return).
ret_long <- tsr %>%
  select(date, ends_with("_Equity_lr")) %>%
  pivot_longer(
    cols = ends_with("_Equity_lr"),
    names_to = "Ticker",
    values_to = "ret"
  ) %>%
  # Clean up ticker names (e.g., "IBM_Equity_lr" -> "IBM")
  mutate(Ticker = sub("_Equity_lr", "", Ticker))

#############################################
# 4. Standardize Ticker Names in Events Data
#############################################
# Adjust ticker names so they match those in ret_long.
events_clean <- events %>%
  mutate(
    Ticker = gsub(" Equity", "", Ticker),  # Remove " Equity" substring
    Ticker = gsub(" ", "_", Ticker)          # Replace spaces with underscores
  )

#############################################
# 5. Merge Events with Returns and Define Windows
#############################################
# We define two windows:
# - Estimation window: days -250 to -11 (used for computing the expected return)
# - Event window: days -10 to +10 (for measuring abnormal returns)
# Here, we only use the event window for the final table.
library(fuzzyjoin)  # if needed for flexible joins

const_mean_data <- events_clean %>%
  left_join(ret_long, by = "Ticker", relationship = "many-to-many") %>%
  mutate(
    day_relative = as.numeric(date - Ann_Date),
    window_type = case_when(
      day_relative >= -250 & day_relative <= -11 ~ "estimation",
      day_relative >= -10  & day_relative <= 10   ~ "event",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(window_type))

#############################################
# 6. Compute Expected Returns and Abnormal Returns (AR)
#############################################
# For each event (grouped by Ticker and event_id), compute the expected return (mu_hat)
# using the estimation window. Then, for each day (both estimation and event window),
# compute the abnormal return AR = ret - mu_hat.
const_mean_AR <- const_mean_data %>%
  group_by(Ticker, event_id) %>%
  mutate(
    mu_hat = mean(ret[window_type == "estimation"], na.rm = TRUE),
    AR = ret - mu_hat
  ) %>%
  ungroup()

#############################################
# 7. Summarize Abnormal Returns for the Event Window (-10 to +10)
#############################################
# Filter only the event window rows, then compute the average abnormal return (AAR)
# for each event day and news category. Next, compute the cumulative abnormal return (CAR)
# for each news category across the event window.
const_mean_event_window <- const_mean_AR %>%
  filter(window_type == "event") %>%
  select(Ticker, event_id, date, day_relative, AR, news)

# Compute average AR per day and news category
CM_AR_summary <- const_mean_event_window %>%
  group_by(day_relative, news) %>%
  summarize(
    AAR = mean(AR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(day_relative)

# Compute cumulative abnormal returns (CAR) within each news category
CM_AR_summary <- CM_AR_summary %>%
  group_by(news) %>%
  mutate(CAAR = cumsum(AAR)) %>%
  ungroup()

#############################################
# 8. Pivot the Event Window Summary to a Wide Format Table
#############################################
# Create a final table with 21 rows (for days -10 to +10). Each row contains
# the average AR and cumulative AR (CAR) for each news category.
table_output <- CM_AR_summary %>%
  pivot_wider(
    id_cols = day_relative,
    names_from = news,
    values_from = c(AAR, CAAR)
  ) %>%
  arrange(day_relative) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  rename(Event_Day = day_relative)

# View the final table (21 rows: one for each event day from -10 to +10)
print(table_output)

#############################################
# 9. (Optional) Nicely Format the Table for Reporting
#############################################
library(knitr)
library(kableExtra)

table_output %>%
  kbl(caption = "Average Abnormal Returns (AAR) and Cumulative AR (CAR) by Event Day and News Category") %>%
  kable_styling(full_width = FALSE)
