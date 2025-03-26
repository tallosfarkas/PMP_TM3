###############################################################################
# 0. Load libraries
###############################################################################
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2)
library(frenchdata)

###############################################################################
# 1. Download Fama-French daily data and subset the date range
###############################################################################
FF_daily <- download_french_data("Fama/French 5 Factors (2x3) [Daily]")$subsets[, 2]$data[[1]] %>%
  mutate(date = ymd(date)) %>%
  filter(between(date, ymd("1995-01-31"), ymd("2025-03-14")))

summary(FF_daily)  # Just to check

###############################################################################
# 2. Prepare your asset returns data (tsr) in the same date range
###############################################################################
# Make sure 'tsr' has columns:
#   - date (YYYY-MM-DD)
#   - [Ticker]_lr for each asset's log return

returns_df <- tsr %>%
  mutate(date = ymd(date)) %>%
  filter(between(date, ymd("1995-01-31"), ymd("2025-03-14")))

# Keep date plus columns with "lr"
returns_df <- returns_df %>%
  select(date, matches("lr"))

###############################################################################
# 3. Convert Fama-French to log returns
###############################################################################
ff_df <- FF_daily %>%
  mutate(date = as.Date(date))

# Market returns = (Mkt-RF + RF), then convert everything to decimal
ff_df$Mkt <- ff_df$`Mkt-RF` + ff_df$RF
ff_df[2:ncol(ff_df)] <- ff_df[2:ncol(ff_df)] / 100

# Convert to log returns
ff_df$Mkt_lr <- log(1 + ff_df$Mkt)
ff_df$RF_lr  <- log(1 + ff_df$RF)
ff_df$SMB_lr <- log(1 + ff_df$SMB)
ff_df$HML_lr <- log(1 + ff_df$HML)
ff_df$RMW_lr <- log(1 + ff_df$RMW)
ff_df$CMA_lr <- log(1 + ff_df$CMA)

str(ff_df)

###############################################################################
# 4. Events data (from events_clean) - Ticker, news, Ann_Date
###############################################################################
events_df <- events_clean %>%
  mutate(event_date = as.Date(Ann_Date)) %>%
  filter(Ann_Date <= as.Date('2024-12-15'))

###############################################################################
# 5. Merge returns + FF factors into one dataset
###############################################################################
all_data <- left_join(returns_df, ff_df, by = "date") %>%
  filter(date <= as.Date('2024-12-31'))

###############################################################################
# 6. Market Model function (MM_t) with parameter T1
###############################################################################
MM_t <- function(company, ann_date, ret = all_data, T1, mp = "Mkt_lr") {
  
  # 1) Identify the log-return column for this company
  company_lr <- paste0(gsub(" ", "_", company), "_lr")
  
  # 2) Regression estimation window: the 250 days prior to ann_date
  #    We keep the last (250 - T1) rows, then keep the first 250 of that subset
  #    This effectively handles "shifting" if T1 != 10
  sub <- ret %>%
    filter(date <= ann_date) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # 3) Determine event window (T1 days after announcement)
  row_num_for_date  <- which(all_data$date == ann_date) + T1
  ann_date_plus_t1  <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>%
    filter(date <= ann_date_plus_t1) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  sub_event_window <- tail(sub_event_window, T1*2 + 1)
  
  # 4) Rename columns for simpler formula
  colnames(sub) <- c("date", "dep", "ind")
  
  # 5) If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # 6) OLS
  model <- lm(dep ~ ind, data = sub)
  res   <- var(residuals(model))
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  
  # 7) Estimate normal returns, compute abnormal returns
  estimate <- alpha + beta * sub_event_window[[mp]]
  abnormal_returns      <- sub_event_window[[company_lr]] - estimate
  cum_abnormal_returns  <- cumsum(abnormal_returns)
  
  # 8) Output
  output <- data.frame(
    Date = sub_event_window$date,
    Company = rep(company, T1*2 + 1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, T1*2 + 1),
    Beta = rep(beta, T1*2 + 1),
    Residuals = rep(res, T1*2 + 1)
  )
  
  return(output)
}

###############################################################################
# 7. Fama-French function (FF_t) with parameter T1 (1 factor or more)
###############################################################################
# For simplicity, the code below uses mp = "Mkt_lr" by default,
# but you can modify to handle multiple factor columns if desired.
###############################################################################
FF_t <- function(company, ann_date, ret = all_data, T1, mp = c("Mkt_lr", "SMB_lr", "HML_lr", "RMW_lr", "CMA_lr")) {
  
  company_lr <- paste0(gsub(" ", "_", company), "_lr")
  
  # 1) Grab the 250 days up to ann_date
  sub <- ret %>%
    filter(date <= ann_date) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # 2) Event window
  row_num_for_date <- which(all_data$date == ann_date) + T1
  ann_date_plus_t1 <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>%
    filter(date <= ann_date_plus_t1) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  sub_event_window <- tail(sub_event_window, T1*2 + 1)
  
  # 3) Rename columns
  colnames(sub) <- c("date", "dep", "ind_1", "ind_2", "ind_3", "ind_4", "ind_5")
  
  # 4) If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # 5) OLS: dep ~ ind
  model <- lm(dep ~ . - date - dep, data = sub)
  res   <- var(residuals(model))
  alpha <- coef(model)[1]
  beta  <- coef(model)[2]
  gamma = coef(model)[3]
  delta = coef(model)[4]
  epsilon = coef(model)[5]
  zeta = coef(model)[6]
  
  # 6) Estimate normal returns
  estimate = alpha + beta*sub_event_window[['Mkt_lr']] + gamma*sub_event_window[['SMB_lr']] + delta*sub_event_window[['HML_lr']] + epsilon*sub_event_window[['RMW_lr']] + zeta*sub_event_window[['CMA_lr']]
  
  # 7) Abnormal + Cumulative abnormal returns
  abnormal_returns      <- sub_event_window[[company_lr]] - estimate
  cum_abnormal_returns  <- cumsum(abnormal_returns)
  
  # 8) Output
  output <- data.frame(
    Date = sub_event_window$date,
    Company = rep(company, T1*2 + 1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, T1*2 + 1),
    Beta_Mkt = rep(beta, T1*2 + 1),
    Beta_SMB = rep(gamma, T1*2 + 1),
    Beta_HML = rep(delta, T1*2 + 1),
    Beta_RMW = rep(epsilon, T1*2 + 1),
    Beta_CMA = rep(zeta, T1*2 + 1),
    Beta = rep(beta, T1*2 + 1),
    Residuals = rep(res, T1*2 + 1)
  )
  
  return(output)
}

###############################################################################
# 8. Run both models (MM, FF) across intervals
###############################################################################
intervals <- c(10, 5, 2, 1, 0)

# Initialize empty data frames for each approach + interval
for (j in intervals) {
  # Market Model
  assign(paste0("MM_list_", j),
         data.frame(date = as.Date(character()),
                    Company = character(),
                    News = character(),
                    Output = I(list())) )
  
  # Fama-French
  assign(paste0("FF_list_", j),
         data.frame(date = as.Date(character()),
                    Company = character(),
                    News = character(),
                    Output = I(list())) )
}

# Fill them with results
for (i in seq_len(nrow(events_df))) {
  # For each event, run each T1
  for (j in intervals) {
    
    # Market Model
    interval_name_MM <- paste0("MM_list_", j)
    mm_result <- MM_t(
      company  = events_df$Ticker[i],
      ann_date = events_df$Ann_Date[i],
      T1       = j
    )
    current_df_mm <- get(interval_name_MM)
    current_df_mm <- rbind(
      current_df_mm,
      data.frame(date = events_df$Ann_Date[i],
                 Company = events_df$Ticker[i],
                 News = events_df$news[i],
                 Output = I(list(mm_result)))
    )
    assign(interval_name_MM, current_df_mm)
    
    # Fama-French
    interval_name_FF <- paste0("FF_list_", j)
    ff_result <- FF_t(
      company  = events_df$Ticker[i],
      ann_date = events_df$Ann_Date[i],
      T1       = j
    )
    current_df_ff <- get(interval_name_FF)
    current_df_ff <- rbind(
      current_df_ff,
      data.frame(date = events_df$Ann_Date[i],
                 Company = events_df$Ticker[i],
                 News = events_df$news[i],
                 Output = I(list(ff_result)))
    )
    assign(interval_name_FF, current_df_ff)
    
  }
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

###############################################################################
# 9. Drop rows where Output is empty or all NA (invalid regression)
###############################################################################
MM_and_FF_lists <- c(
  'MM_list_10', 'MM_list_5', 'MM_list_2', 'MM_list_1', 'MM_list_0',
  'FF_list_10', 'FF_list_5', 'FF_list_2', 'FF_list_1', 'FF_list_0'
)

for (nm in MM_and_FF_lists) {
  df_temp <- get(nm)
  df_temp <- df_temp[!sapply(df_temp$Output, function(x) {
    is.null(x) || all(is.na(x))
  }), ]
  assign(nm, df_temp)
}

###############################################################################
# 10. Aggregate / Summarize results by event type ("good", "bad", "no news")
###############################################################################
# For each approach + interval, we:
#   1) split by "good", "bad", "no news"
#   2) sum (and then average) the abnormal and cumulative abnormal returns
#   3) store in data frames named like: Total_MM_10_Good, etc.
###############################################################################

for (nm in MM_and_FF_lists) {
  # Parse list name into approach + interval
  split_nm   <- strsplit(nm, "_")[[1]]
  approach   <- split_nm[1]              # e.g. "MM" or "FF"
  interval_n <- as.numeric(split_nm[3])  # e.g. 10,5,2,1,0
  
  # Retrieve the data frame
  list_active <- get(nm)
  
  # Possible news states
  states <- c("good", "bad", "no news")
  
  for (state in states) {
    if (state == "no news") {
      nice_state <- "Neutral"
    } else if (state == "good") {
      nice_state <- "Good"
    } else {
      nice_state <- "Bad"
    }
    
    # Subset for each type of news
    df_subset <- subset(list_active, News == state)
    
    # We'll store the final aggregator in "Total_MM_10_Good" style
    # i.e. "Total_<approach>_<interval>_<NiceState>"
    out_name <- paste0("Total_", approach, "_", interval_n, "_", nice_state)
    
    # Create zero-filled aggregator for abnormal + cumulative abnormal returns
    df_accumulator <- data.frame(
      Abnormal_Log_Return = numeric(interval_n*2 + 1),
      Cumulative_Abnormal_Log_Returns = numeric(interval_n*2 + 1)
    )
    
    # If we have no events of this type, keep it at zero
    if (nrow(df_subset) == 0) {
      assign(out_name, df_accumulator, envir = .GlobalEnv)
      next
    }
    
    # Sum over all events in this subset
    for (r in seq_len(nrow(df_subset))) {
      out_df <- df_subset$Output[[r]]
      if (is.data.frame(out_df) &&
          all(c("Abnormal_Log_Return","Cumulative_Abnormal_Log_Returns") %in% names(out_df))) {
        
        df_accumulator$Abnormal_Log_Return <-
          df_accumulator$Abnormal_Log_Return + out_df$Abnormal_Log_Return
        df_accumulator$Cumulative_Abnormal_Log_Returns <-
          df_accumulator$Cumulative_Abnormal_Log_Returns + out_df$Cumulative_Abnormal_Log_Returns
      }
    }
    
    # Average across the events
    df_accumulator <- df_accumulator / nrow(df_subset)
    
    # Store result in global environment
    assign(out_name, df_accumulator, envir = .GlobalEnv)
  }
}

##################################################################################

# Prepare a data frame to store the results
t_stats_results <- data.frame(
  Model          = character(),  # "MM" or "FF"
  Interval       = numeric(),    # 10, 5, 2, 1, 0
  NewsType       = character(),  # "Good", "Neutral", "Bad"
  p_value        = numeric(),    # p-value from the t-test
  Significant_5pct = logical(),  # TRUE if p < 0.05
  Significant_1pct = logical(),
  stringsAsFactors = FALSE
)

for (model in c("MM", "FF")) {
  for (iv in c(10, 5, 2, 1, 0)) {
    for (news_type in c("Good", "Neutral", "Bad")) {
      
      # Build the data frame name, e.g. "Total_MM_10_Good"
      df_name <- paste0("Total_", model, "_", iv, "_", news_type)
      
      # 1) Check if it exists in the global environment
      if (!exists(df_name, envir = .GlobalEnv)) {
        next  # skip if the object doesn't exist
      }
      
      # 2) Retrieve the data frame
      df_current <- get(df_name, envir = .GlobalEnv)
      
      # 3) Check if it has the column and enough observations
      if (!"Cumulative_Abnormal_Log_Returns" %in% names(df_current)) {
        next
      }
      
      car_vals <- df_current$Cumulative_Abnormal_Log_Returns
      
      # Skip if fewer than 2 non-NA values
      if (sum(!is.na(car_vals)) < 2) {
        cat("Skipping", df_name, "- not enough observations.\n")
        next
      }
      
      # 4) Perform the t-test
      t_out <- t.test(car_vals, mu = 0, alternative = "two.sided")
      pval <- t_out$p.value
      sig  <- pval < 0.05
      sig1  <- pval < 0.01
      # 5) Store the result
      t_stats_results <- rbind(
        t_stats_results,
        data.frame(
          Model           = model,
          Interval        = iv,
          NewsType        = news_type,
          p_value         = pval,
          Significant_5pct = sig,
          Significant_1pct = sig1,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

# View the result
t_stats_results




# Combine MM Results
Summary_MM10 <- data.frame(
  Event_Day = -10:10,
  AR_Good = Total_MM_10_Good$Abnormal_Log_Return * 100,
  CAR_Good = Total_MM_10_Good$Cumulative_Abnormal_Log_Returns* 100,
  AR_Neutral = Total_MM_10_Neutral$Abnormal_Log_Return* 100,
  CAR_Neutral = Total_MM_10_Neutral$Cumulative_Abnormal_Log_Returns* 100,
  AR_Bad = Total_MM_10_Bad$Abnormal_Log_Return* 100,
  CAR_Bad = Total_MM_10_Bad$Cumulative_Abnormal_Log_Returns* 100
)

write_xlsx(Summary_MM10, "Summary_MM10.xlsx")

# Combine FF Results
Summary_FF10 <- data.frame(
  Event_Day = -10:10,
  AR_Good = Total_FF_10_Good$Abnormal_Log_Return* 100,
  CAR_Good = Total_FF_10_Good$Cumulative_Abnormal_Log_Returns* 100,
  AR_Neutral = Total_FF_10_Neutral$Abnormal_Log_Return* 100,
  CAR_Neutral = Total_FF_10_Neutral$Cumulative_Abnormal_Log_Returns* 100,
  AR_Bad = Total_FF_10_Bad$Abnormal_Log_Return* 100,
  CAR_Bad = Total_FF_10_Bad$Cumulative_Abnormal_Log_Returns* 100
)

write_xlsx(Summary_FF10, "Summary_FF10.xlsx")



ggplot(Summary_MM10, aes(x = Event_Day)) +
  geom_line(aes(y = CAR_Good, color = "Good News"), linewidth = 1) +
  geom_line(aes(y = CAR_Neutral, color = "Neutral News"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = CAR_Bad, color = "Bad News"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Cumulative Abnormal Returns by News Category (Market Model)",
    x = "Event Day",
    y = "CAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )


ggplot(Summary_FF10, aes(x = Event_Day)) +
  geom_line(aes(y = CAR_Good, color = "Good News"), linewidth = 1) +
  geom_line(aes(y = CAR_Neutral, color = "Neutral News"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = CAR_Bad, color = "Bad News"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Cumulative Abnormal Returns by News Category (FF5 Model)",
    x = "Event Day",
    y = "CAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

library(writexl)
write_xlsx(t_stats_results, "t_stats.xlsx")
