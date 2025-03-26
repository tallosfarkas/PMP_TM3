library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(frenchdata)

FF_daily <- download_french_data("Fama/French 5 Factors (2x3) [Daily]")$subsets[, 2]$data[[1]] %>% 
  mutate(date = ymd(date)) %>% 
  filter(between(date, ymd("1995-01-31"), ymd("2025-03-14")))
summary(FF_daily)

# Estimate the returns of the assets using the Market Returns, that is the "Mkt-RF" column 
# in FF_daily, using a rolling window of 250. The data we use is tsr, which is the time series for the 30 assets.

returns_df <- tsr %>% 
  mutate(date = ymd(date)) %>% 
  filter(between(date, ymd("1995-01-31"), ymd("2025-03-14")))

# Keep all columns with 'lr' in the name
returns_df <- returns_df %>% select(date, matches("lr"))

ff_df <- FF_daily %>% mutate(date = as.Date(date))
ff_df$Mkt <- ff_df$'Mkt-RF' + ff_df$RF
ff_df[2:ncol(ff_df)] <- ff_df[2:ncol(ff_df)] / 100
str(ff_df)
# turn normal returns in ff_df$Mkt to log returns
ff_df$Mkt_lr <- log(1 + ff_df$Mkt)
ff_df$RF_lr <- log(1 + ff_df$RF)
ff_df$SMB_lr <- log(1 + ff_df$SMB)
ff_df$HML_lr <- log(1 + ff_df$HML)
ff_df$RMW_lr <- log(1 + ff_df$RMW)
ff_df$CMA_lr <- log(1 + ff_df$CMA)

events_df  <- events_clean  %>% mutate(event_date = as.Date(Ann_Date)) %>% filter(Ann_Date <= as.Date('2024-12-15'))

# Merge returns + market factor once
all_data <- left_join(returns_df, ff_df, by = "date") %>% filter(date <= as.Date('2024-12-31'))

MM <- function(company, ann_date, ret = all_data, T1 = -10, mp = "Mkt_lr") {
  # The column name for the company is "<company>_lr"
  company_lr <- paste0(gsub(" ", "_", company), "_lr")
  
  # 1) Filter rows up to ann_date
  # 2) Select only date, company_lr, and mp columns
  # 3) Sort by date
  sub <- ret %>%
    filter(date <= ann_date) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  # Keep the last (250 - T1) rows, then keep the first 250 rows of that
  # (the logic behind T1 = -10 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 10
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 21)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind")
  
  # If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # Run OLS: dep ~ ind
  model <- lm(dep ~ ind, data = sub)
  res <- var(residuals(model))
  alpha = coef(model)[1]
  beta = coef(model)[2]
  
  # Estimate the returns
  estimate = alpha + beta*sub_event_window[[mp]]

  abnormal_returns <- sub_event_window[[company_lr]] - estimate

  cum_abnormal_returns <- cumsum(abnormal_returns)
  
  # Create a Data Frame with the Dates, Abnormal Returns,Actual Returns, Estimated Returns, Alpha, Beta, and Residuals
  output <- data.frame(
    Date = sub_event_window$date,
    Company = rep(company, 21),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 21),
    Beta = rep(beta, 21),
    Residuals = rep(res, 21)
  )
  
  return(output)
}

MM_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  mm_result <- MM(events_df$Ticker[i], events_df$Ann_Date[i])
  MM_list <- rbind(
    MM_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
        Output = I(list(mm_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

### FF5 ###
FF <- function(company, ann_date, ret = all_data, T1 = -10, mp = c("Mkt_lr", "SMB_lr", "HML_lr", "RMW_lr", "CMA_lr")) {
  # The column name for the company is "<company>_lr"
  company_lr <- paste0(gsub(" ", "_", company), "_lr")
  
  # 1) Filter rows up to ann_date
  # 2) Select only date, company_lr, and mp columns
  # 3) Sort by date
  sub <- ret %>%
    filter(date <= ann_date) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  # Keep the last (250 - T1) rows, then keep the first 250 rows of that
  # (the logic behind T1 = -10 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 10
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 21)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind_1", "ind_2", "ind_3", "ind_4", "ind_5")
  
  # If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # Run OLS: dep ~ ind
  model <- lm(dep ~ . - date - dep, data = sub)
  res <- var(residuals(model))
  alpha = coef(model)[1]
  beta = coef(model)[2]
  gamma = coef(model)[3]
  delta = coef(model)[4]
  epsilon = coef(model)[5]
  zeta = coef(model)[6]
  
  # Estimate the returns
  estimate = alpha + beta*sub_event_window[['Mkt_lr']] + gamma*sub_event_window[['SMB_lr']] + delta*sub_event_window[['HML_lr']] + epsilon*sub_event_window[['RMW_lr']] + zeta*sub_event_window[['CMA_lr']]
  
  abnormal_returns <- sub_event_window[[company_lr]] - estimate
  
  cum_abnormal_returns <- cumsum(abnormal_returns)
  
  # Create a Data Frame with the Dates, Abnormal Returns,Actual Returns, Estimated Returns, Alpha, Beta, and Residuals
  output <- data.frame(
    Date = sub_event_window$date,
    Company = rep(company, 21),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 21),
    Beta_Mkt = rep(beta, 21),
    Beta_SMB = rep(gamma, 21),
    Beta_HML = rep(delta, 21),
    Beta_RMW = rep(epsilon, 21),
    Beta_CMA = rep(zeta, 21),
    Residuals = rep(res, 21)
  )
  
  return(output)
}


FF_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  ff_result <- FF(events_df$Ticker[i], events_df$Ann_Date[i])
  FF_list <- rbind(
    FF_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(ff_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ",events_df$Ann_Date[i] ))
}

# Drop na's from the MM_list and FF_list
MM_list <- MM_list[!sapply(MM_list$Output, function(x) is.null(x) || all(is.na(x))), ]
FF_list <- FF_list[!sapply(FF_list$Output, function(x) is.null(x) || all(is.na(x))), ]


### Divide MM_list results based on News type
MM_good <- MM_list %>% filter(News == "good")
MM_bad <- MM_list %>% filter(News == "bad")
MM_neutral <- MM_list %>% filter(News == "no news")

FF_good <- FF_list %>% filter(News == "good")
FF_bad <- FF_list %>% filter(News == "bad")
FF_neutral <- FF_list %>% filter(News == "no news")

#MM_good is a dataframe containing dataframe in the 'Output' column, we need to unnest it and choose the first row from each one 
Total_MM_Good <- data.frame(matrix(ncol = 2, nrow = 21, 0))
colnames(Total_MM_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM_good)){
  Total_MM_Good <- Total_MM_Good + MM_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM_Good <- Total_MM_Good / nrow(MM_good)

Total_MM_Bad <- data.frame(matrix(ncol = 2, nrow = 21, 0))
colnames(Total_MM_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM_bad)){
  Total_MM_Bad <- Total_MM_Bad + MM_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM_Bad <- Total_MM_Bad / nrow(MM_bad)

Total_MM_Neutral <- data.frame(matrix(ncol = 2, nrow = 21, 0))
colnames(Total_MM_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM_neutral)){
  Total_MM_Neutral <- Total_MM_Neutral + MM_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM_Neutral <- Total_MM_Neutral / nrow(MM_neutral)

# FF Data

Total_FF_Good <- data.frame(matrix(ncol = 2, nrow = 21, 0))
colnames(Total_FF_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF_good)){
  Total_FF_Good <- Total_FF_Good + FF_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF_Good <- Total_FF_Good / nrow(FF_good)

Total_FF_Bad <- data.frame(matrix(ncol = 2, nrow = 21, 0))
colnames(Total_FF_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF_bad)){
  Total_FF_Bad <- Total_FF_Bad + FF_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF_Bad <- Total_FF_Bad / nrow(FF_bad)

Total_FF_Neutral <- data.frame(matrix(ncol = 2, nrow = 21, 0))
colnames(Total_FF_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF_neutral)){
  Total_FF_Neutral <- Total_FF_Neutral + FF_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF_Neutral <- Total_FF_Neutral / nrow(FF_neutral)


# Combine MM Results
Summary_MM <- data.frame(
  Event_Day = -10:10,
  AR_Good = Total_MM_Good$Abnormal_Log_Return * 100,
  CAR_Good = Total_MM_Good$Cumulative_Abnormal_Log_Returns* 100,
  AR_Neutral = Total_MM_Neutral$Abnormal_Log_Return* 100,
  CAR_Neutral = Total_MM_Neutral$Cumulative_Abnormal_Log_Returns* 100,
  AR_Bad = Total_MM_Bad$Abnormal_Log_Return* 100,
  CAR_Bad = Total_MM_Bad$Cumulative_Abnormal_Log_Returns* 100
)

# Combine FF Results
Summary_FF <- data.frame(
  Event_Day = -10:10,
  AR_Good = Total_FF_Good$Abnormal_Log_Return* 100,
  CAR_Good = Total_FF_Good$Cumulative_Abnormal_Log_Returns* 100,
  AR_Neutral = Total_FF_Neutral$Abnormal_Log_Return* 100,
  CAR_Neutral = Total_FF_Neutral$Cumulative_Abnormal_Log_Returns* 100,
  AR_Bad = Total_FF_Bad$Abnormal_Log_Return* 100,
  CAR_Bad = Total_FF_Bad$Cumulative_Abnormal_Log_Returns* 100
)


library(ggplot2)
library(dplyr)
library(tidyr)


ggplot(Summary_MM, aes(x = Event_Day)) +
  geom_line(aes(y = CAR_Good, color = "Good News"), size = 1) +
  geom_line(aes(y = CAR_Neutral, color = "Neutral News"), size = 1, linetype = "dashed") +
  geom_line(aes(y = CAR_Bad, color = "Bad News"), size = 1, linetype = "dotted") +
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


ggplot(Summary_FF, aes(x = Event_Day)) +
  geom_line(aes(y = CAR_Good, color = "Good News"), size = 1) +
  geom_line(aes(y = CAR_Neutral, color = "Neutral News"), size = 1, linetype = "dashed") +
  geom_line(aes(y = CAR_Bad, color = "Bad News"), size = 1, linetype = "dotted") +
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

#### MM windows for different levels ####

MM <- function(company, ann_date, ret = all_data, T1 = -10, mp = "Mkt_lr") {
  # The column name for the company is "<company>_lr"
  company_lr <- paste0(gsub(" ", "_", company), "_lr")
  
  # 1) Filter rows up to ann_date
  # 2) Select only date, company_lr, and mp columns
  # 3) Sort by date
  sub <- ret %>%
    filter(date <= ann_date) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  # Keep the last (250 - T1) rows, then keep the first 250 rows of that
  # (the logic behind T1 = -10 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 10
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 21)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind")
  
  # If fewer than 100 non-NA observations, return NA
  if (length(na.omit(sub$dep)) < 100) {
    return(NA)
  }
  
  # Run OLS: dep ~ ind
  model <- lm(dep ~ ind, data = sub)
  res <- var(residuals(model))
  alpha = coef(model)[1]
  beta = coef(model)[2]
  
  # Estimate the returns
  estimate = alpha + beta*sub_event_window[[mp]]

  abnormal_returns <- sub_event_window[[company_lr]] - estimate

  cum_abnormal_returns <- cumsum(abnormal_returns)
  
  # Create a Data Frame with the Dates, Abnormal Returns,Actual Returns, Estimated Returns, Alpha, Beta, and Residuals
  output <- data.frame(
    Date = sub_event_window$date,
    Company = rep(company, 21),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 21),
    Beta = rep(beta, 21),
    Residuals = rep(res, 21)
  )
  
  return(output)
}

MM_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  mm_result <- MM(events_df$Ticker[i], events_df$Ann_Date[i])
  MM_list <- rbind(
    MM_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(mm_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}
