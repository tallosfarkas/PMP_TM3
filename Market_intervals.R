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

MM_t <- function(company, ann_date, ret = all_data, T1, mp = "Mkt_lr") {
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
  row_num_for_date <- which(all_data$date == ann_date) + T1
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, T1*2+1)
  
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
    Company = rep(company, T1*2+1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, T1*2+1),
    Beta = rep(beta, T1*2+1),
    Residuals = rep(res, T1*2+1)
  )
  
  return(output)
}

intervals <- c(10,5,2,1,0)

for (j in intervals) {
  name <- paste0('MM_list_', j)
  assign(name, data.frame(
    date = as.Date(character()),
    Company = character(),
    News = character(),
    Output = I(list())
  )
  )
}

for (i in 1:nrow(events_df)) {
  for (j in intervals) {
    interval_name <- paste0('MM_list_', j)
    
    mm_result <- MM_t(events_df$Ticker[i], events_df$Ann_Date[i], T1 = j)
    
    # Append result to the appropriate list object
    current_df <- get(interval_name)
    current_df <- rbind(
      current_df,
      data.frame(
        date = events_df$Ann_Date[i],
        Company = events_df$Ticker[i],
        News = events_df$news[i],
        Output = I(list(mm_result))
      )
    )
    
    assign(interval_name, current_df)
  }
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

#################################################################################


FF_t <- function(company, ann_date, ret = all_data, T1, mp = "Mkt_lr") {
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
  row_num_for_date <- which(all_data$date == ann_date) + T1
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, T1*2+1)
  
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
    Company = rep(company, T1*2+1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, T1*2+1),
    Beta = rep(beta, T1*2+1),
    Residuals = rep(res, T1*2+1)
  )
  
  return(output)
}

for (j in intervals) {
  name <- paste0('FF_list_', j)
  assign(name, data.frame(
    date = as.Date(character()),
    Company = character(),
    News = character(),
    Output = I(list())
  )
  )
}

for (i in 1:nrow(events_df)) {
  for (j in intervals) {
    interval_name <- paste0('FF_list_', j)
    
    FF_result <- FF_t(events_df$Ticker[i], events_df$Ann_Date[i], T1 = j)
    
    # Append result to the appropriate list object
    current_df <- get(interval_name)
    current_df <- rbind(
      current_df,
      data.frame(
        date = events_df$Ann_Date[i],
        Company = events_df$Ticker[i],
        News = events_df$news[i],
        Output = I(list(FF_result))
      )
    )
    
    assign(interval_name, current_df)
  }
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

MM_and_FF_lists <- list('MM_list_10', 'MM_list_5', 'MM_list_2', 'MM_list_1', 'MM_list_0', 'FF_list_10', 'FF_list_5', 'FF_list_2', 'FF_list_1', 'FF_list_0')

for ( i in MM_and_FF_lists ) {
  
  # Split i into the component of its string, split by '_'
  split_i <- strsplit(i, "_")[[1]]
  
  MM_or_FF <- split_i[1]
  interval_number <- as.numeric(split_i[3])
  
  # Get the "list" data frame
  list_active <- get(i)
  
  # Drop rows where Output is NULL or all NA
  list_active <- list_active[!sapply(list_active$Output, function(x) is.null(x) || all(is.na(x))), ]
  
  # Split into Good, Bad, Neutral
  states <- list("good", "bad", "no news")
  for (state in states) {
    if (state == "no news") {
      name_state <- "Neutral"
    } else if (state == "good") {
      name_state <- "Good"
    } else {
      name_state <- "Bad"
    }
    
    # e.g. "MM_10_Good"
    name_df <- paste0(MM_or_FF, "_", interval_number, "_", name_state)
    
    # Subset for each type of news
    df_subset <- subset(list_active, News == state)
    assign(name_df, df_subset)
  }
  
  # For each of the 3 subsets, create a summary "Total_*"
  three_active_lists <- list(
    paste0(MM_or_FF, "_", interval_number, "_Good"),
    paste0(MM_or_FF, "_", interval_number, "_Bad"),
    paste0(MM_or_FF, "_", interval_number, "_Neutral")
  )
  
  for (k in three_active_lists) {
    
    # E.g. "Total_MM_10_Good"
    name_total <- paste0("Total_", k)
    
    # Create a zero-filled data frame of size (2 columns, 2*interval_number+1 rows)
    # to accumulate AR & CAR
    df_accumulator <- data.frame(matrix(
      0, ncol = 2, nrow = interval_number * 2 + 1
    ))
    
    # Set columns
    colnames(df_accumulator) <- c(
      "Abnormal_Log_Return", 
      "Cumulative_Abnormal_Log_Returns"
    )
    
    # Grab the subset data
    k_active <- get(k)
    
    # If no rows, store the zero-filled df immediately
    if (nrow(k_active) == 0) {
      assign(name_total, df_accumulator)
      next
    }
    
    # Summation: add each row's Output data frame
    for (p in seq_len(nrow(k_active))) {
      out_df <- k_active$Output[[p]]
      
      # If out_df is valid and has the needed columns, add them in
      if (is.data.frame(out_df) && 
          all(c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns") %in% names(out_df))) {
        df_accumulator <- df_accumulator + out_df[, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
      }
    }
    
    # Average by the number of rows
    df_accumulator <- df_accumulator / nrow(k_active)
    
    # Finally, assign the finished data frame back to the environment
    assign(name_total, df_accumulator)
  }
}


for ( i in MM_and_FF_lists ) {
  
  # Split i into the component of its string, split by '_'
  split_i <- strsplit(i, "_")[[1]]
  
  MM_or_FF <- split_i[1]
  interval_number <- as.numeric(split_i[3])
  
  list_active <- get(i)
  
  # Drop na's
  list_active <- list_active[!sapply(list_active$Output, function(x) is.null(x) || all(is.na(x))), ]
  
  states <- list("good", "bad", "no news")
  for (state in states) {
    if (state == 'no news') {
      name_state <- 'Neutral'
    } else if (state == 'good') {
      name_state <- 'Good'
    } else {
      name_state <- 'Bad'
    }
    name <- paste0(MM_or_FF, '_', interval_number, '_', name_state)
    assign(name, list_active %>% filter(News == state))
  }
  
  three_active_lists <- list(paste0(MM_or_FF, '_', interval_number, '_Good'), paste0(MM_or_FF, '_', interval_number, '_Bad'), paste0(MM_or_FF, '_', interval_number, '_Neutral'))
  
  for (k in three_active_lists) {
    name <- paste0('Total_', k)
    assign(name, data.frame(matrix(ncol = 2, nrow = interval_number * 2 + 1, 0)))
    
    k_active <- get(k)
    name_active <- get(name)
    
    colnames(name_active) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
    
    for (p in 1:nrow(k_active)) {
      name_active <- name_active + k_active$Output[[p]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
    }
    
    name_active <- name_active / nrow(k_active)
  }
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
}

