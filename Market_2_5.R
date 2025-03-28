###################################################################################################################


MM2_5 <- function(company, ann_date, ret = all_data, T1 = -2, mp = "Mkt_lr") {
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
  row_num_for_date <- which(all_data$date == ann_date) + 2
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 8)
  
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
    Company = rep(company, 8),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 8),
    Beta = rep(beta, 8),
    Residuals = rep(res, 8)
  )
  
  return(output)
}

MM2_5_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  mm2_5_result <- MM2_5(events_df$Ticker[i], events_df$Ann_Date[i])
  MM2_5_list <- rbind(
    MM2_5_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(mm2_5_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

### FF5 ###
FF2_5 <- function(company, ann_date, ret = all_data, T1 = -2, mp = c("Mkt_lr", "SMB_lr", "HML_lr", "RMW_lr", "CMA_lr")) {
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
  row_num_for_date <- which(all_data$date == ann_date) + 2
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 8)
  
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
    Company = rep(company, 8),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 8),
    Beta_Mkt = rep(beta, 8),
    Beta_SMB = rep(gamma, 8),
    Beta_HML = rep(delta, 8),
    Beta_RMW = rep(epsilon, 8),
    Beta_CMA = rep(zeta, 8),
    Residuals = rep(res, 8)
  )
  
  return(output)
}


FF2_5_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  ff2_5_result <- FF2_5(events_df$Ticker[i], events_df$Ann_Date[i])
  FF2_5_list <- rbind(
    FF2_5_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(ff2_5_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ",events_df$Ann_Date[i] ))
}

# Drop na's from the MM_list and FF_list
MM2_5_list <- MM2_5_list[!sapply(MM2_5_list$Output, function(x) is.null(x) || all(is.na(x))), ]
FF2_5_list <- FF2_5_list[!sapply(FF2_5_list$Output, function(x) is.null(x) || all(is.na(x))), ]


### Divide MM_list results based on News type
MM2_5_good <- MM2_5_list %>% filter(News == "good")
MM2_5_bad <- MM2_5_list %>% filter(News == "bad")
MM2_5_neutral <- MM2_5_list %>% filter(News == "no news")

FF2_5_good <- FF2_5_list %>% filter(News == "good")
FF2_5_bad <- FF2_5_list %>% filter(News == "bad")
FF2_5_neutral <- FF2_5_list %>% filter(News == "no news")

#MM_good is a dataframe containing dataframe in the 'Output' column, we need to unnest it and choose the first row from each one 
Total_MM2_5_Good <- data.frame(matrix(ncol = 2, nrow = 8, 0))
colnames(Total_MM2_5_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM2_5_good)){
  Total_MM2_5_Good <- Total_MM2_5_Good + MM2_5_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM2_5_Good <- Total_MM2_5_Good / nrow(MM2_5_good)

Total_MM2_5_Bad <- data.frame(matrix(ncol = 2, nrow = 8, 0))
colnames(Total_MM2_5_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM2_5_bad)){
  Total_MM2_5_Bad <- Total_MM2_5_Bad + MM2_5_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM2_5_Bad <- Total_MM2_5_Bad / nrow(MM2_5_bad)

Total_MM2_5_Neutral <- data.frame(matrix(ncol = 2, nrow = 8, 0))
colnames(Total_MM2_5_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM2_5_neutral)){
  Total_MM2_5_Neutral <- Total_MM2_5_Neutral + MM2_5_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM2_5_Neutral <- Total_MM2_5_Neutral / nrow(MM2_5_neutral)

# FF Data

Total_FF2_5_Good <- data.frame(matrix(ncol = 2, nrow = 8, 0))
colnames(Total_FF2_5_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF2_5_good)){
  Total_FF2_5_Good <- Total_FF2_5_Good + FF2_5_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF2_5_Good <- Total_FF2_5_Good / nrow(FF2_5_good)

Total_FF2_5_Bad <- data.frame(matrix(ncol = 2, nrow = 8, 0))
colnames(Total_FF2_5_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF2_5_bad)){
  Total_FF2_5_Bad <- Total_FF2_5_Bad + FF2_5_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF2_5_Bad <- Total_FF2_5_Bad / nrow(FF2_5_bad)

Total_FF2_5_Neutral <- data.frame(matrix(ncol = 2, nrow = 8, 0))
colnames(Total_FF2_5_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF2_5_neutral)){
  Total_FF2_5_Neutral <- Total_FF2_5_Neutral + FF2_5_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF2_5_Neutral <- Total_FF2_5_Neutral / nrow(FF2_5_neutral)


t_stats_CAR_FF2_5_Good <- t.test(Total_FF2_5_Good$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_FF2_5_Neutral <- t.test(Total_FF2_5_Neutral$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_FF2_5_Bad <- t.test(Total_FF2_5_Bad$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value

t_stats_CAR_MM2_5_Good <- t.test(Total_MM2_5_Good$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_MM2_5_Neutral <- t.test(Total_MM2_5_Neutral$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_MM2_5_Bad <- t.test(Total_MM2_5_Bad$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value


#####################################################################################################################################
