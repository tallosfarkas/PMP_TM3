###################################################################################################################


MM5 <- function(company, ann_date, ret = all_data, T1 = -5, mp = "Mkt_lr") {
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
  row_num_for_date <- which(all_data$date == ann_date) + 5
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 11)
  
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
    Company = rep(company, 11),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 11),
    Beta = rep(beta, 11),
    Residuals = rep(res, 11)
  )
  
  return(output)
}

MM5_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  mm5_result <- MM5(events_df$Ticker[i], events_df$Ann_Date[i])
  MM5_list <- rbind(
    MM5_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(mm5_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

### FF5 ###
FF5 <- function(company, ann_date, ret = all_data, T1 = -5, mp = c("Mkt_lr", "SMB_lr", "HML_lr", "RMW_lr", "CMA_lr")) {
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
  row_num_for_date <- which(all_data$date == ann_date) + 5
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 11)
  
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
    Company = rep(company, 11),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 11),
    Beta_Mkt = rep(beta, 11),
    Beta_SMB = rep(gamma, 11),
    Beta_HML = rep(delta, 11),
    Beta_RMW = rep(epsilon, 11),
    Beta_CMA = rep(zeta, 11),
    Residuals = rep(res, 11)
  )
  
  return(output)
}


FF5_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  ff5_result <- FF5(events_df$Ticker[i], events_df$Ann_Date[i])
  FF5_list <- rbind(
    FF5_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(ff5_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ",events_df$Ann_Date[i] ))
}

# Drop na's from the MM_list and FF_list
MM5_list <- MM5_list[!sapply(MM5_list$Output, function(x) is.null(x) || all(is.na(x))), ]
FF5_list <- FF5_list[!sapply(FF5_list$Output, function(x) is.null(x) || all(is.na(x))), ]


### Divide MM_list results based on News type
MM5_good <- MM5_list %>% filter(News == "good")
MM5_bad <- MM5_list %>% filter(News == "bad")
MM5_neutral <- MM5_list %>% filter(News == "no news")

FF5_good <- FF5_list %>% filter(News == "good")
FF5_bad <- FF5_list %>% filter(News == "bad")
FF5_neutral <- FF5_list %>% filter(News == "no news")

#MM_good is a dataframe containing dataframe in the 'Output' column, we need to unnest it and choose the first row from each one 
Total_MM5_Good <- data.frame(matrix(ncol = 2, nrow = 11, 0))
colnames(Total_MM5_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM5_good)){
  Total_MM5_Good <- Total_MM5_Good + MM5_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM5_Good <- Total_MM5_Good / nrow(MM5_good)

Total_MM5_Bad <- data.frame(matrix(ncol = 2, nrow = 11, 0))
colnames(Total_MM5_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM5_bad)){
  Total_MM5_Bad <- Total_MM5_Bad + MM5_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM5_Bad <- Total_MM5_Bad / nrow(MM5_bad)

Total_MM5_Neutral <- data.frame(matrix(ncol = 2, nrow = 11, 0))
colnames(Total_MM5_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM5_neutral)){
  Total_MM5_Neutral <- Total_MM5_Neutral + MM5_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM5_Neutral <- Total_MM5_Neutral / nrow(MM5_neutral)

# FF Data

Total_FF5_Good <- data.frame(matrix(ncol = 2, nrow = 11, 0))
colnames(Total_FF5_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF5_good)){
  Total_FF5_Good <- Total_FF5_Good + FF5_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF5_Good <- Total_FF5_Good / nrow(FF5_good)

Total_FF5_Bad <- data.frame(matrix(ncol = 2, nrow = 11, 0))
colnames(Total_FF5_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF5_bad)){
  Total_FF5_Bad <- Total_FF5_Bad + FF5_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF5_Bad <- Total_FF5_Bad / nrow(FF5_bad)

Total_FF5_Neutral <- data.frame(matrix(ncol = 2, nrow = 11, 0))
colnames(Total_FF5_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF5_neutral)){
  Total_FF5_Neutral <- Total_FF5_Neutral + FF5_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF5_Neutral <- Total_FF5_Neutral / nrow(FF5_neutral)


t_stats_CAR_FF5_Good <- t.test(Total_FF5_Good$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_FF5_Neutral <- t.test(Total_FF5_Neutral$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_FF5_Bad <- t.test(Total_FF5_Bad$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value

t_stats_CAR_MM5_Good <- t.test(Total_MM5_Good$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_MM5_Neutral <- t.test(Total_MM5_Neutral$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_MM5_Bad <- t.test(Total_MM5_Bad$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value


#####################################################################################################################################
