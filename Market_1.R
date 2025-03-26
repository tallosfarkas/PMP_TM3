###################################################################################################################


MM1 <- function(company, ann_date, ret = all_data, T1 = -1, mp = "Mkt_lr") {
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
  # (the logic behind T1 = -1 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 1
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 3)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind")
  
  # If fewer than 10 non-NA observations, return NA
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
    Company = rep(company, 3),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 3),
    Beta = rep(beta, 3),
    Residuals = rep(res, 3)
  )
  
  return(output)
}

MM1_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  mm1_result <- MM1(events_df$Ticker[i], events_df$Ann_Date[i])
  MM1_list <- rbind(
    MM1_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(mm1_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

### FF5 ###
FF1 <- function(company, ann_date, ret = all_data, T1 = -1, mp = c("Mkt_lr", "SMB_lr", "HML_lr", "RMW_lr", "CMA_lr")) {
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
  # (the logic behind T1 = -1 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 1
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 3)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind_1", "ind_2", "ind_3", "ind_4", "ind_5")
  
  # If fewer than 10 non-NA observations, return NA
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
    Company = rep(company, 3),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 3),
    Beta_Mkt = rep(beta, 3),
    Beta_SMB = rep(gamma, 3),
    Beta_HML = rep(delta, 3),
    Beta_RMW = rep(epsilon, 3),
    Beta_CMA = rep(zeta, 3),
    Residuals = rep(res, 3)
  )
  
  return(output)
}


FF1_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  ff1_result <- FF1(events_df$Ticker[i], events_df$Ann_Date[i])
  FF1_list <- rbind(
    FF1_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(ff1_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ",events_df$Ann_Date[i] ))
}

# Drop na's from the MM_list and FF_list
MM1_list <- MM1_list[!sapply(MM1_list$Output, function(x) is.null(x) || all(is.na(x))), ]
FF1_list <- FF1_list[!sapply(FF1_list$Output, function(x) is.null(x) || all(is.na(x))), ]


### Divide MM_list results based on News type
MM1_good <- MM1_list %>% filter(News == "good")
MM1_bad <- MM1_list %>% filter(News == "bad")
MM1_neutral <- MM1_list %>% filter(News == "no news")

FF1_good <- FF1_list %>% filter(News == "good")
FF1_bad <- FF1_list %>% filter(News == "bad")
FF1_neutral <- FF1_list %>% filter(News == "no news")

#MM_good is a dataframe containing dataframe in the 'Output' column, we need to unnest it and choose the first row from each one 
Total_MM1_Good <- data.frame(matrix(ncol = 2, nrow = 3, 0))
colnames(Total_MM1_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM1_good)){
  Total_MM1_Good <- Total_MM1_Good + MM1_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM1_Good <- Total_MM1_Good / nrow(MM1_good)

Total_MM1_Bad <- data.frame(matrix(ncol = 2, nrow = 3, 0))
colnames(Total_MM1_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM1_bad)){
  Total_MM1_Bad <- Total_MM1_Bad + MM1_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM1_Bad <- Total_MM1_Bad / nrow(MM1_bad)

Total_MM1_Neutral <- data.frame(matrix(ncol = 2, nrow = 3, 0))
colnames(Total_MM1_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM1_neutral)){
  Total_MM1_Neutral <- Total_MM1_Neutral + MM1_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM1_Neutral <- Total_MM1_Neutral / nrow(MM1_neutral)

# FF Data

Total_FF1_Good <- data.frame(matrix(ncol = 2, nrow = 3, 0))
colnames(Total_FF1_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF1_good)){
  Total_FF1_Good <- Total_FF1_Good + FF1_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF1_Good <- Total_FF1_Good / nrow(FF1_good)

Total_FF1_Bad <- data.frame(matrix(ncol = 2, nrow = 3, 0))
colnames(Total_FF1_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF1_bad)){
  Total_FF1_Bad <- Total_FF1_Bad + FF1_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF1_Bad <- Total_FF1_Bad / nrow(FF1_bad)

Total_FF1_Neutral <- data.frame(matrix(ncol = 2, nrow = 3, 0))
colnames(Total_FF1_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF1_neutral)){
  Total_FF1_Neutral <- Total_FF1_Neutral + FF1_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF1_Neutral <- Total_FF1_Neutral / nrow(FF1_neutral)


t_stats_CAR_FF1_Good <- t.test(Total_FF1_Good$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_FF1_Neutral <- t.test(Total_FF1_Neutral$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_FF1_Bad <- t.test(Total_FF1_Bad$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value

t_stats_CAR_MM1_Good <- t.test(Total_MM1_Good$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_MM1_Neutral <- t.test(Total_MM1_Neutral$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value
t_stats_CAR_MM1_Bad <- t.test(Total_MM1_Bad$Cumulative_Abnormal_Log_Returns, mu = 0, alternative = "two.sided")$p.value


#####################################################################################################################################
