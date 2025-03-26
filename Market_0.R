###################################################################################################################


MM0 <- function(company, ann_date, ret = all_data, T1 = -0, mp = "Mkt_lr") {
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
  # (the logic behind T1 = -0 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 0
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 1)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind")
  
  # If fewer than 00 non-NA observations, return NA
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
    Company = rep(company, 1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 1),
    Beta = rep(beta, 1),
    Residuals = rep(res, 1)
  )
  
  return(output)
}

MM0_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  mm0_result <- MM0(events_df$Ticker[i], events_df$Ann_Date[i])
  MM0_list <- rbind(
    MM0_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(mm0_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ", events_df$Ann_Date[i]))
}

### FF5 ###
FF0 <- function(company, ann_date, ret = all_data, T1 = -0, mp = c("Mkt_lr", "SMB_lr", "HML_lr", "RMW_lr", "CMA_lr")) {
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
  # (the logic behind T1 = -0 might be your "event" offset)
  sub <- tail(sub, 250 - T1)
  sub <- head(sub, 250)
  
  # Obtain the row number that corresponds to ann_date
  row_num_for_date <- which(all_data$date == ann_date) + 0
  
  ann_date_plus_ten <- ret$date[row_num_for_date]
  
  sub_event_window <- ret %>% 
    filter(date <= ann_date_plus_ten) %>%
    select(date, all_of(company_lr), all_of(mp)) %>%
    arrange(date)
  
  sub_event_window <- tail(sub_event_window, 1)
  
  # Rename columns for the regression
  colnames(sub) <- c("date", "dep", "ind_1", "ind_2", "ind_3", "ind_4", "ind_5")
  
  # If fewer than 00 non-NA observations, return NA
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
    Company = rep(company, 1),
    Abnormal_Log_Return = abnormal_returns,
    Cumulative_Abnormal_Log_Returns = cum_abnormal_returns,
    Actual_Log_Return = sub_event_window[[company_lr]],
    Estimated_Log_Return = estimate,
    Alpha = rep(alpha, 1),
    Beta_Mkt = rep(beta, 1),
    Beta_SMB = rep(gamma, 1),
    Beta_HML = rep(delta, 1),
    Beta_RMW = rep(epsilon, 1),
    Beta_CMA = rep(zeta, 1),
    Residuals = rep(res, 1)
  )
  
  return(output)
}


FF0_list <- data.frame(
  date = as.Date(character()),
  Company = character(),
  News = character(),
  Output = I(list())
)

for (i in 1:nrow(events_df)) {
  ff0_result <- FF0(events_df$Ticker[i], events_df$Ann_Date[i])
  FF0_list <- rbind(
    FF0_list,
    data.frame(
      date = events_df$Ann_Date[i],
      Company = events_df$Ticker[i],
      News = events_df$news[i],
      Output = I(list(ff0_result))
    )
  )
  
  print(paste0("Finished ", events_df$Ticker[i], " at ",events_df$Ann_Date[i] ))
}

# Drop na's from the MM_list and FF_list
MM0_list <- MM0_list[!sapply(MM0_list$Output, function(x) is.null(x) || all(is.na(x))), ]
FF0_list <- FF0_list[!sapply(FF0_list$Output, function(x) is.null(x) || all(is.na(x))), ]


### Divide MM_list results based on News type
MM0_good <- MM0_list %>% filter(News == "good")
MM0_bad <- MM0_list %>% filter(News == "bad")
MM0_neutral <- MM0_list %>% filter(News == "no news")

FF0_good <- FF0_list %>% filter(News == "good")
FF0_bad <- FF0_list %>% filter(News == "bad")
FF0_neutral <- FF0_list %>% filter(News == "no news")

#MM_good is a dataframe containing dataframe in the 'Output' column, we need to unnest it and choose the first row from each one 
Total_MM0_Good <- data.frame(matrix(ncol = 2, nrow = 1, 0))
colnames(Total_MM0_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM0_good)){
  Total_MM0_Good <- Total_MM0_Good + MM0_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM0_Good <- Total_MM0_Good / nrow(MM0_good)

Total_MM0_Bad <- data.frame(matrix(ncol = 2, nrow = 1, 0))
colnames(Total_MM0_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM0_bad)){
  Total_MM0_Bad <- Total_MM0_Bad + MM0_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM0_Bad <- Total_MM0_Bad / nrow(MM0_bad)

Total_MM0_Neutral <- data.frame(matrix(ncol = 2, nrow = 1, 0))
colnames(Total_MM0_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")
for (i in 1:nrow(MM0_neutral)){
  Total_MM0_Neutral <- Total_MM0_Neutral + MM0_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_MM0_Neutral <- Total_MM0_Neutral / nrow(MM0_neutral)

# FF Data

Total_FF0_Good <- data.frame(matrix(ncol = 2, nrow = 1, 0))
colnames(Total_FF0_Good) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF0_good)){
  Total_FF0_Good <- Total_FF0_Good + FF0_good$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF0_Good <- Total_FF0_Good / nrow(FF0_good)

Total_FF0_Bad <- data.frame(matrix(ncol = 2, nrow = 1, 0))
colnames(Total_FF0_Bad) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF0_bad)){
  Total_FF0_Bad <- Total_FF0_Bad + FF0_bad$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF0_Bad <- Total_FF0_Bad / nrow(FF0_bad)

Total_FF0_Neutral <- data.frame(matrix(ncol = 2, nrow = 1, 0))
colnames(Total_FF0_Neutral) <- c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")

for (i in 1:nrow(FF0_neutral)){
  Total_FF0_Neutral <- Total_FF0_Neutral + FF0_neutral$Output[[i]][, c("Abnormal_Log_Return", "Cumulative_Abnormal_Log_Returns")]
}

Total_FF0_Neutral <- Total_FF0_Neutral / nrow(FF0_neutral)



#####################################################################################################################################
