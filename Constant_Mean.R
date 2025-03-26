
# Install needed packages

library(dplyr)
library(slider)
library(tidyr)
library(ggplot2)


# Constant Mean Log-Returns Returns Model

# Create the mean log-returns
for (col in names(log_ret_df)) {
  if (is.numeric(log_ret_df[[col]])) {  # Apply only to numeric columns
   log_ret_df <- log_ret_df %>%
      mutate(!!paste0(col, "_rolling_mean") := slide_dbl(.data[[col]], mean, .before = 250, .complete = TRUE))
  }
}
log_ret_mean_df <- log_ret_df[,-(2:33)]
log_ret_mean_df <- log_ret_mean_df[,1:31]
log_ret_df <- log_ret_df[,-(32:65)]


# Extract company, date and news state
events_df <- events_clean[,c(1,2,8)]

# Add the time frame

Ticker <- (unique(events_df[1]))[[1]]
colnames(log_ret_mean_df) <- c("est_Date",Ticker)
colnames(log_ret_df) <- c("est_Date",Ticker)

log_ret_mean_df_long <- log_ret_mean_df %>%
  pivot_longer(cols = Ticker,  # Columns to pivot
               names_to = "Ticker",        # New column for variable names
               values_to = "log_ret_mean")        # New column for values

events_df <- events_df %>%
  mutate(
    start_Date = as.Date(Ann_Date-10),
    end_Date = as.Date(Ann_Date+10)
  ) 

# Add mean log returns (nearest possible date)
setDT(events_df)
setDT(log_ret_mean_df_long)

# Ensure the key columns are Date objects
events_df[, Ann_Date := as.Date(Ann_Date)]
log_ret_mean_df_long[, est_Date := as.Date(est_Date)]
# Here we create est_Date in events_df as Ann_Date - 11 days.
events_df[, est_Date := Ann_Date - 11]

# Set keys on log_ret_mean_df_long for the join.
setkey(log_ret_mean_df_long, Ticker, est_Date)

# Perform a rolling join: for each row in events_df, join by Ticker and est_Date,
# and if no exact match is found in log_ret_mean_df_long,
# roll forward to the next available date (roll = -Inf).
events_dt <- log_ret_mean_df_long[events_df, on = .(Ticker, est_Date), roll = -Inf]

events_df <- as.data.frame(events_dt)



## Creat the abnormal returns for each company and every event

ov_results_list <- list()
ov_results_good_list <- list()
ov_results_bad_list <- list()
ov_results_neutral_list <- list()



for (name in Ticker){
  
  events_df_loop <- events_df[events_df$Ticker == name,]
  
  announcement_Date <- events_df_loop[,"Ann_Date"]

  result_df <- data.frame(matrix(nrow = 21, ncol = 0))
  result_good_df <- data.frame(matrix(nrow = 21, ncol = 0))
  result_bad_df<- data.frame(matrix(nrow = 21, ncol = 0))
  result_neutral_df <- data.frame(matrix(nrow = 21, ncol = 0))
  
  for (i in seq_along(announcement_Date)){
    
    #check status
    status <- events_df_loop[events_df_loop$Ann_Date == announcement_Date[i],"news"]
    
    idx <- which(log_ret_df$est_Date == announcement_Date[i])
    # If the date might not match exactly, find the closest:
    if (length(idx) == 0) {
      idx <- which.min(abs(log_ret_df$est_Date - announcement_Date[i]))
    }
    # Define a window: 10 rows before and 10 rows after (total of 21 rows)
    start_idx <- max(1, idx - 10)         
    end_idx   <- min(nrow(df), idx + 10)   
    
    # Extract the window of rows 
    log_returns <- log_ret_df[start_idx:end_idx,..name]
    abnormal_returns <- log_returns - events_df_loop[events_df_loop$Ann_Date == as.Date(announcement_Date[i]),"log_ret_mean"]
    
    result_df <- cbind(result_df, abnormal_returns)
    
    if (status == "good"){
      result_good_df[[ as.character(announcement_Date[i]) ]] <- abnormal_returns
    }
    else if (status == "bad"){
      result_bad_df[[ as.character(announcement_Date[i]) ]] <- abnormal_returns
    }
    else{
      result_neutral_df[[ as.character(announcement_Date[i]) ]] <- abnormal_returns
    }
  }
  #Change column names to announcement dates:
  colnames(result_df) <- as.character(announcement_Date)
  
  # Save data in large lists
  ov_results_list[[name]] <- result_df
  ov_results_good_list[[name]] <-  result_good_df
  ov_results_bad_list[[name]] <-  result_bad_df
  ov_results_neutral_list[[name]] <-  result_neutral_df
}


X <- ov_results_good_list[[1]]
Y <- ov_results_bad_list[[1]]
Z <- ov_results_neutral_list[[1]]

# Take Averages of the Abnormal Returns

# Overall 
Avg_AR_Ret_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowSums(ov_results_list[[name]])
  Avg_AR_Ret_df[[ name ]] <- Avg_AR_Ret
}

# Good
Avg_AR_Ret_good_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowSums(ov_results_good_list[[name]])
  Avg_AR_Ret_good_df[[ name ]] <- Avg_AR_Ret
}

# bad
Avg_AR_Ret_bad_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowSums(ov_results_bad_list[[name]])
  Avg_AR_Ret_bad_df[[ name ]] <- Avg_AR_Ret
}

# neutral
Avg_AR_Ret_neutral_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowSums(ov_results_neutral_list[[name]])
  Avg_AR_Ret_neutral_df[[ name ]] <- Avg_AR_Ret
}


# Take CumSum of the Log Abnormal Returns

# Overall 
Avg_CAR_Ret_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_CAR_Ret <- cumsum(Avg_AR_Ret_df[,name])
  Avg_CAR_Ret_df[[ name ]] <- Avg_CAR_Ret
}

# Good 
Avg_CAR_Ret_good_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_CAR_Ret <- cumsum(Avg_AR_Ret_good_df[,name])
  Avg_CAR_Ret_good_df[[ name ]] <- Avg_CAR_Ret
}

# bad 
Avg_CAR_Ret_bad_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_CAR_Ret <- cumsum(Avg_AR_Ret_bad_df[,name])
  Avg_CAR_Ret_bad_df[[ name ]] <- Avg_CAR_Ret
}

# neutral 
Avg_CAR_Ret_neutral_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_CAR_Ret <- cumsum(Avg_AR_Ret_neutral_df[,name])
  Avg_CAR_Ret_neutral_df[[ name ]] <- Avg_CAR_Ret
}

###### PLOTTING ########

### Overall
Avg_CAR_Ret_df <- Avg_CAR_Ret_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Pivot to long
df_long <- Avg_CAR_Ret_df %>%
  pivot_longer(
    cols = -day,            # all columns except day
    names_to = "event",     # event names (original column names)
    values_to = "CAR_value" # the CAR values
  )

# Plot
ggplot(df_long, aes(x = day, y = CAR_value, color = event)) +
  geom_line() +
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
  labs(x = "Day", y = "Avg CAR Return", title = "Average CAR Returns Across Events") +
  theme_minimal()

### Good

Avg_CAR_Ret_good_df <- Avg_CAR_Ret_good_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Pivot to long
df_long_good <- Avg_CAR_Ret_good_df %>%
  pivot_longer(
    cols = -day,            # all columns except day
    names_to = "event",     # event names (original column names)
    values_to = "CAR_value" # the CAR values
  )

# Plot
ggplot(df_long_good, aes(x = day, y = CAR_value, color = event)) +
  geom_line() +
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
  labs(x = "Day", y = "Avg CAR Return", title = "Average CAR Returns Across Good Events") +
  theme_minimal()


### bad

Avg_CAR_Ret_bad_df <- Avg_CAR_Ret_bad_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Pivot to long
df_long_bad <- Avg_CAR_Ret_bad_df %>%
  pivot_longer(
    cols = -day,            # all columns except day
    names_to = "event",     # event names (original column names)
    values_to = "CAR_value" # the CAR values
  )

# Plot
ggplot(df_long_bad, aes(x = day, y = CAR_value, color = event)) +
  geom_line() +
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
  labs(x = "Day", y = "Avg CAR Return", title = "Average CAR Returns Across bad Events") +
  theme_minimal()
