
# Install needed packages

library(dplyr)
library(slider)
library(tidyr)
library(ggplot2)
library(gt)


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


# Take Averages of the Abnormal Returns

# Overall 
Avg_AR_Ret_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowMeans(ov_results_list[[name]],na.rm = TRUE)
  Avg_AR_Ret_df[[ name ]] <- Avg_AR_Ret
}

# Good
Avg_AR_Ret_good_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowMeans(ov_results_good_list[[name]],na.rm = TRUE)
  Avg_AR_Ret_good_df[[ name ]] <- Avg_AR_Ret
}

# bad
Avg_AR_Ret_bad_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowMeans(ov_results_bad_list[[name]],na.rm = TRUE)
  Avg_AR_Ret_bad_df[[ name ]] <- Avg_AR_Ret
}

# neutral
Avg_AR_Ret_neutral_df <- data.frame(matrix(nrow = 21, ncol = 0))

for (name in Ticker){
  Avg_AR_Ret <- rowMeans(ov_results_neutral_list[[name]],na.rm = TRUE)
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

### Industry

industry_CAR <- cumsum(rowMeans(Avg_AR_Ret_df[,1:30], na.rm = TRUE))
industry_good_CAR <- cumsum(rowMeans(Avg_AR_Ret_good_df[,1:30], na.rm = TRUE))
industry_bad_CAR <- cumsum(rowMeans(Avg_AR_Ret_bad_df[,1:30], na.rm = TRUE))
industry_neutral_CAR <- cumsum(rowMeans(Avg_AR_Ret_neutral_df[,1:30], na.rm = TRUE))

Industry_CAR_df <- data.frame(
  industry_CAR = industry_CAR,
  industry_good_CAR = industry_good_CAR,
  industry_bad_CAR = industry_bad_CAR,
  industry_neutral_CAR = industry_neutral_CAR
)
Industry_CAR_df <- Industry_CAR_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Pivot to long
df_long <- Industry_CAR_df %>%
  pivot_longer(
    cols = -day,            # all columns except day
    names_to = "event",     # event names (original column names)
    values_to = "CAR_value" # the CAR values
  )

# Plot Industry

ggplot(Industry_CAR_df , aes(x = day)) +
  geom_line(aes(y = industry_good_CAR, color = "Good News"), size = 1) +
  geom_line(aes(y = industry_neutral_CAR, color = "Neutral News"), size = 1, linetype = "dashed") +
  geom_line(aes(y = industry_bad_CAR, color = "Bad News"), size = 1, linetype = "dotted") +
  labs(
    title = "Cumulative Abnormal Returns by News Category (Constant Mean Model)",
    x = "Event Day",
    y = "CAR"
  ) +
  scale_color_manual(values = c("Good News" = "darkgreen", "Neutral News" = "black", "Bad News" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

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

### neutral

Avg_CAR_Ret_neutral_df <- Avg_CAR_Ret_neutral_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Pivot to long
df_long_neutral <- Avg_CAR_Ret_neutral_df %>%
  pivot_longer(
    cols = -day,            # all columns except day
    names_to = "event",     # event names (original column names)
    values_to = "CAR_value" # the CAR values
  )

# Plot
ggplot(df_long_neutral, aes(x = day, y = CAR_value, color = event)) +
  geom_line() +
  scale_x_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
  labs(x = "Day", y = "Avg CAR Return", title = "Average CAR Returns Across neutral Events") +
  theme_minimal()


####### Statistical Significance ###########

# Windows
windows <- list(
  "[-10:10]"  = -10:10,
  "[-5:5]"    = -5:5,
  "[-2:5]"  = -2:5,
  "[-1:1]" = -1:1
)

# Add day column to Avg_AR_Ret_df (assumes 21 rows)
Avg_AR_Ret_df <- Avg_AR_Ret_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Initialize a list to store the result data frames (one per window)
result_list <- list()

names(windows)

# Loop over each window
for (w in names(windows)) {
  
  # Current window's day values
  current_window <- windows[[w]]
  
  # Filter rows of Avg_AR_Ret_df where day is in the current window and sort by day
  window_df <- Avg_AR_Ret_df %>%
    filter(day %in% current_window) %>%
    arrange(day)
  
  # Extract the numeric data for companies (all columns except 'day')
  companies_data <- window_df %>% select(-day)
  
  # Initialize an empty dataframe for results for this window
  # Columns: ticker, mean, st_dev, p_value
  window_results <- data.frame(ticker = character(),
                               mean = numeric(),
                               st_dev = numeric(),
                               p_value = numeric(),
                               stringsAsFactors = FALSE)
  
  # Loop over each company (each column in companies_data)
  for (comp in colnames(companies_data)) {
    
    # Get the vector of observations for the company over the current window
    comp_data <- companies_data[[comp]]
    
    # Compute the cumulative sum over the window (vector of length equal to nrow(window_df))
    cs <- cumsum(comp_data)
    
    # Skip if there are fewer than 2 non-NA observations (t-test requires at least 2 values)
    if(sum(!is.na(cs)) < 2) next
    
    # Run a one-sample t-test for the cumulative sum (testing if mean != 0)
    tt <- t.test(cs, mu = 0)
    
    # Extract results:
    # - Use the mean of the cumulative sum as the estimate,
    # - Calculate standard deviation with sd(),
    # - Extract the p-value from the t-test.
    comp_mean <- mean(cs, na.rm = TRUE)
    comp_sd   <- sd(cs, na.rm = TRUE)
    comp_p    <- tt$p.value
    
    # Append the results for this company to window_results
    window_results <- rbind(window_results,
                            data.frame(ticker = comp,
                                       mean = comp_mean,
                                       st_dev = comp_sd,
                                       p_value = comp_p,
                                       stringsAsFactors = FALSE))
  }
  
  # Save the result for this window in the result_list
  result_list[[w]] <- window_results
}

# Example Window 1

result_list[[1]] %>%
  gt() %>%
  tab_style(
    style = cell_borders(sides = "right", color = "grey80", weight = px(2)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "grey80", weight = px(2)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_header(title = "Significane Overall per Company -10:10")


###### T-Test for the Industry ######

# Overall

# Initialize a data frame to store the industry results for each window
result_industry_all <- data.frame(window = character(),
                                  mean = numeric(),
                                  st_dev = numeric(),
                                  p_value = numeric(),
                                  stringsAsFactors = FALSE)

# Loop over each window
for (w in names(windows)) {
  
  # Current window's day values
  current_window <- windows[[w]]
  
  # Filter rows where 'day' is in the current window and sort by day
  window_df <- Avg_AR_Ret_df %>%
    filter(day %in% current_window) %>%
    arrange(day)
  
  # Extract only the numeric data (all columns except 'day')
  companies_data <- window_df %>% select(-day)
  
  # Compute the industry vector: average across companies for each day
  industry_vec <- rowMeans(companies_data, na.rm = TRUE)
  
  # Compute the cumulative sum (CAR) over the window
  CAR <- cumsum(industry_vec)
  
  # Run a one-sample t-test on the CAR (test if mean != 0)
  tt <- t.test(CAR, mu = 0)
  
  # Extract summary values:
  # Here we use the mean and standard deviation of the CAR vector,
  # and the p-value from the t-test.
  mean_val <- mean(CAR, na.rm = TRUE)
  sd_val   <- sd(CAR, na.rm = TRUE)
  p_val    <- tt$p.value
  
  # Append the result for the current window to result_industry_all
  result_industry_all <- rbind(result_industry_all,
                               data.frame(window = w,
                                          mean = mean_val,
                                          st_dev = sd_val,
                                          p_value = p_val,
                                          stringsAsFactors = FALSE))
}


result_industry_all %>%
  gt() %>%
  tab_style(
    style = cell_borders(sides = "right", color = "grey80", weight = px(2)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "grey80", weight = px(2)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_header(title = "Significane Overall per Industry")


# Good

# Add day column to Avg_AR_Ret_df (assumes 21 rows)
Avg_AR_Ret_good_df <- Avg_AR_Ret_good_df %>%
  mutate(day = seq(-10, 10, length.out = n()))

# Initialize a data frame to store the industry results for each window
result_industry_all_good <- data.frame(window = character(),
                                  mean = numeric(),
                                  st_dev = numeric(),
                                  p_value = numeric(),
                                  stringsAsFactors = FALSE)

# Loop over each window
for (w in names(windows)) {
  
  # Current window's day values
  current_window <- windows[[w]]
  
  # Filter rows where 'day' is in the current window and sort by day
  window_df <- Avg_AR_Ret_good_df %>%
    filter(day %in% current_window) %>%
    arrange(day)
  
  # Extract only the numeric data (all columns except 'day')
  companies_data <- window_df %>% select(-day)
  
  # Compute the industry vector: average across companies for each day
  industry_vec <- rowMeans(companies_data, na.rm = TRUE)
  
  # Compute the cumulative sum (CAR) over the window
  CAR <- cumsum(industry_vec)
  
  # Run a one-sample t-test on the CAR (test if mean != 0)
  tt <- t.test(CAR, mu = 0)
  
  # Extract summary values:
  # Here we use the mean and standard deviation of the CAR vector,
  # and the p-value from the t-test.
  mean_val <- mean(CAR, na.rm = TRUE)
  sd_val   <- sd(CAR, na.rm = TRUE)
  p_val    <- tt$p.value
  
  # Append the result for the current window to result_industry_all
  result_industry_all_good <- rbind(result_industry_all_good,
                               data.frame(window = w,
                                          mean = mean_val,
                                          st_dev = sd_val,
                                          p_value = p_val,
                                          stringsAsFactors = FALSE))
}


result_industry_all_good %>%
  gt() %>%
  tab_style(
    style = cell_borders(sides = "right", color = "grey80", weight = px(2)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "grey80", weight = px(2)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_header(title = "Significane Good News per Industry")



