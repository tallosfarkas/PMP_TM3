
#### 1. Load Required Libraries ####

library(readxl)    # for reading Excel files
library(data.table)  # for fast data manipulation


#### 2. Load and Inspect the Stock Prices Data ####

# Load the RData file that contains the stock prices (tsr)
load("StockPrices.TM3.RData")

# Check structure and summary of tsr
str(tsr)
summary(tsr)


#### 3. Load and Clean the Earnings Events Data  ####

# Load the Excel file containing earnings announcements
events <- read_excel("HistoryEarningsTM3.xlsx")

# Convert to a data.frame
events <- as.data.frame(events)

# Convert the 'Ann Date' column to Date format
events$`Ann Date` <- as.Date(events$`Ann Date`, format = "%m/%d/%Y")

# Rename columns for consistency:
# 'Ann Date' to 'Ann_Date' and 'Per End' to 'Per_End'
names(events)[names(events) == 'Ann Date'] <- 'Ann_Date'
names(events)[names(events) == 'Per End'] <- 'Per_End'

# Inspect the structure and summary of the events data
str(events)
summary(events)


#### 4. Clean the Events Data: Remove Missing Values  ####

# Remove events with missing Reported or Estimate values
events_clean <- subset(events, !is.na(Reported) & !is.na(Estimate))

# Calculate the percentage difference between Reported and Estimate
events_clean$Diff <- (events_clean$Reported - events_clean$Estimate) / events_clean$Estimate

# Categorize events based on the earnings difference:
#   "good" if Diff > 2.5%, "bad" if Diff < -2.5%, else "no news"
events_clean$news <- with(events_clean,
                          ifelse(Diff > 0.025, "good",
                                 ifelse(Diff < -0.025, "bad", "no news")))

# Inspect the cleaned events data
str(events_clean)
summary(events_clean)


#### 5. Prepare the Stock Prices Data (tsr): Compute Log Returns ####

# Identify the columns in tsr containing equity prices using pattern matching
equity_cols <- grep("Equity", names(tsr), value = TRUE)

# Compute log returns for each equity using the shift function from data.table
tsr[, (paste0(equity_cols, "_lr")) := lapply(.SD, function(x) log(x) - log(shift(x, type = "lag"))),
    .SDcols = equity_cols]

# Compute log returns for the market indices (e.g., DJI_Index and SPX_Index)
tsr[, DJI_lr := log(DJI_Index) - log(shift(DJI_Index, type = "lag"))]
tsr[, SPX_lr := log(SPX_Index) - log(shift(SPX_Index, type = "lag"))]

# View a snapshot of the updated tsr data with log returns
head(tsr)
str(tsr)
summary(tsr)
