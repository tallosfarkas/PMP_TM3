
# Preparation -------------------------------------------------------------

setwd("C:/Users/pertl/Desktop/PMP/Technical Meetings/4")

library(tidyverse)
library(kableExtra)
library(knitr)

earnings <- read_csv("earnings.csv",
                     col_types = cols(...1 = col_skip(),
                                      date = col_date(format = "%Y-%m-%d")))

# add a columns that serves as a unique identifier for each event
earnings$ident <- with(earnings, paste(date, id, sep = "/"))


returns <- read_csv("returns.csv",
                    col_types = cols(...1 = col_skip(), 
                                     date = col_date(format = "%Y-%m-%d")))


fama <- read_csv("FF_Dataset_modified.CSV", 
                 col_types = cols(...1 = col_skip(), date = col_date(format = "%Y-%m-%d")))




# CMR and MM function ----------------------------------------------------------------


# constant mean return model
# this function takes an the id as input and returns the mu
CMR <- function(identity, ret = returns, T1 = -10){
  
  # extract the company
  company <- sub(".*/", "", identity)
  
  # extract the date
  datum <- as.Date(sub("/.*", "", identity))
  
  # create sub data frame from returns
  sub <- ret %>% filter(id == company) %>% filter(date <= datum) %>% arrange(date)
  sub <- tail(sub, 250-T1)
  sub <- head(sub, 250)
  
  
  # extract returns
  returns <- na.omit(sub$return)
  # return NA if we have less than 100 observations
  if (length(returns) < 100) {return(NA)}
  
  variance <- var(returns)
  
  return(list(
    mu = mean(returns),
    sigma = variance))
  
}


# market model
# this function takes the identity as input and returns the coef
# for mp (market portfolio) input either SPX or DJI
MM <- function(identity, ret = returns, T1 = -10, mp = "SPX"){
  
  # extract the company
  company <- sub(".*/", "", identity)
  
  # extract the date
  datum <- as.Date(sub("/.*", "", identity))
  
  # create sub data frame from returns
  sub <- ret %>% filter(id %in% c(company, mp)) %>% filter(date <= datum)
  sub <- sub %>% pivot_wider(names_from = id, values_from = return) %>% arrange(date)
  sub <- tail(sub, 250-T1)
  sub <- head(sub, 250)
  colnames(sub) <- c("date", "dep", "ind")
  
  # return NA if we have less than 50 observations
  if (length(na.omit(sub$dep)) < 100) {return(NA)}
  
  # run the regression (OLS)
  model <- lm(dep ~ ind, data = sub)
  
  coef <- model$coefficients
  sigma <- var(model$residuals)
  
  return(list(
    coef = coef,
    sigma = sigma
  ))
  
}


# fama french model
# this function takes the identity as input and returns the coef of a ff5 OLS
FF5 <- function(identity, T1 = -10){
  
  # extract the company and date
  company <- sub(".*/", "", identity)
  datum <- as.Date(sub("/.*", "", identity))
  
  # create sub data frame from returns
  
  sub_dep <- returns %>% filter(id == company) %>% filter(date <= datum)
  sub_dep <- sub_dep %>% pivot_wider(names_from = id, values_from = return) %>% arrange(date)
  sub_dep <- tail(sub_dep, 250-T1)
  sub_dep <- head(sub_dep, 250)
  colnames(sub_dep) <- c("date", "dep")
  
  sub_ind <- fama %>% filter(date <= datum)
  sub_ind <- tail(sub_ind, 250-T1)
  sub_ind <- head(sub_ind, 250)
  
  # return NA if we have less than 50 observations
  if (length(na.omit(sub_dep$dep)) < 100) {return(NA)}
  
  model <- lm(sub_dep$dep-sub_ind$RF ~ sub_ind$MKT + sub_ind$SMB + sub_ind$HML + sub_ind$RMW + sub_ind$CMA)
  
  coef <- model$coefficients
  sigma <- var(model$residuals)
  
  return(list(
    coef = coef,
    sigma = sigma
  ))
}



# Average Abnormal Return -------------------------------------------------


# computes the average abnormal event for all tau with the constant mean return model
AR.CMR <- function(mood, ear = earnings, ret = returns, T1 = -10, T2 = 10){
  
  # define all events
  events <- ear %>% filter(news == mood)
  events <- events$ident
  
  # sort returns
  ret <- ret %>% arrange(date)
  
  # initiate results & variance
  results <- NULL
  variance <- NULL
  
  for (event in events) {
    
    # extract the company
    company <- sub(".*/", "", event)
    
    # extract the date
    datum <- as.Date(sub("/.*", "", event))
    
    # compute model
    model <- CMR(event, T1 = T1)
    
    # skip loop if model returns NA
    if (!is.list(model)) {next}
    
    # save mu and sigma
    mu <- model$mu
    sigma <- model$sigma
    
    # filter for the company
    return <- ret %>%  filter(id == company)
    
    # extract location of date
    t <- which(return$date == datum)
    
    # extract return
    return <- return[(t+T1):(t+T2), 3]
    return <- return$return
    
    AR <- t(return - mu)
    
    variance <- c(variance, sigma)
    results <- rbind(results, AR)
    
  }
  
  colnames(results) <- as.character(T1:T2)
  
  # save the variances
  variance = sum(variance)/length(variance)^2
  
  # save the results
  results <- colMeans(na.omit(results))
  
  return(list(
    AR = results,
    variance = variance
  ))
  
}



# computes the average abnormal event for all tau with the market model
AR.MM <- function(mood, ear = earnings, ret = returns, T1 = -10, T2 = 10, mp = "SPX"){
  
  # define all events
  events <- ear %>% filter(news == mood)
  events <- events$ident
  
  # sort returns
  ret <- ret %>% arrange(date)
  
  # initiate results & variance
  results <- NULL
  variance <- NULL
  
  for (event in events) {
    
    # extract the company
    company <- sub(".*/", "", event)
    
    # extract the date
    datum <- as.Date(sub("/.*", "", event))
    
    # compute model and save coef and sigma
    model <- MM(identity = event, T1 = T1, mp = mp)
    
    # skip loop if model returns NA
    if (!is.list(model)) {next}
    
    coef <- model$coef
    sigma <- model$sigma
    
    # filter for the company
    return <- ret %>%  filter(id == company)
    
    # Extract the returns
    # extract location of date
    t <- which(return$date == datum)
    
    return <- return[(t+T1):(t+T2), 3]
    return <- return$return
    
    #......................
    # extract the market returns
    
    # filter for the index
    return.m <- ret %>%  filter(id == mp)
    
    # Extract the returns
    # extract location of date
    t <- which(return.m$date == datum)
    
    return.m <- return.m[(t+T1):(t+T2), 3]
    return.m <- return.m$return
    return.m <- coef[1] + coef[2]*return.m
    #...................................
    
    AR <- t(return - return.m)
    
    results <- rbind(results, AR)
    variance <- c(variance, sigma)
    
  }
  
  colnames(results) <- as.character(T1:T2)
  
  # save the variances
  variance = sum(variance)/length(variance)^2
  
  # save the results
  results <- colMeans(na.omit(results))
  
  return(list(
    AR = results,
    variance = variance
  ))
  
}







# computes the average abnormal event for all tau with the fama french model
AR.FF5 <- function(mood, ear = earnings, ret = returns, ff5 = fama, T1 = -10, T2 = 10){
  
  # define all events
  events <- ear %>% filter(news == mood)
  events <- events$ident
  
  # sort returns
  ret <- ret %>% arrange(date)
  
  # initiate results & variance
  results <- NULL
  variance <- NULL
  
  for (event in events) {
    
    # extract the company
    company <- sub(".*/", "", event)
    
    # extract the date
    datum <- as.Date(sub("/.*", "", event))
    
    # compute model
    model <- FF5(identity = event, T1 = T1)
    
    # skip loop if model returns NA
    if (!is.list(model)) {next}
    
    # save coef and sigma
    coef <- model$coef
    sigma <- model$sigma
    
    # filter for the company
    return <- ret %>%  filter(id == company)
    
    # Extract the returns
    # extract location of date
    t <- which(return$date == datum)
    
    return <- return[(t+T1):(t+T2), 3]
    return <- return$return
    #........................
    
    # Extract the fama returns
    # extract location of date
    t <- which(ff5$date == datum)
    
    # skip event if it is not in fama data
    if (length(t)==0) {next}
    
    fama.return <- cbind(1, fama[(t+T1):(t+T2), 2:6])
    fama.return <- colSums(apply(fama.return, 1, function(r){r*coef}))
    #........................
    
    
    
    
    AR <- t(return - fama.return)
    
    variance <- c(variance, sigma)
    results <- rbind(results, AR)
    
  }
  
  colnames(results) <- as.character(T1:T2)
  
  # save the variances
  variance = sum(variance)/length(variance)^2
  
  # save the results
  results <- colMeans(na.omit(results))
  
  return(list(
    AR = results,
    variance = variance
  ))
  
}





# Test Zone ---------------------------------------------------------------

# Test for: CMR & MM & FF5

# this identity should return NA
identity = "2013-04-23/AAPL"

# this identity should return correctly
identity = "2024-02-28/CRM"

system.time(FF5(identity))
CMR(identity)
MM(identity)
FF5(identity)


# appears to work
AR.CMR(mood = "good")
AR.MM(mood = "good")
AR.FF5(mood = "good")




# Exercise 3 --------------------------------------------------------------

# Constant Mean Return Model

AR.CMR.df <- t(rbind(
  good = AR.CMR(mood = "good")$AR,
  no = AR.CMR(mood = "no")$AR,
  bad = AR.CMR(mood = "bad")$AR
))

AR.CMR.df <- (exp(as.data.frame(AR.CMR.df))-1)*100

CAR.CMR.df <-  AR.CMR.df %>% mutate_all(cumsum)


# Create & save table for AR.CMR.df
AR.CMR.df %>% 
  kbl(caption = "Abnormal Returns (in %)", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/AR_CMR.pdf")

# Create & save table for CAR.CMR.df
CAR.CMR.df %>% 
  kbl(caption = "Cumulative Abnormal Returns (in %)", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/CAR_CMR.pdf")


#..............................................

# Market Model

AR.MM.df <- t(rbind(
  good = AR.MM(mood = "good")$AR,
  no = AR.MM(mood = "no")$AR,
  bad = AR.MM(mood = "bad")$AR
))

AR.MM.df <- (exp(as.data.frame(AR.MM.df))-1)*100

CAR.MM.df <-  AR.MM.df %>% mutate_all(cumsum)

# Create & save table for AR.MM.df
AR.MM.df %>% 
  kbl(caption = "Abnormal Returns (in %)", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/AR_MM.pdf")

# Create & save table for CAR.MM.df
CAR.MM.df %>% 
  kbl(caption = "Cumulative Abnormal Returns (in %)", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/CAR_MM.pdf")

#...................................................................

# FF5 Model
AR.FF5.df <- t(rbind(
  good = AR.FF5(mood = "good")$AR,
  no = AR.FF5(mood = "no")$AR,
  bad = AR.FF5(mood = "bad")$AR
))

AR.FF5.df <- (exp(as.data.frame(AR.FF5.df))-1)*100

CAR.FF5.df <-  AR.FF5.df %>% mutate_all(cumsum)

# Create & save table for AR.FF5.df
AR.FF5.df %>% 
  kbl(caption = "Abnormal Returns (in %)", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/AR_FF5.pdf")

# Create & save table for CAR.FF5.df
CAR.FF5.df %>% 
  kbl(caption = "Cumulative Abnormal Returns (in %)", digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/CAR_FF5.pdf")



# Exercise 4 --------------------------------------------------------------


CAR.MM.df$row_names <- rownames(CAR.MM.df)

# Reshape the data frame from wide to long format
CAR.MM.df_long <- tidyr::pivot_longer(CAR.MM.df, cols = c("good", "bad", "no"),
                                      names_to = "Category", values_to = "Value")

p <- ggplot(CAR.MM.df_long, aes(x = as.numeric(row_names), y = Value, color = factor(Category, levels = c("good", "no", "bad")))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1), labels = seq(-10, 10, by = 1)) +
  scale_color_manual(values = c(good = "#369108", no = "#F2CC0D", bad = "#AA1609")) +
  labs(x = "Event Time", y = "CAR (in %)", title = "MM Model: Cumulative Abnormal Return by news category", color = "News") +
  theme_bw() +
  theme(axis.text = element_text(size = 11))  # Adjust axis label font size here

ggsave("Results/CAR_MM_plot.png", p, width = 7, height = 4, dpi = 600)


rm(CAR.MM.df_long, p)
#...................................................
  
# Lets also do this for the CMR model

CAR.CMR.df$row_names <- rownames(CAR.CMR.df)

# Reshape the data frame from wide to long format
CAR.CMR.df_long <- tidyr::pivot_longer(CAR.CMR.df, cols = c("good", "bad", "no"),
                                      names_to = "Category", values_to = "Value")

p <- ggplot(CAR.CMR.df_long, aes(x = as.numeric(row_names), y = Value, color = factor(Category, levels = c("good", "no", "bad")))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1), labels = seq(-10, 10, by = 1)) +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  scale_color_manual(values = c(good = "#369108", no = "#F2CC0D", bad = "#AA1609")) +
  labs(x = "Event Time", y = "CAR (in %)", title = "CMR Model: Cumulative Abnormal Return by news category", color = "News") +
  theme_bw() +
  theme(axis.text = element_text(size = 11))  # Adjust axis label font size here

ggsave("Results/CAR_CMR_plot.png", p, width = 7, height = 4, dpi = 600)


rm(CAR.CMR.df_long, p)

#..........................................................

# Lets also do this for the FF5 model

CAR.FF5.df$row_names <- rownames(CAR.FF5.df)

# Reshape the data frame from wide to long format
CAR.FF5.df_long <- tidyr::pivot_longer(CAR.FF5.df, cols = c("good", "bad", "no"),
                                       names_to = "Category", values_to = "Value")

p <- ggplot(CAR.FF5.df_long, aes(x = as.numeric(row_names), y = Value, color = factor(Category, levels = c("good", "no", "bad")))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1), labels = seq(-10, 10, by = 1)) +
  scale_color_manual(values = c(good = "#369108", no = "#F2CC0D", bad = "#AA1609")) +
  labs(x = "Event Time", y = "CAR (in %)", title = "FF5 Model: Cumulative Abnormal Return by news category", color = "News") +
  theme_bw() +
  theme(axis.text = element_text(size = 11))  # Adjust axis label font size here

ggsave("Results/CAR_FF5_plot.png", p, width = 7, height = 4, dpi = 600)


rm(CAR.FF5.df_long, p)



# Exercise 5 --------------------------------------------------------------

# Constant Mean Return Model

variance.CMR <- c(
  good = AR.CMR(mood = "good")$variance,
  no = AR.CMR(mood = "no")$variance,
  bad = AR.CMR(mood = "bad")$variance)


estimator.CMR <- function(T1, T2, mood){
  sum(AR.CMR.df[(T1+11):(T2+11), mood]/100)/(variance.CMR[mood]*(T2-T1))^(1/2)
}

t.CMR <- rbind(
  "good" = c(estimator.CMR(-10,10,"good"), estimator.CMR(-5,5,"good"), estimator.CMR(-2,5,"good"), estimator.CMR(-1,1,"good"), (AR.CMR.df[11, "good"]/100)/(variance.CMR["good"])^(1/2)),
  "no" = c(estimator.CMR(-10,10,"no"), estimator.CMR(-5,5,"no"), estimator.CMR(-2,5,"no"), estimator.CMR(-1,1,"no"), (AR.CMR.df[11, "no"]/100)/(variance.CMR["no"])^(1/2)),
  "bad" = c(estimator.CMR(-10,10,"bad"), estimator.CMR(-5,5,"bad"), estimator.CMR(-2,5,"bad"), estimator.CMR(-1,1,"bad"), (AR.CMR.df[11, "bad"]/100)/(variance.CMR["bad"])^(1/2))
)

colnames(t.CMR) <- c("[-10,10]", "[-5,5]", "[-2,5]", "[-1,1]", "[0,0]")


p.CMR <- 2*pnorm(abs(t.CMR), lower.tail = F)

# Create & save table for p values
p.CMR %>% 
  kbl(caption = "CMR Model: P Values for CARs", digits = 6) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/p_CMR.pdf")


rm(estimator.CMR, variance.CMR, t.CMR)

#................................................................................

# Market Model

variance.MM <- c(
  good = AR.MM(mood = "good")$variance,
  no = AR.MM(mood = "no")$variance,
  bad = AR.MM(mood = "bad")$variance)


estimator.MM <- function(T1, T2, mood){
  sum(AR.MM.df[(T1+11):(T2+11), mood]/100)/(variance.MM[mood]*(T2-T1))^(1/2)
}

t.MM <- rbind(
  "good" = c(estimator.MM(-10,10,"good"), estimator.MM(-5,5,"good"), estimator.MM(-2,5,"good"), estimator.MM(-1,1,"good"), (AR.MM.df[11, "good"]/100)/(variance.MM["good"])^(1/2)),
  "no" = c(estimator.MM(-10,10,"no"), estimator.MM(-5,5,"no"), estimator.MM(-2,5,"no"), estimator.MM(-1,1,"no"), (AR.MM.df[11, "no"]/100)/(variance.MM["no"])^(1/2)),
  "bad" = c(estimator.MM(-10,10,"bad"), estimator.MM(-5,5,"bad"), estimator.MM(-2,5,"bad"), estimator.MM(-1,1,"bad"), (AR.MM.df[11, "bad"]/100)/(variance.MM["bad"])^(1/2))
)

colnames(t.MM) <- c("[-10,10]", "[-5,5]", "[-2,5]", "[-1,1]", "[0,0]")



p.MM <- 2*pnorm(abs(t.MM), lower.tail = F)

# Create & save table for p values
p.MM %>% 
  kbl(caption = "MM Model: P Values for CARs", digits = 6) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/p_MM.pdf")


rm(estimator.MM, variance.MM)

#..............................................................................

# FF5 Model

variance.FF5 <- c(
  good = AR.FF5(mood = "good")$variance,
  no = AR.FF5(mood = "no")$variance,
  bad = AR.FF5(mood = "bad")$variance)


estimator.FF5 <- function(T1, T2, mood){
  sum(AR.FF5.df[(T1+11):(T2+11), mood]/100)/(variance.FF5[mood]*(T2-T1))^(1/2)
}

t.FF5 <- rbind(
  "good" = c(estimator.FF5(-10,10,"good"), estimator.FF5(-5,5,"good"), estimator.FF5(-2,5,"good"), estimator.FF5(-1,1,"good"), (AR.FF5.df[11, "good"]/100)/(variance.FF5["good"])^(1/2)),
  "no" = c(estimator.FF5(-10,10,"no"), estimator.FF5(-5,5,"no"), estimator.FF5(-2,5,"no"), estimator.FF5(-1,1,"no"), (AR.FF5.df[11, "no"]/100)/(variance.FF5["no"])^(1/2)),
  "bad" = c(estimator.FF5(-10,10,"bad"), estimator.FF5(-5,5,"bad"), estimator.FF5(-2,5,"bad"), estimator.FF5(-1,1,"bad"), (AR.FF5.df[11, "bad"]/100)/(variance.FF5["bad"])^(1/2))
)

colnames(t.FF5) <- c("[-10,10]", "[-5,5]", "[-2,5]", "[-1,1]", "[0,0]")



p.FF5 <- 2*pnorm(abs(t.FF5), lower.tail = F)

# Create & save table for p values
p.FF5 %>% 
  kbl(caption = "FF5 Model: P Values for CARs", digits = 6) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable("Results/p_FF5.pdf")


rm(estimator.FF5, variance.FF5)


