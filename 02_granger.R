library(readxl)
library(tidyverse)
library(lmtest)
library(tseries)
library(vars)
library(zoo)
library(gridExtra)
library(forecast)
library(scales)
library(urca)

df <- read_excel("Dataset.xlsx") %>% 
  dplyr::select(Sentiment, Date) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  group_by(Date, Sentiment) %>% 
  summarise(count = n()) %>%
  pivot_wider(names_from = Sentiment, values_from = count, values_fill = 0)

dates <- data.frame(Date = seq(min(df$Date), max(df$Date), by = "day"))

s_counts <- dates %>%
  left_join(df, by = "Date") %>%
  mutate(across(-Date, ~ replace_na(., NA))) %>%
  dplyr::select(-c(joy, neutral, surprise))

dfc <- read_csv("mpox-daily-confirmed-cases.csv") %>%
  filter(Country == "World")%>%
  mutate(Date = as.Date(Day, format="%m/%d/%y")) %>%
  filter(Date >= min(df$Date) & Date <= max(df$Date)) %>%
  dplyr::select(c(Date, `Daily cases`))%>%
  left_join(s_counts, by = "Date") %>%
  mutate(
    across(-Date, ~ replace_na(., NA)),
    across(-Date, ~ na.approx(., Date, na.rm = FALSE))
    ) %>% dplyr::select(-sadness)

dfc_zoo <- zoo(dfc[,-1], order.by = dfc$Date)
plot(dfc_zoo)

# Check stationarity
dfc_df <- data.frame(Date = index(dfc_zoo), coredata(dfc_zoo))
dfc_df <- dfc_df[,-1]
par(mfrow = c(2,2))
acf(dfc_df$fear, main = "Fear", lag.max = 50)
acf(dfc_df$Daily.cases, main = "Daily Cases", lag.max = 50)
acf(dfc_df$anger, main = "Anger", lag.max = 50)
acf(dfc_df$Daily.cases, main = "Disgust", lag.max = 50)


## Differencing
dfc_diff <- diff(dfc_zoo, differences = 1)
data_matrix <- coredata(dfc_diff)
dfc_df <- data.frame(Date = index(dfc_diff), coredata(dfc_diff))
dfc_df <- dfc_df[,-1]

diff_fear <- ts(na.omit(dfc_df$fear))
diff_anger <- ts(na.omit(dfc_df$anger))
diff_disgust <- ts(na.omit(dfc_df$disgust))
diff_cases <- ts(na.omit(dfc_df$Daily.cases))

a1 <- ggAcf(diff_fear, lag.max = 50) + theme_minimal()
a2 <- ggAcf(diff_anger, lag.max = 50) + theme_minimal()
a3 <- ggAcf(diff_disgust, lag.max = 50) + theme_minimal()
a4 <- ggAcf(diff_cases, lag.max = 50) + theme_minimal()
grid.arrange(a1, a2, a3, a4, ncol = 2, nrow = 2)

# Run ADF test for each differenced sentiment time series
summary(ur.df(na.omit(coredata(diff_anger)), type = "trend"))
summary(ur.df(na.omit(coredata(diff_fear)), type = "trend"))
summary(ur.df(na.omit(coredata(diff_disgust)), type = "trend"))
summary(ur.df(na.omit(coredata(diff_cases)), type = "trend"))

# select optimal Lag
lag <- VARselect(data_matrix, type = "const")
print(lag$selection)
var_model <- VAR(data_matrix, p = lag$selection[1], type = "const")
summary(var_model)
plot.ts(data_matrix, main = "Differenced Time Series", col = 1:5)

# Run Granger Causality test
pairwise_granger <- function(data, variables, order = lag$selection[1]) {
  results <- list()
  pairs <- combn(variables, 2, simplify = FALSE)
  
  for (pair in pairs) {
    t1 <- grangertest(as.formula(paste(pair[1], "~", pair[2])), order = order, data = data)
    t2 <- grangertest(as.formula(paste(pair[2], "~", pair[1])), order = order, data = data)
    results[[paste(pair[2], "->", pair[1])]] <- t1
    results[[paste(pair[1], "->", pair[2])]] <- t2
  }
  
  return(results)
}

variables <- c("anger", "fear", "disgust", "Daily.cases")
granger_results <- pairwise_granger(dfc_df, variables)
granger_results

