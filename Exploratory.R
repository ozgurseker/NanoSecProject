library(tidyverse)

op <- options(digits.secs=3)
df_futures <- read_csv("Archive/trb_usdt_futures_export.csv") %>% select(-symbol)
# df_futures$time - lag(df_futures$time), time diff in secs
df_spot <- read_csv("Archive/trb_usdt_spot_export.csv") %>% select(-symbol)
df_trades <- read_csv("Archive/trb_usdt_trades_export.csv") %>% select(-symbol)

# Ensure time is POSIXct
df_spot <- df_spot %>%
  arrange(time) %>% mutate(price = (bid_price + ask_price)/2) %>%
  mutate(bid_change = (bid_price - lag(bid_price))*10000,
  ask_change = (ask_price - lag(ask_price))*10000,
  price_change = (price - lag(price))*10000) %>%
  mutate(bid_change_3ms = map_dbl(row_number(), function(i) {
    t0 <- time[i]
    idx <- which(time >= t0 - 0.003 & time <= t0)
    sum(bid_change[idx], na.rm = TRUE)
  }),
  ask_change_3ms = map_dbl(row_number(), function(i) {
    t0 <- time[i]
    idx <- which(time >= t0 - 0.003 & time <= t0)
    sum(ask_change[idx], na.rm = TRUE)
  }),
  price_change_3ms = map_dbl(row_number(), function(i) {
    t0 <- time[i]
    idx <- which(time >= t0 - 0.003 & time <= t0)
    sum(price_change[idx], na.rm = TRUE)
  })) %>% mutate(
    sudden_bid_change = bid_change_3ms > 7,
    sudden_ask_change = ask_change_3ms > 7,
    sudden_price_change = price_change_3ms > 7
  )

df_futures <- df_futures %>%
  arrange(time) %>% mutate(price = (bid_price + ask_price)/2) %>%
  mutate(bid_change = (bid_price - lag(bid_price))*10000,
  ask_change = (ask_price - lag(ask_price))*10000,
  price_change = (price - lag(price))*10000) %>%
  mutate(bid_change_3ms = map_dbl(row_number(), function(i) {
    t0 <- time[i]
    idx <- which(time >= t0 - 0.003 & time <= t0)
    sum(bid_change[idx], na.rm = TRUE)
  }),
  ask_change_3ms = map_dbl(row_number(), function(i) {
    t0 <- time[i]
    idx <- which(time >= t0 - 0.003 & time <= t0)
    sum(ask_change[idx], na.rm = TRUE)
  }),
  price_change_3ms = map_dbl(row_number(), function(i) {
    t0 <- time[i]
    idx <- which(time >= t0 - 0.003 & time <= t0)
    sum(price_change[idx], na.rm = TRUE)
  })) %>% mutate(
    sudden_bid_change = bid_change_3ms > 7,
    sudden_ask_change = ask_change_3ms > 7,
    sudden_price_change = price_change_3ms > 7
  ) 

max_return <- function(df, t0, delta_t) {
  df <- df %>% filter(time >= t0 & time <= t0 + delta_t)
  return(max(cumsum(df$price_change)))
}

min_return <- function(df, t0, delta_t) {
  df <- df %>% filter(time >= t0 & time <= t0 + delta_t)
  return(min(cumsum(df$price_change)))
}