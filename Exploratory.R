library(tidyverse)
library(timeplyr)

op <- options(digits.secs=3)
df_futures <- read_csv("Archive/trb_usdt_futures_export.csv") %>% select(-symbol)
# df_futures$time - lag(df_futures$time), time diff in secs
df_spot <- read_csv("Archive/trb_usdt_spot_export.csv") %>% select(-symbol)
df_trades <- read_csv("Archive/trb_usdt_trades_export.csv") %>% select(-symbol)

# Ensure time is POSIXct
df_spot <- df_spot %>%
  arrange(time) %>% mutate(price = (bid_price + ask_price)/2) %>%
  mutate(bid_change = (log(bid_price) - log(lag(bid_price)))*10000,
  ask_change = (log(ask_price) - log(lag(ask_price)))*10000,
  price_change = (log(price) - log(lag(price)))*10000) %>% 
  mutate(
  bid_change3ms = time_roll_sum(x = bid_change, window = "3 milliseconds", time = time),
  ask_change3ms = time_roll_sum(x = ask_change, window = "3 milliseconds", time = time),
  price_change3ms = time_roll_sum(x = price_change, window = "3 milliseconds", time = time),
  ) %>% mutate(
    sudden_bid_change = bid_change3ms > 7,
    sudden_ask_change = ask_change3ms > 7,
    sudden_price_change = price_change3ms > 7
  )


df_futures <- df_futures %>%
  arrange(time) %>% mutate(price = (bid_price + ask_price)/2) %>%
  mutate(bid_change = (log(bid_price) - log(lag(bid_price)))*10000,
         ask_change = (log(ask_price) - log(lag(ask_price)))*10000,
         price_change = (log(price) - log(lag(price)))*10000) %>% 
  mutate(
    bid_change3ms = time_roll_sum(x = bid_change, window = "3 milliseconds", time = time),
    ask_change3ms = time_roll_sum(x = ask_change, window = "3 milliseconds", time = time),
    price_change3ms = time_roll_sum(x = price_change, window = "3 milliseconds", time = time),
  ) %>% mutate(
    sudden_bid_change = bid_change3ms > 7,
    sudden_ask_change = ask_change3ms > 7,
    sudden_price_change = price_change3ms > 7
  )

max_return <- function(df, t0, delta_t) {
  return(max(cumsum(df$price_change[df$time >= t0 & df$time <= t0 + delta_t])))
}

min_return <- function(df, t0, delta_t) {
  return(min(cumsum(df$price_change[df$time >= t0 & df$time <= t0 + delta_t])))
}

traded_qty <- function(df, t0, delta_t) {
  return(sum(df$quantity[df$time >= t0-delta_t & df$time <= t0]))
}

# Interested oldugun suddenchange ile diger datayi birlestir, 
# suddenchangeden sonra gelen ilk observationlari sil
# Sonra cumsum ve max 

spot_moves <- df_spot %>% filter(sudden_ask_change | sudden_price_change)

library(tibbletime)

futures5ms <- df_futures %>% tbl_time(index = time) %>% 
  collapse_by(period = "5 ms", side = "start", clean = T) %>% 
  group_by(time) %>% summarise(max_5ms = max(cumsum(price_change)),
                               min_5ms = min(cumsum(price_change))) %>% 
  select(time, max_5ms, min_5ms)

m_future5ms_spotmoves <- survival::neardate(1:nrow(spot_moves), 1:nrow(futures5ms), 
                                            spot_moves$time,futures5ms$time, best = "after")

