library(tidyverse)
library(timeplyr)

op <- options(digits.secs=9)
df_futures <- read_csv("Archive/trb_usdt_futures_export.csv") %>% select(-symbol)
# df_futures$time - lag(df_futures$time), time diff in secs
df_spot <- read_csv("Archive/trb_usdt_spot_export.csv") %>% select(-symbol)
df_trades <- read_csv("Archive/trb_usdt_trades_export.csv") %>% select(-symbol)


df <- tibble(time = unique(c(df_futures$time, df_spot$time, df_trades$time) ) ) %>% arrange(desc(time))
df <- df %>% left_join(df_futures %>% rename(bid_futures = bid_price, ask_futures = ask_price)) %>%
  left_join(df_spot %>% rename(bid_spot = bid_price, ask_spot = ask_price)) %>% 
  left_join(df_trades %>% rename(price_trade = price) %>% select(-is_market_maker))

df <- df[order(nrow(df):1),]

df <- df %>% 
  mutate(bid_futures = approxfun(time, bid_futures, ties = "ordered")(time), 
         ask_futures = approxfun(time, ask_futures, ties = "ordered")(time),
         bid_spot = approxfun(time, bid_spot, ties = "ordered")(time),
         ask_spot = approxfun(time, ask_spot, ties = "ordered")(time),
         quantity = ifelse(is.na(quantity), 0, quantity))


df <- df %>% 
  mutate(price_spot = (bid_spot + ask_spot)/2,
         d_bid_spot = (log(bid_spot) - log(lag(bid_spot)))*10000,
         d_ask_spot = (log(ask_spot) - log(lag(ask_spot)))*10000,
         d_price_spot = (log(price_spot) - log(lag(price_spot)))*10000) %>% 
  mutate(price_futures = (bid_futures + ask_futures)/2,
         d_bid_futures = (log(bid_futures) - log(lag(bid_futures)))*10000,
         d_ask_futures = (log(ask_futures) - log(lag(ask_futures)))*10000,
         d_price_futures = (log(price_futures) - log(lag(price_futures)))*10000) %>%
  mutate(
    bid_change3ms_s = time_roll_apply(x = d_bid_spot, window = "3 milliseconds", time = time, fun = function(x) max(cumsum(x))),
    ask_change3ms_s = time_roll_apply(x = d_ask_spot, window = "3 milliseconds", time = time, fun = function(x) min(cumsum(x))),
    #price_change3ms_s = time_roll_apply(x = d_price_spot, window = "3 milliseconds", time = time, fun = function(x) max(abs(cumsum(x)))),
  ) %>% mutate(
    sudden_bid_change_s = bid_change3ms_s > 7,
    sudden_ask_change_s = ask_change3ms_s < -7,
    #sudden_price_change_s = abs(price_change3ms_s) > 7
  ) %>% 
  mutate(
    bid_change3ms_f = time_roll_apply(x = d_bid_futures, window = "3 milliseconds", time = time, fun = function(x) max(cumsum(x))),
    ask_change3ms_f = time_roll_apply(x = d_ask_futures, window = "3 milliseconds", time = time, fun = function(x) min(cumsum(x))),
    #price_change3ms_f = time_roll_apply(x = d_price_futures, window = "3 milliseconds", time = time, fun = function(x) max(abs(cumsum(x)))),
  ) %>% mutate(
    sudden_bid_change_f = bid_change3ms_f > 7,
    sudden_ask_change_f = ask_change3ms_f < -7,
    #sudden_price_change_f = abs(price_change3ms_f) > 7
  )

df %>% filter(sudden_bid_change_s, sudden_bid_change_f) %>% select(time, sudden_bid_change_s, sudden_bid_change_f) %>%
  rename(spot_increase =sudden_bid_change_s, futures_increase = sudden_bid_change_f) %>% 
  pivot_longer(cols = c("spot_increase", "futures_increase")) %>% distinct() %>% View()
