library(tidyverse)
library(timeplyr)
library(tibbletime)
library(lubridate)

op <- options(digits.secs=9)
df_futures <- read_csv("Archive/trb_usdt_futures_export.csv") %>% select(-symbol)
# df_futures$time - lag(df_futures$time), time diff in secs
df_spot <- read_csv("Archive/trb_usdt_spot_export.csv") %>% select(-symbol)
df_trades <- read_csv("Archive/trb_usdt_trades_export.csv") %>% select(-symbol)

df <- tibble(time = unique(c(df_futures$time, df_spot$time, df_trades$time) ) ) %>% arrange(desc(time))
df <- df %>% left_join(df_futures %>% rename(bid_futures = bid_price, ask_futures = ask_price)) %>%
  left_join(df_spot %>% rename(bid_spot = bid_price, ask_spot = ask_price)) %>% 
  left_join(df_trades %>% rename(price_trade = price) %>% select(-is_market_maker))

df <- df %>% distinct()

rm(df_futures)
rm(df_spot)
rm(df_trades)

df <- df[order(nrow(df):1),]

df <- df %>% 
  mutate(bid_futures = approxfun(time, bid_futures, ties = "ordered")(time), 
         ask_futures = approxfun(time, ask_futures, ties = "ordered")(time),
         bid_spot = approxfun(time, bid_spot, ties = "ordered")(time),
         ask_spot = approxfun(time, ask_spot, ties = "ordered")(time),
         quantity = ifelse(is.na(quantity), 0, quantity))

df_sudden <- read_csv("suddenchanges.csv")

df_shorter <- tibble(time = unique(df$time)) %>% left_join(df_sudden) %>% select(time, sudden_bid_change_s)
df_shorter$last_time <- as.POSIXct(rep(NA, nrow(df_shorter)))
df_shorter$last_time[!is.na(df_shorter$sudden_bid_change_s)] <- as.POSIXct(df_shorter$time[!is.na(df_shorter$sudden_bid_change_s)] + 0.01)
df_shorter <- df_shorter %>% fill(last_time, .direction = "down") %>% filter(time < last_time)

df_shorter <- df_shorter %>% left_join(df) %>%
  mutate(time_diff = time - lag(time)) %>%
  mutate(
    price_spot = (bid_spot + ask_spot)/2,
    price_futures = (bid_futures + ask_futures)/2,
    price_change_s = ifelse(time_diff > 0.01, 0, (log(price_spot)-log(lag(price_spot)))*10000) ,
    price_change_f = ifelse(time_diff > 0.01, 0, (log(price_futures)-log(lag(price_futures)))*10000)
    )

rm(df)
library(data.table)

# df_returns <- df_shorter %>% filter(!is.na(price_change_s), !is.na(price_change_f)) %>%
#   tbl_time(index = time) %>% 
#   collapse_by(period = "5 ms", side = "start") %>%
#   group_by(time) %>% 
#   summarise(max_spot_return = max(cumsum(price_change_s), na.rm = T),
#             min_spot_return = min(cumsum(price_change_s), na.rm = T),
#             max_futures_return = max(cumsum(price_change_f), na.rm = T),
#             min_futures_return = min(cumsum(price_change_f), na.rm = T)) 

df_returns2 <- setDT(df_shorter)[, `:=`(max_spot_return = max(cumsum(df_shorter$price_change_s[between(df_shorter$time, time, time+0.005, incbounds = F)])), 
                                        max_futures_return = max(cumsum(df_shorter$price_change_f[between(df_shorter$time, time, time+0.005, incbounds = F)])),
                                        min_spot_return = min(cumsum(df_shorter$price_change_s[between(df_shorter$time, time, time+0.005, incbounds = F)])), 
                                        min_futures_return = min(cumsum(df_shorter$price_change_f[between(df_shorter$time, time, time+0.005, incbounds = F)]))),
           by = c("time")][]

df_returns2 %>% select(time, max_spot_return, max_futures_return, 
                       min_spot_return, min_futures_return) %>% 
  distinct() %>%
  write_csv("returns.csv")

  
  
  
  
  