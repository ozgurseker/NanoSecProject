library(tidyverse)
library(timeplyr)
library(tibbletime)

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

df_sudden <- read_csv("suddenchanges.csv")

df_short <- tibble(time = unique(df$time)) %>% left_join(df_sudden) %>% select(time, sudden_bid_change_s)
df_sudden %>% select(time) %>% distinct() %>% nrow()
