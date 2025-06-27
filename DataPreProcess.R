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
         d_price_futures = (log(price_futures) - log(lag(price_futures)))*10000)


df %>% mutate(
  sudden_bid_change_s = d_bid_spot > 5,
  sudden_bid_change_f = d_bid_futures > 5,
  #sudden_price_change_f = abs(price_change3ms_f) > 7
) %>% 
  filter(sudden_bid_change_s | sudden_bid_change_f) %>% select(time, sudden_bid_change_s, sudden_bid_change_f) %>%
  rename(spot_increase =sudden_bid_change_s, futures_increase = sudden_bid_change_f) %>% 
  pivot_longer(cols = c("spot_increase", "futures_increase")) %>% filter(value) %>%
  ggplot() + geom_point(aes(x = time, y = 1, color = name))



sudden_bid_increases <- df %>% mutate(
  sudden_bid_change_s = d_bid_spot > 7,
  sudden_bid_change_f = d_bid_futures > 7,
  time_diff = time - lag(time)) %>% filter((sudden_bid_change_s | sudden_bid_change_f), time_diff < 0.003) %>%
  select(time, sudden_bid_change_s, sudden_bid_change_f, quantity)
  
sudden_bid_increases_sum <- sudden_bid_increases %>% tbl_time(index = time) %>% 
  collapse_by("5 ms", side = "end") %>% group_by(time) %>%
  summarise(pre_bid_s = sum(sudden_bid_change_s), pre_bid_f = sum(sudden_bid_change_f),
            pre_quantity = sum(quantity, na.rm = T)) %>% 
  left_join(sudden_bid_increases %>% tbl_time(index = time) %>% 
              collapse_by("10 ms", side = "start") %>% group_by(time) %>% 
              summarise(post_bid_s = sum(sudden_bid_change_s), post_bid_f = sum(sudden_bid_change_f)))


sudden_bid_increases <- sudden_bid_increases %>% left_join(sudden_bid_increases_sum) 

fix_cols <- c("pre_bid_s", "pre_bid_f", "pre_quantity", "post_bid_f", "post_bid_s")
sudden_bid_increases[fix_cols][is.na(sudden_bid_increases[fix_cols])] <- -1


# spot to futures effect for bid changes 
sudden_bid_increases %>% filter(sudden_bid_change_s) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_s) %>% 
  filter(pre_bid_s < 2) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_s, !sudden_bid_change_f) %>% 
  filter(pre_bid_s < 2) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_s, !sudden_bid_change_f) %>% 
  filter(pre_bid_s < 2, pre_bid_f < 1) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_s, !sudden_bid_change_f) %>% 
  filter(pre_bid_s < 2, pre_bid_f < 1, post_bid_f > 0) %>% nrow()

# futures to spot effect for bid changes
sudden_bid_increases %>% filter(sudden_bid_change_f) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_f) %>% 
  filter(pre_bid_f < 2) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_f, !sudden_bid_change_s) %>% 
  filter(pre_bid_f < 2) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_f, !sudden_bid_change_s) %>% 
  filter(pre_bid_f < 2, pre_bid_s < 1) %>% nrow()
sudden_bid_increases %>% filter(sudden_bid_change_f, !sudden_bid_change_s) %>% 
  filter(pre_bid_f < 2, pre_bid_s < 1, post_bid_s > 0) %>% nrow()



#### Sudden Changes Save ####
df_suddenchanges <- df %>% filter(abs(d_bid_spot) > 1 | abs(d_ask_spot) > 1 | quantity > 0 | 
                abs(d_bid_futures) > 1 | abs(d_ask_futures) > 1 ) %>%
  mutate(
    bid_change3ms_s = time_roll_apply(x = d_bid_spot, window = "3 milliseconds", time = time, fun = function(x) max(cumsum(x))),
    ask_change3ms_s = time_roll_apply(x = d_ask_spot, window = "3 milliseconds", time = time, fun = function(x) min(cumsum(x))),
    #price_change3ms_s = time_roll_apply(x = d_price_spot, window = "3 milliseconds", time = time, fun = function(x) max(abs(cumsum(x)))),
  ) %>% mutate(
    sudden_bid_change_s = bid_change3ms_s > 7,
    sudden_ask_change_s = ask_change3ms_s < -7,
    quantity_rolled = time_roll_sum(x = quantity, window = "10 milliseconds", time = time)
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

df_suddenchanges %>% filter(sudden_bid_change_s | sudden_ask_change_s | sudden_bid_change_f | sudden_ask_change_f) %>% distinct() %>%
  write_csv("suddenchanges.csv")

