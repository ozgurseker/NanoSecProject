library(tidyverse)
library(timeplyr)
library(tibbletime)
library(lubridate)

op <- options(digits.secs=9)

df_sudden <- read_csv("suddenchanges.csv")
df_returns <- read_csv("returns.csv")

df_sudden <- left_join(df_sudden, df_returns)

df <- df_sudden %>% filter(complete.cases(.))

# Bu tablelari temize al latexe, belki
df %>% group_by(sudden_bid_change_s, sudden_bid_change_f, sudden_ask_change_s, sudden_ask_change_f) %>%
  summarise(avg_spot_max = mean(max_spot_return),
            avg_spot_min = mean(min_spot_return),
            avg_futures_max = mean(max_futures_return),
            avg_futures_min = mean(min_futures_return),
            cases = n()) %>% View()

df %>% group_by(sudden_bid_change_s, sudden_ask_change_s) %>%
  summarise(avg_spot_max = mean(max_spot_return),
            avg_spot_min = mean(min_spot_return),
            avg_futures_max = mean(max_futures_return),
            avg_futures_min = mean(min_futures_return),
            cases = n()) %>% View()

df %>% group_by(sudden_bid_change_f, sudden_ask_change_f) %>%
  summarise(avg_spot_max = mean(max_spot_return),
            avg_spot_min = mean(min_spot_return),
            avg_futures_max = mean(max_futures_return),
            avg_futures_min = mean(min_futures_return),
            cases = n()) %>% View()

# Ilk graphler
df %>% filter(sudden_bid_change_s) %>% 
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_futures_return))

df %>% filter(sudden_bid_change_s, sudden_bid_change_f) %>% 
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_futures_return))

df %>% filter(sudden_ask_change_s, sudden_ask_change_f) %>% 
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_futures_return))

df %>% filter(sudden_ask_change_f) %>% 
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_futures_return))

# Her case icin quantity ya da log quantity ile bi regresyon,
# Caseler, sade bid/ask - spot sade bid-ask - futures, iki bid ve iki ask 
# Exact 0 kalma probabilitysini quantity predict ediyor mu diye de bi regresyon logit 


