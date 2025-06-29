library(tidyverse)
library(timeplyr)
library(tibbletime)
library(lubridate)

op <- options(digits.secs=9)

df_sudden <- read_csv("suddenchanges.csv")
df_returns <- read_csv("returns.csv")

df_sudden <- left_join(df_sudden, df_returns)

df <- df_sudden %>% filter(complete.cases(.))

# Ilk graphler

df %>% ggplot() + geom_histogram(aes(x=quantity_rolled))

df %>% filter(sudden_bid_change_s, !sudden_bid_change_f) %>% 
  ggplot() + geom_density(aes(x = min_futures_return, color = "blue")) +
  geom_density(aes(x = max_futures_return, color = "red")) + 
  xlim(c(-8,8)) + 
  scale_colour_manual(name = 'Returns', 
                      values =c('blue'='blue','red'='red'), labels = c('Min','Max')) + 
  xlab("Futures Returns in 10ms (bps)") + ylab("Density") +
  ggtitle("Sudden Decrease in Spot Market Bids Only")

ggplot() + geom_density(aes(x = min_futures_return, color = "blue"),
                        data = df %>% filter(sudden_bid_change_s, 
                                             !sudden_bid_change_f)) +
  geom_density(aes(x = min_spot_return, color = "red"),
               data = df %>% filter(!sudden_bid_change_s, 
                                    sudden_bid_change_f)) + 
  xlim(c(-8,8)) + scale_colour_manual(name = 'Returns', 
                                      values =c('blue'='blue','red'='red'), labels = c('Min','Max'))



df %>% filter(sudden_bid_change_s) %>% 
  ggplot() + geom_point(aes(x=(quantity_rolled), y=max_futures_return)) + 
  stat_smooth(aes(x=(quantity_rolled), y=max_futures_return), se = F, method = "lm")

mdl <- lm(max_futures_return, quantity, df %>% filter(sudden_bid_change_s))

df %>% filter(sudden_ask_change_s, !sudden_ask_change_f) %>% 
  filter(quantity_rolled > 0) %>% filter(max_futures_return < 100) %>% 
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_futures_return)) + 
  stat_smooth(aes(x=log(quantity_rolled), y=max_futures_return), se = F, method = "lm") +
  xlab("log Rolled Quantity Traded in last 10ms") + ylab("Max Futures Return (bps)") +
  ggtitle("Return vs Quantity after Ask Price Jump for Spot")

df %>% filter(!sudden_ask_change_s, sudden_ask_change_f) %>% 
  filter(quantity_rolled > 0) %>%
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_spot_return))

df %>% filter(sudden_bid_change_s, !sudden_bid_change_f) %>% 
  filter(quantity_rolled > 0) %>%
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=min_futures_return))

df %>% filter(!sudden_bid_change_s, sudden_bid_change_f) %>% 
  filter(quantity_rolled > 0) %>%
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=min_spot_return))

df %>% filter(sudden_ask_change_f) %>% 
  ggplot() + geom_point(aes(x=log(quantity_rolled), y=max_futures_return))

# Her case icin quantity ya da log quantity ile bi regresyon,
# Caseler, sade bid/ask - spot sade bid-ask - futures, iki bid ve iki ask 
# Exact 0 kalma probabilitysini quantity predict ediyor mu diye de bi regresyon logit 

summary(lm(max_futures_return ~ log(quantity_rolled), df %>% filter(sudden_ask_change_s, !sudden_ask_change_f) %>% 
     filter(quantity_rolled > 0)))

summary(lm(max_spot_return ~ log(quantity_rolled), df %>% filter(!sudden_ask_change_s, sudden_ask_change_f) %>% 
             filter(quantity_rolled > 0)))


summary(lm(min_futures_return ~ log(quantity_rolled), df %>% filter(sudden_bid_change_s, !sudden_bid_change_f) %>% 
             filter(quantity_rolled > 0)))

summary(lm(min_spot_return ~ log(quantity_rolled), df %>% filter(!sudden_bid_change_s, sudden_bid_change_f) %>% 
             filter(quantity_rolled > 0)))


df <- df %>%
  mutate(pre_sudden_ask_s = time_roll_sum(x = sudden_ask_change_s, window = "1 seconds", time = time),
         pre_sudden_ask_f = time_roll_sum(x = sudden_ask_change_f, window = "1 seconds", time = time),
         pre_sudden_bid_s = time_roll_sum(x = sudden_bid_change_s, window = "1 seconds", time = time),
         pre_sudden_bid_f = time_roll_sum(x = sudden_bid_change_f, window = "1 seconds", time = time)) %>%
  mutate(pre_sudden_ask_s = ifelse(sudden_ask_change_s,pre_sudden_ask_s -1 ,pre_sudden_ask_s),
         pre_sudden_ask_f = ifelse(sudden_ask_change_f,pre_sudden_ask_f -1 ,pre_sudden_ask_f),
         pre_sudden_bid_s = ifelse(sudden_bid_change_s,pre_sudden_bid_s -1 ,pre_sudden_bid_s),
         pre_sudden_bid_f = ifelse(sudden_bid_change_f,pre_sudden_bid_f -1 ,pre_sudden_bid_f))


ggplot() + geom_density(aes(x=pre_sudden_ask_f, color = "Spot"),  
                        df %>% filter(sudden_ask_change_s, !sudden_ask_change_f)) +
  geom_density(aes(x=pre_sudden_ask_s, color = "Futures"),
               df %>% filter(sudden_ask_change_f, !sudden_ask_change_s)) +
  xlab("Previous Sudden Ask Increases in the Other Market (1s)")


df %>% filter(sudden_ask_change_f) %>% 
  ggplot() + geom_density(aes(x=pre_sudden_ask_f, color = "Futures")) +
  geom_density(aes(x=pre_sudden_ask_s, color="Spot"))

df %>% filter(sudden_ask_change_s) %>% 
  ggplot() + geom_density(aes(x=pre_sudden_ask_f, color = "Futures")) +
  geom_density(aes(x=pre_sudden_ask_s, color="Spot"))


df %>% filter(pre_sudden_ask_s == 0, pre_sudden_ask_f == 0) %>%
  group_by(sudden_ask_change_s, sudden_ask_change_f) %>%
  summarise(avg_spot_max = mean(max_spot_return),
            avg_spot_min = mean(min_spot_return),
            avg_futures_max = mean(max_futures_return),
            avg_futures_min = mean(min_futures_return),
            cases = n()) %>% 
  rename(Spot_Ask = sudden_ask_change_s,
         Futures_Ask = sudden_ask_change_f
  ) %>% filter(Spot_Ask | Futures_Ask)


df %>% filter(pre_sudden_ask_s == 0, pre_sudden_ask_f == 0,
              pre_sudden_bid_s == 0, pre_sudden_bid_f == 0) %>%
  group_by(sudden_ask_change_s, sudden_ask_change_f) %>%
  summarise(avg_spot_max = mean(max_spot_return),
            avg_spot_min = mean(min_spot_return),
            avg_futures_max = mean(max_futures_return),
            avg_futures_min = mean(min_futures_return),
            cases = n()) %>% 
  rename(Spot_Ask = sudden_ask_change_s,
         Futures_Ask = sudden_ask_change_f
  ) %>% filter(Spot_Ask | Futures_Ask)
