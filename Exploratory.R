library(tidyverse)

op <- options(digits.secs=3)
df_futures <- read_csv("Archive/trb_usdt_futures_export.csv")
# df_futures$time - lag(df_futures$time), time diff in secs
df_spot <- read_csv("Archive/trb_usdt_spot_export.csv")
df_trades <- read_csv("Archive/trb_usdt_trades_export.csv")

