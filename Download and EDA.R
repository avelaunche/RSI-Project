library(tidyverse)
library(quantmod)
library(lubridate)
library(xts)
library(patchwork)

spy = read_csv("spy_daily_2020.csv")
head(spy)
spy = spy[, c(-9)]

#convert time to correct time
spy$timestamp <- ymd_hms(spy$timestamp, tz = "UTC")
spy$timestamp <- with_tz(spy$timestamp, tzone = "America/New_York")

spy = spy |>
  mutate(RSI = RSI(close, n = 14))

dim(spy)
summary(spy)

AAPL = filter(spy, symbol == "AAPL")

AAPL$pos = 1:nrow(AAPL)

ggplot(AAPL, aes(timestamp, open)) + 
  geom_line()

ggplot(AAPL, aes(timestamp, RSI)) + 
  geom_line()

AAPL = AAPL |>
  mutate(change = lag(close, 1)/open)

filter(AAPL, RSI < 20)

filter(AAPL, pos > 160 & pos < 180)

spy = spy |>
  mutate(change = lag(close, 1)/open)

spy = spy |>
  group_by(symbol) |>
  mutate(change = 1:n(), RSI = RSI(close, n = 20))

filter(spy, RSI < 20)

p1 = ggplot(filter(spy, symbol == "AAPL"), aes(timestamp, open)) + 
  geom_line()

p2 = ggplot(filter(spy, symbol == "AAPL"), aes(timestamp, RSI)) + 
  geom_line()

p1/p2

spy = mutate(spy, change = 1 - close/open)

mid_RSI = spy |>
  mutate(
    forward = lead(change, 1),
    backward = lag(change, 1)
  ) |>
  filter(RSI < 60 & RSI > 40)

low_RSI = spy |>
  mutate(
    forward = lead(change, 1),
    backward = lag(change, 1)
  ) |>
  filter(RSI < 30 & RSI > 20)

time_forward = 50

#future look

mean(spy$close)

future_RSI_low = spy |>
  group_by(symbol) |>
  mutate(
    forward = lead(close, time_forward)/close
  ) |>
  filter(RSI < 30 & RSI > 20)

future_RSI_mid = spy |>
  group_by(symbol) |>
  mutate(
    forward = lead(close, time_forward)/close
  ) |>
  filter(RSI < 60 & RSI > 40)

future_RSI_high = spy |>
  group_by(symbol) |>
  mutate(
    forward = lead(close, time_forward)/close
  ) |>
  filter(RSI > 70 & RSI < 80)

ggplot(mid_RSI, aes(change)) + 
  geom_histogram(bins = 50) + 
  labs(
    title = "Average Change on Days with Low RSI"
  )

ggplot(mid_RSI, aes(forward)) + 
  geom_histogram(bins = 50) + 
  labs(
    title = "Average Change the Day After a Day with Low RSI"
  )

ggplot(mid_RSI, aes(backward)) + 
  geom_histogram(bins = 50) + 
  labs(
    title = "Average Change the Day Before a Day with Low RSI"
  )

ggplot(low_RSI, aes(change)) + 
  geom_histogram(bins = 50)

ggplot(low_RSI, aes(forward)) + 
  geom_histogram(bins = 50)

ggplot(future_RSI_low, aes(forward)) + 
  geom_histogram(bins = 50)

ggplot(future_RSI_mid, aes(forward)) + 
  geom_histogram(bins = 50)

ggplot(future_RSI_high, aes(forward)) + 
  geom_histogram(bins = 50)

ggplot(spy, aes(RSI)) + 
  geom_histogram()

median(future_RSI_mid$forward[!is.na(future_RSI_mid$forward)])
median(future_RSI_low$forward[!is.na(future_RSI_low$forward)])
median(future_RSI_high$forward[!is.na(future_RSI_high$forward)])


