library(tidyquant)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)

tickers <- c("GOOGL", "^VIX")
raw_data <- tq_get(tickers, 
                   get  = "stock.prices", 
                   from = "2004-08-18", 
                   to   = "2023-12-31")

df_final <- raw_data %>%
  select(date, symbol, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  rename(VIX = `^VIX`) %>%
  arrange(date) %>%
  mutate(
    Ret_GOOGL = (GOOGL - lag(GOOGL)) / lag(GOOGL),
    Delta_VIX = VIX - lag(VIX)
  ) %>%
  drop_na()

raw_mom <- read_csv("F-F_Momentum_Factor_daily.csv", col_names = FALSE)

df_mom_clean <- raw_mom %>%
  unite("brut", everything(), sep = " ", na.rm = TRUE) %>%
  mutate(brut = str_replace_all(brut, ",", " ")) %>%
  filter(str_detect(brut, "^(19|20)\\d{6}")) %>%
  extract(brut, 
          into = c("date_raw", "Momentum_perc"), 
          regex = "^(\\d{8})\\s+([\\-\\.\\d]+)") %>%
  mutate(
    date = ymd(date_raw),
    Momentum_perc = as.numeric(Momentum_perc),
    WML = ifelse(Momentum_perc <= -99, NA, Momentum_perc / 100)
  ) %>%
  select(date, WML) %>%
  drop_na()

raw_ff3 <- read_csv("F-F_Research_Data_Factors_daily.csv", skip = 4, show_col_types = FALSE)

df_ff3_clean <- raw_ff3 %>%
  rename(date_raw = 1) %>%
  mutate(date_raw = as.character(date_raw)) %>%
  filter(str_detect(date_raw, "^(19|20)\\d{6}$")) %>%
  
  mutate(
    date = ymd(date_raw),
    across(
      .cols = c(`Mkt-RF`, SMB, HML, RF), 
      .fns = ~ ifelse(.x <= -99, NA, .x / 100)
    )
  ) %>%
  select(date, `Mkt-RF`, SMB, HML, RF) %>%
  drop_na()

base_regression <- df_final %>%
  inner_join(df_ff3_clean, by = "date") %>%
  inner_join(df_mom_clean, by = "date") %>%
  mutate(
    Ret_Exc_GOOGL = Ret_GOOGL - RF
  )
head(base_regression)

write_csv(file = "Final_data",base_regression)
