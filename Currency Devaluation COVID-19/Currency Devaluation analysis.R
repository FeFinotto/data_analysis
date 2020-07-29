
# About this script -------------------------------------------------------

# This script was made to compare the devaluation of Brazilian Real and other
# currencies vs US Dollar since the beginning of the year
# Author: Fernando Finotto Visani
# Date: May 27, 2020

# https://www.quantmod.com/examples/data/
# https://www.datacamp.com/community/blog/r-xts-cheat-sheet
# https://rdrr.io/cran/quantmod/man/getFX.html
# https://stackoverflow.com/questions/26694042/how-to-get-currency-exchange-rates-in-r
# https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
# https://books.google.com.co/books?id=WUjWDwAAQBAJ&pg=PT142&lpg=PT142&dq=getfx+examples+r&source=bl&ots=-d_OfXO1IP&sig=ACfU3U1uaufDKUCMIce22XhZdka8xe42Sw&hl=en&sa=X&ved=2ahUKEwiQzMSVrunpAhXPVN8KHcHbBeQQ6AEwCHoECAoQAQ#v=onepage&q=getfx%20examples%20r&f=false

# https://bizfluent.com/how-6704791-calculate-currency-depreciation.html

# Loading libraries -------------------------------------------------------

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
# rm(list = ls())

library(tidyverse)
library(quantmod)
library(lubridate)
library(xts)
library(janitor)
library(gghighlight)
library(ggtext)
library(scales)
library(ggdark)
library(rvest)
library(ggtext)
library(ggcharts)

# Settings ----------------------------------------------------------------

url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_wikipedia <- "https://en.wikipedia.org/wiki/List_of_circulating_currencies#List_of_circulating_currencies_by_state_or_territory"
start_date <- max(exchange_rates$date) + days(1)

# Getting all affected countries by COVID-19 ------------------------------

confirmed_cases <- read_csv(url(url_confirmed))

confirmed_countries <- confirmed_cases %>%
  clean_names() %>% 
  select(country = country_region) %>% 
  distinct()

# Getting list of countries and respective currencies  -------------------------------

exclude_rows <- c(1, 6, 8, 16, 32, 41, 59, 65, 74, 76, 82, 86, 101, 116, 122, 127, 137, 164, 175, 183, 184, 191, 200:202, 216, 246, 250)

# Scrapping Wikipedia table for Countries and currencies
wikipedia <- read_html(url_wikipedia)

countries_currencies <- html_table(wikipedia, fill = TRUE)[[1]] %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(row_n = row_number()) %>% 
  filter(!row_n %in% exclude_rows, # ecluding countries that have more than one currency
         !iso_code_2 %in% c("USD", "(none)", "SSP", "VES")) %>% # excluding countries that use USD as main currency and others for data cleaning
  select(country = state_or_territory_1,
         currencies_from = iso_code_2) %>% 
  mutate(currencies_from = str_sub(currencies_from, 1, 3),
         currencies_list = paste(currencies_from, "USD", sep = "/"))

# Joining confirmed countries and respective currencies -------------------

final_currencies <- confirmed_countries %>% 
  inner_join(countries_currencies, by = "country") %>% 
  group_by(currencies_from, currencies_list) %>% 
  summarise(n = n()) %>% 
  distinct() %>% 
  select(-n)

currencies_list <- as.list(final_currencies$currencies_list)

# Getting exchange rates historical data ----------------------------------

new_exchange_rates <- tibble(curr_to = as.character(),
                             curr_from = as.character(),
                             date = ymd(),
                             exchange_rate = as.double())

for (currency in currencies_list) {
  
  tmp <- getFX(currency, from = start_date) # I keep record of historical data and use it for the start_date
  tmp <- str_replace(tmp, "/", "")
  
  trend <- to.daily(as.xts(get(tmp)))
  trend <- data.frame(trend$`as.xts(get(tmp)).Close`, date = as.Date(index(trend)))
  
  trend2 <- trend %>% 
    as_tibble() %>% 
    pivot_longer(-date, names_to = "currency", values_to = "exchange_rate") %>% 
    mutate(currency = tmp,
           curr_from = str_sub(currency, 1, 3),
           curr_to = str_sub(currency, 4, 6)) %>% 
    select(curr_to, curr_from, date, exchange_rate)
  
  new_exchange_rates <- bind_rows(new_exchange_rates, trend2)
  
  rm(tmp)
}

# Data transformation -----------------------------------------------------

# Getting the value from January 1st as the initial exchange rate for devaluation comparison
exchange_rates <- exchange_rates %>% 
  bind_rows(new_exchange_rates) %>% 
  arrange(curr_to, curr_from, date) %>% 
  mutate(initial_ex_rate = ifelse(date == "2020-01-01", exchange_rate, factor(NA))) %>% 
  fill(initial_ex_rate, .direction = "down") %>% 
  mutate(ex_rate_depreciation = initial_ex_rate / exchange_rate - 1) %>% 
  filter(date < Sys.Date())

# Summarizing data for the average of all currencies but BRL
exchange_rates_mean <- exchange_rates %>% 
  filter(curr_from != "BRL") %>% 
  group_by(curr_to, date) %>% 
  summarise(exchange_rate = mean(exchange_rate, na.rm = TRUE),
            curr_from = "AVG") %>% 
  ungroup() %>% 
  mutate(initial_ex_rate = ifelse(date == "2020-01-01", exchange_rate, factor(NA))) %>% 
  fill(initial_ex_rate, .direction = "down") %>% 
  mutate(ex_rate_depreciation = initial_ex_rate / exchange_rate - 1) %>% 
  filter(date < Sys.Date()) %>% 
  select(curr_to, curr_from, date, exchange_rate, initial_ex_rate, ex_rate_depreciation) %>% 
  bind_rows(filter(exchange_rates, curr_from == "BRL"))
  

# Plotting Data -----------------------------------------------------------

# This plot compares all currencies from countries affected by COVID-19 and highlights
# Brazilian as one that has the worst devaluation vs US Dollars
exchange_rates %>%
  ggplot(aes(date, ex_rate_depreciation, group = curr_from)) +
  geom_line(color = "#C3423F", size = 1, show.legend = FALSE) +
  gghighlight(curr_from == "BRL", use_direct_label = FALSE, unhighlighted_params = list(size = .5, color = "#706C61")) +
  theme_ng() +
  labs(title = "How much the <span style='color:#C3423F'>Brazilian Real</span> depreciated compared to US Dollars",
       subtitle = "This chart compares all countries that have confirmed COVID-19 cases and analyzes how much their currencies \ndepreciated compared to US Dollar since January 1st, 2020",
       x = "",
       y = "Currency Devaluation") +
  theme(plot.title = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  # ggsave("BRL_devaluation.png", width = 40, height = 20, units = "cm")
  
# PT-BR
exchange_rates %>%
  ggplot(aes(date, ex_rate_depreciation, group = curr_from)) +
  geom_line(color = "#C3423F", size = 1, show.legend = FALSE) +
  gghighlight(curr_from == "BRL", use_direct_label = FALSE, unhighlighted_params = list(size = .5, color = "#706C61")) +
  # geom_vline(xintercept = as.numeric(as.Date("2020-02-26"))) +
  theme_ng() +
  labs(title = "Quanto o <span style='color:#C3423F'>Real</span> desvalorizou com relação ao Dólar",
       subtitle = "Esse gráfico compara as moedas de todos os países afetados pela COVID-19 e analisa o quanto \nelas foram desvalorizadas com relação ao Dólar desde 1º de Janeiro de 2020",
       x = "",
       y = "Desvalorização da Moeda vs Dólar") +
  theme(plot.title = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  # ggsave("BRL_devaluation_ptbr.png", width = 40, height = 20, units = "cm")

# Getting average for all currencies but BRL and adding what happened in Brazil in the pikes 
events <- tribble(
  ~date,        ~text,
  "2020-02-26", "26-Fev: Primeiro caso de COVID-19",
  "2020-03-19", "19-Mar: 7 mortes por COVID",
  "2020-04-04", "4-Abr: 432 mortes",
  "2020-04-16", "16-Abr: Saída Mandetta do MS\n1.900 mortes",
  "2020-04-24", "24-Abr: Saída Moro do governo\n3.600 mortes",
  "2020-05-15", "15-Mai: Saída Teich do MS\n14.800 mortes",
  "2020-06-05", "5-Jun: Bolsonaro atrasa dados \nda COVID - 35.000 mortes"
  ) %>% 
  mutate(date = parse_date(date))

ggplot(exchange_rates_mean, aes(date)) +
  geom_line(aes(y = ex_rate_depreciation, group = curr_from), color = "#C3423F", size = 1, show.legend = FALSE) +
  gghighlight(curr_from == "BRL", use_direct_label = FALSE, unhighlighted_params = list(size = 1, color = "#7FB685")) +
  geom_vline(data = events, aes(xintercept = as.numeric(date)), linetype = "dashed", size = .25) +
  geom_text(data = events, mapping = aes(label = text, x = date, y = .15), angle = 90, vjust = -.3) +
  theme_ng() +
  labs(title = "Desvalorização média de <span style='color:#7FB685'>outras moedas</span> e do <span style='color:#C3423F'>Real</span> vs Dólar",
       subtitle = "Esse gráfico compara a desvalorização média das moedas dos países afetados pela COVID-19 vs Real com relação ao dólar.\nRelacionamos com os principais acontecimentos no Brasil no período.",
       x = "",
       y = "Desvalorização da Moeda vs Dólar") +
  theme(plot.title = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  # ggsave("BRL_devaluation_ptbr_events.png", width = 40, height = 20, units = "cm")






