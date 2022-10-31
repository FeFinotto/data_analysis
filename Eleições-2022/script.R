library(tidyverse)
library(jsonlite)
library(scales)
library(ggtext)
library(httr)
library(rtweet)
library(ggrepel)

# Settings ----------------------------------------------------------------

url_resultados_2turno <- "https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/br/br-c0001-e000545-r.json"

# auth <- auth_setup_default()
# 
# auth <- rtweet_app()
# auth_as(auth)
# 
# auth_save(auth, "twitter-auth-fefinotto")
# 
# auth_as("twitter-auth-fefinotto")
# 
# my_token <- rtweet::get_token()


# Getting data ------------------------------------------------------------

# Porcentagem de urnas apuradas
pct_urnas_apuradas <- fromJSON(url_resultados_2turno, simplifyDataFrame = TRUE) %>% 
  .[["pst"]] %>% 
  as_tibble() %>% 
  transmute(pct_urnas_apuradas = parse_double(str_replace(value, ",", "."))/100)

# Últimos resultados
ultimos_resultados <- fromJSON(url_resultados_2turno, simplifyDataFrame = TRUE) %>% 
  .[["cand"]] %>% 
  as_tibble() %>% 
  select(n, nm, vap, pvap) %>% 
  arrange(desc(pvap)) %>%
  mutate(pvap = parse_double(str_replace(pvap, ",", "."))/100,
         vap = parse_integer(vap)/100,
         pct_urnas_apuradas)

# Criando base falsa
# historical_df <- tibble(n = as.character(),
#                         nm = as.character(),
#                         vap = as.integer(),
#                         pvap = as.double(),
#                         pct_urnas_apuradas = as.double())

# Binding data frames
df <- bind_rows(df, ultimos_resultados)

# Plotting chart ----------------------------------------------------------

## Create temporary file name
chart <- tempfile(fileext = ".png")

## Save as png
png(chart, 800, 600, "px", res = 127.5)

# Plotting
df %>% 
  filter(n %in% c(13, 22)) %>% 
  ggplot(aes(pct_urnas_apuradas, pvap, color = nm)) +
  geom_line(size = 1) +
  geom_text_repel(aes(label = str_c(pvap*100, "%")), 
                  data = ultimos_resultados, 
                  direction = "x",
                  nudge_x = .01) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, by = 0.25),
                     limits = c(0, 1.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.4, 0.6)) +
  labs(title = "<span style='color:blue;'>**Jair Bolsonaro**</span> x <span style='color:red;'>**Lula**</span>",
       subtitle = str_c("2º turno eleições 2022. Urnas apuradas: ", pct_urnas_apuradas*100, "%"),
       caption = "Fonte: TSE",
       x = "Porcentagem das urnas apuradas",
       y = "Porcentagem dos votos válidos") +
  scale_color_manual(values = c("JAIR BOLSONARO" = "blue", 
                                "LULA" = "red")) +
  theme(plot.title = element_markdown(family = "sans",
                                      hjust = .05,
                                      size = 16),
        plot.subtitle = element_markdown(hjust = 0.05),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

dev.off()

# Posting in Twitter ------------------------------------------------------

## post tweet with media attachment
post_tweet("Resultado FINAL das eleições presidenciais de 2022 - Lula vs Bolsonaro",
           chart,
           media_alt_text = "resultado eleições 2022",
           token = my_token)

## lookup status_id
my_tweets <- get_timeline(user = "fefinotto", token = my_token)
reply_id <- my_tweets$id_str[1]

## post reply
post_tweet("Resultado atualizado das eleições presidenciais de 2022 - Lula vs Bolsonaro",
           media = chart,
           media_alt_text = "test",
           in_reply_to_status_id = reply_id,
           token = my_token)
