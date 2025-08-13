# MapIO-BH - GT6 #
# Análises de clima e óbitos - Agosto/2025 #





## Limpar o ambiente ####

rm(list=ls())





## Diretório ####

setwd("C:/Users/amand/Amanda/GitHub/mapio_gt6")
getwd()





## Pacotes ####

pacman::p_load(
  # Importação de dados
  "haven",
  # Manipulação de dados
  "dplyr", "lubridate", "tidyr",
  # Visualização de dados
  "ggplot2",
  # Modelagem estatística
  "splines", "lme4", "glmmTMB"
)





## Dados ####

dta <- read_dta("C:/Users/amand/Amanda/GitHub/mapio_gt6/clima_obitos.dta")
attach(dta)





## Descritiva ####


# Data

summary(dta$data)

dta %>%
  mutate(ano = year(data)) %>%
  count(ano) %>%
  arrange(ano)

dta %>%
  mutate(mes = month(data)) %>%
  count(mes) %>%
  arrange(mes)


# Temperatura máxima (°C)

summary(dta$temp_max)


# Temperatura média (°C)

summary(dta$temp_media)


# Temperatura mínima (°C)

summary(dta$temp_min)


# Amplitude térmica (°C)

summary(dta$amplitude_termica)


# Ponto de orvalho (°C)

summary(dta$orvalho)


# Umidade relativa média (%)

summary(dta$umidade_relativa_media)


# Umidade relativa mínima (%)

summary(dta$umidade_relativa_min)


# Velocidade máxima do vento (m/s)

summary(dta$velocidade_vento_max)


# Velocidade média do vento (m/s)

summary(dta$velocidade_vento_media)


# Pressão atmosférica média (hPa)

summary(dta$pressao_atm_media)


# Precipitação total (mm)

summary(dta$precipitacao_total)


# Média mensal da temperatura máxima (°C)

summary(dta$temp_max_mes)


# Média mensal da temperatura máxima mais 5°C

summary(dta$temp_max_mes_mais5C)


# Média mensal da temperatura mínima (°C)

summary(dta$temp_min_mes)


# Média mensal da temperatura mínima menos 5°C

summary(dta$temp_min_mes_menos5C)


# Óbitos em mulheres

summary(dta$obitos_feminino)

tab <- table(dta$obitos_feminino)
data.frame(`Óbitos` = as.numeric(names(tab)),
           Freq = as.vector(tab),
           Percent = round(100 * as.vector(tab) / sum(tab), 1))


# Óbitos em homens

summary(dta$obitos_masculino)

tab <- table(dta$obitos_masculino)
data.frame(`Óbitos` = as.numeric(names(tab)),
           Freq = as.vector(tab),
           Percent = round(100 * as.vector(tab) / sum(tab), 1))


# Óbitos infantis

summary(dta$obitos_infantil)

tab <- table(dta$obitos_infantil)
data.frame(`Óbitos` = as.numeric(names(tab)),
           Freq = as.vector(tab),
           Percent = round(100 * as.vector(tab) / sum(tab), 1))


# Óbitos totais

summary(dta$obitos_total)

tab <- table(dta$obitos_total)
data.frame(`Óbitos` = as.numeric(names(tab)),
           Freq = as.vector(tab),
           Percent = round(100 * as.vector(tab) / sum(tab), 1))


### Plot temperaturas ####

dir.create("plots_temp", showWarnings = FALSE)

dta_long <- dta %>%
  select(temp_min, temp_media, temp_max) %>%
  pivot_longer(cols = everything(),
               names_to = "tipo",
               values_to = "temperatura") %>%
  mutate(tipo = factor(tipo, levels = c("temp_min", "temp_media", "temp_max"),
                       labels = c("Mínima", "Média", "Máxima")))

ggplot(dta_long, aes(x = tipo, y = temperatura, fill = tipo)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Mínima" = "#253C9C", 
                               "Média" = "#35b779",
                               "Máxima" = "#D64933")) +
  scale_y_continuous(name = "Temperatura (°C) diária \n", 
                     breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(x = "", y = "Temperatura (°C)")

ggsave(filename = file.path("plots_temp", "temp.jpg"),
       width = 21, height = 12, units = "in", dpi = 300)


### Plots temperaturas diárias por ano ####

dta_long <- dta %>%
  select(data, temp_min, temp_media, temp_max) %>%
  pivot_longer(cols = c(temp_min, temp_media, temp_max),
               names_to = "tipo",
               values_to = "temperatura") %>%
  mutate(tipo = factor(tipo, 
                       levels = c("temp_min", "temp_media", "temp_max"),
                       labels = c("Mínima", "Média", "Máxima")),
         ano = year(data))

anos <- unique(dta_long$ano)

for(a in anos){
  
  dta_ano <- dta_long %>% filter(ano == a)
  
  p <- ggplot(dta_ano, aes(x = data, y = temperatura, color = tipo)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Mínima" = "#253C9C", 
                                  "Média" = "#35b779",
                                  "Máxima" = "#D64933")) +
    scale_y_continuous(name = "Temperatura (°C) diária \n", breaks = seq(0, 40, 5), limits = c(0, 40)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
    theme_minimal(base_size = 14) +
    labs(x = "\n Data", y = "Temperatura (°C) \n", color = "Temperatura (°C)")
  
  ggsave(filename = file.path("plots_temp", 
                              paste0("temp_", a, ".jpg")),
         plot = p,
         width = 21, height = 12, units = "in", dpi = 300)
  
  print(p)
}


### Plots temperaturas e precipitação diárias por ano ####

dir.create("plots_temp_prec", showWarnings = FALSE)

dta_long <- dta %>%
  select(data, temp_min, temp_media, temp_max, precipitacao_total) %>%
  pivot_longer(cols = c(temp_min, temp_media, temp_max),
               names_to = "tipo",
               values_to = "temperatura") %>%
  mutate(tipo = factor(tipo, 
                       levels = c("temp_min", "temp_media", "temp_max"),
                       labels = c("Mínima", "Média", "Máxima")),
         ano = year(data))

anos <- unique(dta_long$ano)

for(a in anos){
  
  dta_ano <- dta_long %>% filter(ano == a)
  
  escala_prec <- 200 / 40
  
  p <- ggplot() +
    geom_col(data = dta_ano %>% distinct(data, precipitacao_total), 
             aes(x = data, y = precipitacao_total / escala_prec, fill = "Precipitação"), 
             alpha = 1) +
    geom_line(data = dta_ano, aes(x = data, y = temperatura, color = tipo), 
              size = 1, alpha = 0.4) +
    scale_color_manual(values = c("Mínima" = "#253C9C", 
                                  "Média" = "#35b779",
                                  "Máxima" = "#D64933"),
                       name = "Temperatura (°C)") +
    scale_fill_manual(values = c("Precipitação" = "grey20"),
                      name = "Precipitação (mm)") +
    scale_y_continuous(name = "Temperatura (°C) diária \n", 
                       breaks = seq(0, 40, 5), limits = c(0, 40),
                       sec.axis = sec_axis(~ . * escala_prec, 
                                           name = "\n Precipitação (mm) diária \n",
                                           breaks = seq(0, 200, 20))) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
    theme_minimal(base_size = 14) +
    labs(x = "\n Data", y = "Temperatura (°C) \n") +
    theme(legend.position = "right")
  
  ggsave(filename = file.path("plots_temp_prec", 
                              paste0("temp_prec_", a, ".jpg")),
         plot = p,
         width = 21, height = 12, units = "in", dpi = 300)
  
  print(p)
}


### Plots temperaturas e óbitos diários por ano ####

dir.create("plots_temp_obit", showWarnings = FALSE)

dta_long <- dta %>%
  select(data, temp_min, temp_media, temp_max, obitos_total) %>%
  pivot_longer(cols = c(temp_min, temp_media, temp_max),
               names_to = "tipo",
               values_to = "temperatura") %>%
  mutate(tipo = factor(tipo, 
                       levels = c("temp_min", "temp_media", "temp_max"),
                       labels = c("Temperatura mínima", 
                                  "Temperatura média", 
                                  "Temperatura máxima")),
         ano = year(data))

anos <- unique(dta_long$ano)

for(a in anos){
  
  dta_ano <- dta_long %>% filter(ano == a)
  
  escala_obitos <- 200 / 40
  
  dta_ano <- dta_ano %>% mutate(obitos_escala = obitos_total / escala_obitos)
  
  p <- ggplot() +
    geom_smooth(data = dta_ano, aes(x = data, y = obitos_escala, color = "Óbitos totais"), 
                method = "loess", se = FALSE, span = 0.2, size = 1) +
    geom_smooth(data = dta_ano, aes(x = data, y = temperatura, color = tipo), 
                method = "loess", se = FALSE, span = 0.2, size = 1, alpha = 0.4) +
    scale_color_manual(values = c("Temperatura mínima" = "#253C9C", 
                                  "Temperatura média" = "#35b779",
                                  "Temperatura máxima" = "#D64933",
                                  "Óbitos totais" = "grey20"),
                       name = "Temperatura e óbitos suavizados (Loess)") +
    scale_y_continuous(name = "Temperatura (°C) diária \n", 
                       breaks = seq(0, 40, 5), limits = c(0, 40),
                       sec.axis = sec_axis(~ . * escala_obitos, 
                                           name = "\n Óbitos diários\n",
                                           breaks = seq(0, 200, 20))) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
    theme_minimal(base_size = 14) +
    labs(x = "\n Data", y = "Temperatura (°C) \n") +
    theme(legend.position = "right")
  
  ggsave(filename = file.path("plots_temp_obit",
                              paste0("temp_obit_", a, ".jpg")),
         plot = p,
         width = 21, height = 12, units = "in", dpi = 300)
  
  print(p)
}



















