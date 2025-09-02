# MapIO-BH - GT6 #
# Análises de clima e óbitos - Agosto/2025 #





## Limpar o ambiente ####

rm(list=ls())





## Diretório ####

setwd("C:/Users/amand/Amanda/GitHub/mapio_gt6")
#setwd("C:/Users/amanda/Downloads")
getwd()





## Pacotes ####

pacman::p_load(
  # Importação de dados
  "haven",
  # Manipulação de dados
  "dplyr", "lubridate", "tidyr",
  # Visualização de dados
  "ggplot2", "patchwork",
  # Correlação
  "Hmisc", "reshape2",
  # Modelagem
  "dlnm", "gnm", "splines", "MuMIn")





## Dados ####

dta <- read_dta("clima_obitos.dta")
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


# Temperatura mínima (°C)
summary(dta$temp_min)


# Temperatura média (°C)
summary(dta$temp_media)


# Temperatura máxima (°C)
summary(dta$temp_max)


# Amplitude térmica (°C)
summary(dta$amplitude_termica)


# Ponto de orvalho (°C)
summary(dta$orvalho)


# Umidade relativa mínima (%)
summary(dta$umidade_relativa_min)


# Umidade relativa média (%)
summary(dta$umidade_relativa_media)


# Velocidade média do vento (m/s)
summary(dta$velocidade_vento_media)


# Velocidade máxima do vento (m/s)
summary(dta$velocidade_vento_max)


# Pressão atmosférica média (hPa)
summary(dta$pressao_atm_media)


# Precipitação total (mm)
summary(dta$precipitacao_total)


# Óbitos em mulheres
summary(dta$obitos_feminino)


# Óbitos em homens
summary(dta$obitos_masculino)


# Óbitos infantis
summary(dta$obitos_infantil)


# Óbitos totais

summary(dta$obitos_total)

dta <- dta %>%
  mutate(ano = year(data),
         grupo_anos = case_when(ano >= 2014 & ano <= 2019 ~ "2014-2019",
                                ano >= 2020 & ano <= 2022 ~ "2020-2022",
                                ano >= 2023 & ano <= 2024 ~ "2023-2024",
                                TRUE ~ NA_character_))

dta %>%
  filter(!is.na(grupo_anos)) %>%
  group_by(grupo_anos) %>%
  summarise(total_obitos = sum(obitos_total, na.rm = TRUE), .groups = "drop") %>%
  mutate(percent = round(100 * total_obitos / sum(total_obitos), 1))



### Plot temperatura média ####


# Plot 1

p <- ggplot(dta, aes(x = temp_media)) +
  geom_histogram(bins = 20, fill = "gray45", color = "white") +
  xlab("\n Temperatura média (°C)") +
  ylab("Frequência \n") +
  ggtitle("") +
  scale_y_continuous(breaks = seq(0, 800, by = 100), limits = c(0, 800)) +
  theme_minimal(base_size = 14)
p

ggsave(filename = file.path("plots_temp", "temp_media.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 2

p <- ggplot(dta, aes(x = temp_max)) +
  geom_histogram(bins = 20, fill = "gray45", color = "white") +
  xlab("\n Temperatura máxima (°C)") +
  ylab("Frequência \n") +
  ggtitle("") +
  scale_y_continuous(breaks = seq(0, 800, by = 100), limits = c(0, 800)) +
  theme_minimal(base_size = 14)
p

ggsave(filename = file.path("plots_temp", "temp_max.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 3

p <- ggplot(dta, aes(x = temp_media)) +
  geom_histogram(bins = 20, fill = "gray45", color = "white") +
  xlab("\n Temperatura média (°C)") +
  ylab("Frequência \n") +
  scale_y_continuous(breaks = seq(0, 400, by = 100), limits = c(0, 400)) +
  facet_wrap(~ grupo_anos, ncol = 1) +
  theme_minimal(base_size = 14)
p

ggsave(filename = file.path("plots_temp", "temp_media_periodos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 4

p <- ggplot(dta, aes(x = temp_max)) +
  geom_histogram(bins = 20, fill = "gray45", color = "white") +
  xlab("\n Temperatura máxima (°C)") +
  ylab("Frequência \n") +
  scale_y_continuous(breaks = seq(0, 400, by = 100), limits = c(0, 400)) +
  facet_wrap(~ grupo_anos, ncol = 1) +
  theme_minimal(base_size = 14)
p

ggsave(filename = file.path("plots_temp", "temp_mmax_periodos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)



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



### Plots temperaturas por ano ####


# Plot 1

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
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Mínima" = "#253C9C", 
                                  "Média" = "#35b779",
                                  "Máxima" = "#D64933")) +
    scale_y_continuous(name = "Temperatura (°C) diária \n", 
                       breaks = seq(0, 40, 5), limits = c(0, 40)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
    theme_minimal(base_size = 14) +
    labs(x = "\n Data", y = "Temperatura (°C) \n", color = "Temperatura (°C)")
  
  ggsave(filename = file.path("plots_temp", 
                              paste0("temp_", a, ".jpg")),
         plot = p,
         width = 21, height = 12, units = "in", dpi = 300)
  
  print(p)}


# Plot 2

p <- ggplot(dta_long, aes(x = data, y = temperatura, color = tipo)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Mínima" = "#253C9C", 
                                "Média" = "#35b779",
                                "Máxima" = "#D64933")) +
  scale_y_continuous(name = "Temperatura (°C) diária \n", 
                     breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 14) +
  labs(x = "\n Ano", y = "Temperatura (°C) \n", color = "Temperatura (°C)")
p

ggsave(filename = file.path("plots_temp", "temp_anos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 3

dir.create("plots_temp", showWarnings = FALSE)

dta_long <- dta %>%
  select(grupo_anos, temp_min, temp_media, temp_max) %>%
  pivot_longer(cols = c(temp_min, temp_media, temp_max),
               names_to = "tipo",
               values_to = "temperatura") %>%
  mutate(tipo = factor(tipo, levels = c("temp_min", "temp_media", "temp_max"),
                       labels = c("Mínima", "Média", "Máxima")))

ggplot(dta_long, aes(x = tipo, y = temperatura, fill = tipo)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Mínima" = "#253C9C", 
                               "Média" = "#35b779",
                               "Máxima" = "#D64933")) +
  facet_wrap(~ grupo_anos) +
  scale_y_continuous(name = "Temperatura (°C) diária \n", 
                     breaks = seq(0, 40, by = 5), 
                     limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(x = "", y = "Temperatura (°C)")

ggsave(filename = file.path("plots_temp", "temp_periodos.jpg"),
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 4

dta$data <- as.Date(dta$data)  

p <- ggplot(dta, aes(x = data, y = temp_media)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("\n Anos") +
  ylab("Temperatura média (°C) \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 0.5))
p

ggsave(filename = file.path("plots_temp", "temp_media_anos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 5

dta$data <- as.Date(dta$data)  

p <- ggplot(dta, aes(x = data, y = temp_max)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("\n Anos") +
  ylab("Temperatura máxima (°C) \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 0.5))
p

ggsave(filename = file.path("plots_temp", "temp_max_anos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)



### Plots temperaturas e precipitação por ano ####

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
              linewidth = 1, alpha = 0.4) +
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
  
  print(p)}



### Plots óbitos por ano ####


# Plot 1
p <- ggplot(dta, aes(x = data, y = obitos_total)) + 
  geom_point(color = "gray45", size = 1.5) + xlab("\n Anos") + 
  ylab("Óbitos totais \n") + ggtitle("") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  scale_y_continuous(breaks = seq(0, 150, by = 50), limits = c(0, 150)) + 
  theme_minimal(base_size = 14) + 
  theme(axis.text.x = element_text(hjust = 1))
p

ggsave(filename = file.path("plots_temp_obit", "obitos_anos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 2

p <- ggplot(dta, aes(x = grupo_anos, y = obitos_total, fill = grupo_anos)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("2014-2019" = "gray60", 
                               "2020-2022" = "gray75", 
                               "2023-2024" = "gray90")) +
  scale_y_continuous(name = "Óbitos totais \n", 
                     breaks = seq(0, 150, by = 25), 
                     limits = c(0, 150)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(x = "", y = "Óbitos totais")
p

ggsave(filename = file.path("plots_temp_obit", "obitos_periodos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)



### Plots temperaturas e óbitos por ano ####


# Plot 1

dir.create("plots_temp_obit", showWarnings = FALSE)

dta_long <- dta %>%
  select(data, temp_min, temp_media, temp_max, obitos_total) %>%
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

  escala_obitos <- 200 / 40
  
  p <- ggplot() +
    geom_col(data = dta_ano %>% distinct(data, obitos_total), 
             aes(x = data, y = obitos_total / escala_obitos, fill = "Óbitos totais"), 
             alpha = 1) +
    geom_line(data = dta_ano, aes(x = data, y = temperatura, color = tipo), 
              size = 1, alpha = 0.4) +
    scale_color_manual(values = c("Mínima" = "#253C9C", 
                                  "Média" = "#35b779",
                                  "Máxima" = "#D64933"),
                       name = "Temperatura (°C)") +
    scale_fill_manual(values = c("Óbitos totais" = "grey20"),
                      name = "Óbitos") +
    scale_y_continuous(name = "Temperatura (°C) diária \n", 
                       breaks = seq(0, 40, 5), limits = c(0, 40),
                       sec.axis = sec_axis(~ . * escala_obitos, 
                                           name = "\n Óbitos diários \n",
                                           breaks = seq(0, 200, 20))) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y") +
    theme_minimal(base_size = 14) +
    labs(x = "\n Data", y = "Temperatura (°C) \n") +
    theme(legend.position = "right")
  
  ggsave(filename = file.path("plots_temp_obit", 
                              paste0("temp_obit_", a, ".jpg")),
         plot = p,
         width = 21, height = 12, units = "in", dpi = 300)
  
  print(p)}


# Plot 2

p1 <- ggplot(dta, aes(x = data, y = obitos_total)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("") +
  ylab("Óbitos totais \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 150, by = 50), limits = c(0, 150)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 0.5))

p2 <- ggplot(dta, aes(x = data, y = temp_media)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("\n Anos") +
  ylab("Temperatura média (°C) \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 0.5))

p_final <- p1 / p2

p_final

ggsave(filename = file.path("plots_temp_obit", "temp_media_obit_anos.jpg"),
       plot = p_final,
       width = 21, height = 12, units = "in", dpi = 300)


# Plot 3

p1 <- ggplot(dta, aes(x = data, y = obitos_total)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("") +
  ylab("Óbitos totais \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 150, by = 50), limits = c(0, 150)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 0.5))

p2 <- ggplot(dta, aes(x = data, y = temp_max)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("\n Anos") +
  ylab("Temperatura máxima (°C) \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 0.5))

p_final <- p1 / p2

p_final

ggsave(filename = file.path("plots_temp_obit", "temp_max_obit_anos.jpg"),
       plot = p_final,
       width = 21, height = 12, units = "in", dpi = 300)



### Plot correlação ####


# Remover colunas indesejadas e linhas com NAs, e converter para numérico
dta_limpo <- sapply(na.omit(dta[, !names(dta) %in% c(
  "data", "obitos_infantil", "grupo_anos", "strata")]), 
  as.numeric)

# Calcular correlação de Spearman
cor_res <- rcorr(dta_limpo, type = "spearman")
cor_matrix <- cor_res$r
p_values <- cor_res$P

# Criar matriz de texto com duas casas e asterisco para significativos
cor_text <- ifelse(!is.na(cor_matrix), 
                   paste0(formatC(cor_matrix, format = "f", digits = 2),
                          ifelse(p_values < 0.05, "*", "")), NA)

# Substituir diagonal
diag(cor_text) <- "1.00"

# Definir nomes das linhas/colunas
colnames(cor_matrix) <- rownames(cor_matrix) <- colnames(dta_limpo)
colnames(cor_text)   <- rownames(cor_text)   <- colnames(dta_limpo)

# Manter só a metade inferior
cor_matrix[upper.tri(cor_matrix)] <- NA
cor_text[upper.tri(cor_text)] <- NA

# Converter em data frame
cor_df <- melt(cor_matrix, value.name = "value")
cor_text_df <- melt(cor_text, value.name = "label")

cor_df$label <- cor_text_df$label

# Plot
ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(na.rm = TRUE) + 
  geom_text(aes(label = label), color = "black", size = 5, na.rm = TRUE) +
  scale_fill_gradient2(low = "#253C9C", mid = "white", high = "#D64933",
                       midpoint = 0, limits = c(-1, 1), na.value = NA) +  # nada para NA
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        panel.grid = element_line(color = "gray95")) +
  labs(x = "", y = "")

ggsave("correlacao.jpg", width = 21, height = 12, units = "in", dpi = 300)





## Modelos ####

### Modelo de Poisson condicional com termo DLNM para temperatura ####
# Poisson condicional: modelo para dados de contagem que controla sazonalidade 
# e tendências de longo prazo por meio de estratificação.
# DLNMs: Modelos não lineares de defasagem (lags) distribuída.
# Avaliam efeitos não lineares da temperatura em diferentes atrasos (lags).
# Objetivo: investigar a associação entre temperatura e mortalidade.
# Cada linha representa uma data (série temporal de mortalidade diária).



#### Strata ####
# Criamos uma variável de estrato (ano, mês e dia da semana) 
# para controlar sazonalidade e variação semanal.

dta <- dta |> 
  mutate(strata = paste(year(data),
                        month(data),
                        wday(data, label = TRUE), 
                        sep = ":") |> factor())



#### Knots #####
# Conjuntos de nós (knots) internos para splines cúbicos naturais usados
# para modelar de forma não linear a relação entre temperatura e mortalidade.
# Esses nós definem onde a curva spline pode “dobrar”,
# permitindo capturar variações na associação sem assumir linearidade.

# Nós para temperatura média
knots_media <- list(
  # https://doi.org/10.1038/s41591-022-01872-6
  knots_10_75_90 = quantile(dta$temp_media, c(10, 75, 90)/100, na.rm = TRUE),
  # https://doi.org/10.1038/s41591-023-02419-z
  knots_10_50_90 = quantile(dta$temp_media, c(10, 50, 90)/100, na.rm = TRUE),
  # https://doi.org/10.1590/0102-311XEN042524
  knots_25_50_90 = quantile(dta$temp_media, c(25, 50, 90)/100, na.rm = TRUE),
  #
  knots_50_75_90 = quantile(dta$temp_media, c(50, 75, 90)/100, na.rm = TRUE),
  # https://doi.org/10.1016/j.envres.2024.119347; https://doi.org/10.3390/atmos16020196
  knots_50_90 = quantile(dta$temp_media, c(50, 90)/100, na.rm = TRUE))



knots_media

# Nós para temperatura máxima
knots_max <- list(
  knots_10_75_90 = quantile(dta$temp_max, c(10, 75, 90)/100, na.rm = TRUE),
  knots_10_50_90 = quantile(dta$temp_max, c(10, 50, 90)/100, na.rm = TRUE),
  knots_25_50_90 = quantile(dta$temp_max, c(25, 50, 90)/100, na.rm = TRUE),
  knots_50_75_90 = quantile(dta$temp_media, c(50, 75, 90)/100, na.rm = TRUE),
  knots_50_90 = quantile(dta$temp_max, c(50, 90)/100, na.rm = TRUE))

knots_max



#### Boundary Knots #####
# Definir boundary knots explícitos (1º e 99º percentil da temperatura)
# Os boundary knots determinam os limites do spline cúbico natural,
# garantindo que a curva seja flexível dentro deste intervalo e linear fora,
# o que evita distorções causadas por valores extremos raros.

boundary_media_1_99 = quantile(dta$temp_media, c(.01, .99), na.rm = TRUE)

boundary_max_1_99 = quantile(dta$temp_max, c(.01, .99), na.rm = TRUE)



#### Lags #####
# Número máximo de dias de defasagem (lags), que permite avaliar tanto efeitos imediatos 
# quanto aqueles que surgem vários dias após a exposição à temperatura.

lags <- list(
  # https://doi.org/10.1038/s41591-022-01872-6
  lag_21 = c(1,21),
  # https://doi.org/10.1016/j.scs.2022.103758
  lag_14 = c(1,14),
  # https://doi.org/10.1093/aje/kwv260
  lag_10 = c(1,10),
  # https://doi.org/10.1016/j.envres.2024.119347
  lag_7 = c(1,7))

lags



#### knots para os splines de lags #####
# logknots(): concentra mais nós nos primeiros dias de defasagem,
#             capturando efeitos imediatos da temperatura.
# equalknots(): distribui os nós igualmente ao longo do período de lags,
#               útil para modelar efeitos graduais sem priorizar os primeiros dias.

# Nós logarítmicos concentrados nos primeiros dias
lags_knots_3 <- list(lag_21_knots_3 = logknots(lags$lag_21, nk = 3),
                     lag_14_knots_3 = logknots(lags$lag_14, nk = 3),
                     lag_10_knots_3 = logknots(lags$lag_10, nk = 3),
                     lag_7_knots_3 = logknots(lags$lag_7,  nk = 3))

# Nós igualmente espaçados
lags_knots_3_equal <- list(lag_21_knots_3_eq = equalknots(lags$lag_21, nk = 3),
                           lag_14_knots_3_eq = equalknots(lags$lag_14, nk = 3),
                           lag_10_knots_3_eq = equalknots(lags$lag_10, nk = 3),
                           lag_7_knots_3_eq = equalknots(lags$lag_7,  nk = 3))

# Combinar listas de nós 
lags_knots_list <- c(lags_knots_3, lags_knots_3_equal)



#### Plot lags e lags knots #####

# Criar dataframe com nós
df_knots <- bind_rows(lapply(names(lags_knots_list), function(knots_name) {
  knots <- lags_knots_list[[knots_name]]
  lag_label <- sub("_knots.*", "", knots_name)

  tipo_no <- ifelse(grepl("_eq$", knots_name), "Equalknots nk=3", "Lagknots nk=3")
  
  data.frame(
    lag = knots,
    tipo_plot = tipo_no,
    lag_label = lag_label)}))

# Criar dataframe dos lags
df_lags <- bind_rows(lapply(names(lags), function(lag_name) {
  data.frame(lag = lags[[lag_name]],
             tipo_plot = "Lags",
             lag_label = lag_name)}))

# Ajustar fatores
tipo_levels_plot <- c("Lags", "Lagknots nk=3", "Equalknots nk=3")
df_knots$tipo_plot <- factor(df_knots$tipo_plot, levels = tipo_levels_plot)
df_lags$tipo_plot   <- factor(df_lags$tipo_plot, levels = tipo_levels_plot)

df_knots$lag_label <- factor(df_knots$lag_label, levels = c("lag_7", "lag_10", "lag_14", "lag_21"))
df_lags$lag_label <- factor(df_lags$lag_label, levels = c("lag_7", "lag_10", "lag_14", "lag_21"))

# Plot
ggplot() +
  geom_point(data = df_lags, aes(x = lag, y = 1, color = tipo_plot), size = 2) +
  geom_point(data = df_knots, aes(x = lag, y = 1, color = tipo_plot), size = 4, shape = 15) +
  scale_color_manual(values = c("Lags" = "#253C9C",
                                "Lagknots nk=3" = "#D64933",
                                "Equalknots nk=3" = "#FFB400")) +
  scale_x_continuous(breaks = seq(1, max(unlist(lags)), by = 1)) +
  facet_wrap(~lag_label, scales = "free_x") +
  labs(x = "\nLag (dias)", 
       y = "", 
       color = "", 
       title = "Distribuição dos lags e nós internos \n") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("lagknots.jpg", width = 21, height = 12, units = "in", dpi = 300)



#### Cross-basis #####
# Criamos o cross-basis, que combina a exposição (temperatura) e os lags de forma não linear.
# argvar: define um spline cúbico natural para a temperatura, com nós nos percentis selecionados,
#         permitindo capturar associações não lineares entre temperatura e mortalidade.
# arglag: define um spline cúbico natural para os lags, com nós nos valores de lag_knots,
#         permitindo modelar efeitos retardados da temperatura ao longo do tempo.
# Intercept é incluído no lag spline para garantir que o modelo capture corretamente o efeito acumulado.


# Crossbasis com temperatura média
cbs_media <- list()

for(knots_name in names(knots_media)) {
  pred_knots <- knots_media[[knots_name]]
  
  for(lag_name in names(lags)) {
    n_lag <- lags[[lag_name]]
    
    for(tipo_lag in c("logknots", "equally")) {
      lag_knots <- if(tipo_lag == "logknots") {
        lags_knots_3[[paste0(lag_name, "_knots_3")]]
      } else {
        lags_knots_3_equal[[paste0(lag_name, "_knots_3_eq")]]
      }
      
      cb <- crossbasis(
        dta$temp_media,
        lag = n_lag,
        argvar = list(fun = "ns", knots = pred_knots, Boundary.knots = boundary_media_1_99),
        arglag = list(fun = "ns", knots = lag_knots, intercept = TRUE))
      
      cb_name <- paste("temp_media", knots_name, lag_name, tipo_lag, sep = "_")
      cbs_media[[cb_name]] <- cb}}}

names(cbs_media)


# Crossbasis com temperatura máxima
cbs_max <- list()

for(knots_name in names(knots_max)) {
  pred_knots <- knots_max[[knots_name]]
  
  for(lag_name in names(lags)) {
    n_lag <- lags[[lag_name]]
    
    for(tipo_lag in c("logknots", "equally")) {
      lag_knots <- if(tipo_lag == "logknots") {
        lags_knots_3[[paste0(lag_name, "_knots_3")]]
      } else {
        lags_knots_3_equal[[paste0(lag_name, "_knots_3_eq")]]
      }
      
      cb <- crossbasis(
        dta$temp_max,
        lag = n_lag,
        argvar = list(fun = "ns", knots = pred_knots, Boundary.knots = boundary_max_1_99),
        arglag = list(fun = "ns", knots = lag_knots, intercept = TRUE))
      
      cb_name <- paste("temp_max", knots_name, lag_name, tipo_lag, sep = "_")
      cbs_max[[cb_name]] <- cb}}}

names(cbs_max)



# O cross-basis combina a spline da temperatura com a spline de lag.
# O número de colunas do cross-basis = df temperatura × df lag,
# onde df temperatura = número de graus de liberdade da spline de temperatura (nós internos + 1)
#      df lag = número de graus de liberdade da spline de lags (nós internos + intercept, se incluído)
# Por exemplo:
# - temperatura com 3 nós internos → 4 graus de liberdade
# - lag com 2 nós internos → 4 graus de liberdade (incluindo intercept)
# - cross-basis terá 4 × 4 = 16 colunas
# Cada coluna representa uma combinação possível de temperatura × lag, que será estimada no modelo.
# As primeiras linhas são NA porque o efeito acumulado considera dias anteriores à observação,
# que não estão disponíveis no início da série.

#View(cbs_media[["temp_media_knots_10_75_90_lag_21_nk2"]])
#View(cbs_media[["temp_media_knots_10_75_90_lag_21_nk3"]])

#View(cbs_max[["temp_max_knots_10_75_90_lag_21_nk2"]])
#View(cbs_max[["temp_max_knots_10_75_90_lag_21_nk3"]])



#### Modelo CP-DLNM #####
# Ajuste do modelo CP-DLNM:
# - Inclui a cross-basis para temperatura e lags
# - Estrato (strata) controla sazonalidade e variação semanal
# - Quasi-Poisson para lidar com superdispersão em contagens de óbitos


# Modelos com temperatura média

# Lista para armazenar os modelos
modelos_media <- list()

# Loop sobre todos os crossbasis
for(cb_name in names(cbs_media)){
  
  # Crossbasis
  cb <- cbs_media[[cb_name]]
  
  # Modelo
  mod <- gnm(obitos_total ~ cb,
             eliminate = strata,
             family = quasipoisson(),
             data = dta)
  
  # Salvar o modelo na lista com o mesmo nome do crossbasis
  modelos_media[[cb_name]] <- mod}

# Visualizar nomes dos modelos
names(modelos_media)

# Exemplo de resumo de um modelo
summary(modelos_media[["temp_media_knots_10_75_90_lag_21_logknots"]])


# Modelos com temperatura máxima

# Lista para armazenar os modelos
modelos_max <- list()

# Loop sobre todos os crossbasis
for(cb_name in names(cbs_max)){
  
  # Crossbasis 
  cb <- cbs_max[[cb_name]]
  
  # Modelo
  mod <- gnm(obitos_total ~ cb,
             eliminate = strata,
             family = quasipoisson(),
             data = dta)
  
  # Salvar o modelo na lista com o mesmo nome do crossbasis
  modelos_max[[cb_name]] <- mod}

# Visualizar nomes dos modelos
names(modelos_max)

# Exemplo de resumo de um modelo
summary(modelos_max[["temp_max_knots_10_75_90_lag_21_logknots"]])



####  Comparar modelos ####
# Akaike’s Information Criterion for quasi-Poisson (Q-AIC) was used to choose the df for temperature and lag
# https://doi.org/10.1186/1476-069X-11-63

# Deviance residual média
deviance_media <- function(mod){
  mean(abs(residuals(mod, type = "deviance")), na.rm = TRUE)}

# Pearson residual média
pearson_media <- function(mod){
  mean(abs(residuals(mod, type = "pearson")), na.rm = TRUE)}

# QAIC manual
qaic_manual <- function(mod){
  dev <- deviance(mod)
  phi <- summary(mod)$dispersion
  k <- length(coef(mod))
  dev/phi + 2*k}

# Função para avaliar lista de modelos
avaliar_modelos <- function(modelos, tipo = "media"){
  resultados <- data.frame(modelo = names(modelos),
                           deviance_media = NA,
                           pearson_media = NA,
                           QAIC = NA,
                           tipo = tipo)
  
  for(i in seq_along(modelos)){
    mod <- modelos[[i]]
    resultados$deviance_media[i] <- deviance_media(mod)
    resultados$pearson_media[i] <- pearson_media(mod)
    resultados$QAIC[i] <- qaic_manual(mod)}

  resultados$delta_QAIC <- resultados$QAIC - min(resultados$QAIC)
  
  return(resultados)}

# Avaliar modelos média e máxima
resultados_media <- avaliar_modelos(modelos_media, tipo = "media")
resultados_max   <- avaliar_modelos(modelos_max, tipo = "max")

# Juntar e ordenar pelo menor deviance média
resultados <- bind_rows(resultados_media, resultados_max) %>%
  arrange(deviance_media)

# Visualizar resultados
resultados

# Mostrar apenas modelos com delta_QAIC < 5
resultados_competitivos <- resultados %>%
  filter(delta_QAIC < 5) %>%
  arrange(QAIC)

# Visualizar
resultados_competitivos



# Valores extremos e central da temperatura


# Temperatura mínima

temp_media_min <- min(dta$temp_media)
temp_media_min

temp_max_min <- min(dta$temp_max)
temp_max_min


# Mediana da temperatura

temp_media_mediana <- median(dta$temp_media)
temp_media_mediana

temp_max_mediana <- median(dta$temp_max)
temp_max_mediana


# Temperatura máxima

temp_media_max <- max(dta$temp_media)
temp_media_max

temp_max_max <- max(dta$temp_max)
temp_max_max



#### Superfície temperatura–lag–mortalidade #####
# Gera a superfície temperatura–lag–mortalidade a partir do modelo ajustado:
# - Eixo x: temperatura (°C)
# - Eixo y: lags (dias após a exposição)
# - Eixo z: risco relativo de mortalidade
# A superfície mostra como o efeito da temperatura sobre a mortalidade varia ao longo do tempo (efeitos retardados).
# A curva é centralizada na temperatura mediana, permitindo calcular efeitos cumulativos ao longo dos lags.
# Posteriormente, é possível centralizar na MMT (temperatura de menor mortalidade) para interpretação epidemiológica.


# Predições temperatura média

pred_media <- list()

for(cb_name in names(cbs_media)){
  cb <- cbs_media[[cb_name]]
  mod <- modelos_media[[cb_name]]
  pred <- crosspred(cb,
                    mod,
                    at = seq(temp_media_min, temp_media_max, by = 0.1),
                    cen = temp_media_mediana,
                    cumul = TRUE)
  pred_media[[cb_name]] <- pred}

names(pred_media)      

# Plot

dir.create("plots_temp_superficie", showWarnings = FALSE)

for(pred_name in names(pred_media)) {
  pred <- pred_media[[pred_name]]
  file_path <- file.path("plots_temp_superficie", paste0(pred_name, ".jpg"))
  jpeg(filename = file_path, width = 12, height = 8, units = "in", res = 300)
  plot(pred,
       xlab = "Temperatura média (°C)",
       ylab = "Lag (dias)",
       zlab = "Risco Relativo",
       main = paste("Superfície temperatura–lag–mortalidade:", pred_name))
  dev.off()}


# Predições temperatura máxima

pred_max <- list()

for(cb_name in names(cbs_max)){
  cb <- cbs_max[[cb_name]]
  mod <- modelos_max[[cb_name]]
  pred <- crosspred(cb,
                    mod,
                    at = seq(temp_max_min, temp_max_max, by = 0.1),
                    cen = temp_max_mediana,
                    cumul = TRUE)
  pred_max[[cb_name]] <- pred}

names(pred_max)

# Plot

for(pred_name in names(pred_max)) {
  pred <- pred_max[[pred_name]]
  file_path <- file.path("plots_temp_superficie", paste0(pred_name, ".jpg"))
  jpeg(filename = file_path, width = 12, height = 8, units = "in", res = 300)
  plot(pred,
       xlab = "Temperatura máxima (°C)",
       ylab = "Lag (dias)",
       zlab = "Risco Relativo",
       main = paste("Superfície temperatura–lag–mortalidade:", pred_name))
  
  dev.off()}



#### Crossreduce temperatura #####
# Redução da superfície temperatura–lag–mortalidade apenas para a dimensão da temperatura.
# - Calcula a curva de risco relativo (RR) da temperatura sobre a mortalidade.
# - Resume os coeficientes necessários para reconstruir a curva completa.
# - Grade fina de 0,1 °C, centralizada na temperatura mediana.


# Crossreduce temperatura média

red_media <- list()

for(cb_name in names(cbs_media)) {
  cb <- cbs_media[[cb_name]]
  mod <- modelos_media[[cb_name]]
  red <- crossreduce(cb,
                     mod,
                     at = seq(temp_media_min, temp_media_max, by = 0.1),
                     cen = temp_media_mediana)
  red_media[[cb_name]] <- red}

names(red_media)

# Plot

dir.create("plots_temp_mediana", showWarnings = FALSE)

for(red_name in names(red_media)) {
  red <- red_media[[red_name]]
  
  if(grepl("10_75_90", red_name)) {
    knots_vals <- knots_media$knots_10_75_90
    knots_label <- c("10", "75", "90")
  } else if(grepl("10_50_90", red_name)) {
    knots_vals <- knots_media$knots_10_50_90
    knots_label <- c("10", "50", "90")
  } else if(grepl("25_50_90", red_name)) {
    knots_vals <- knots_media$knots_25_50_90
    knots_label <- c("25", "50", "90")
  } else if(grepl("50_75_90", red_name)) {
    knots_vals <- knots_media$knots_50_75_90
    knots_label <- c("50", "75", "90")
  } else if(grepl("50_90", red_name)) {
    knots_vals <- knots_media$knots_50_90
    knots_label <- c("50", "90")
  }
  
  perc_5 <- quantile(dta$temp_media, 0.05, na.rm = TRUE)
  perc_95 <- quantile(dta$temp_media, 0.95, na.rm = TRUE)
  
  knots_text <- paste0(knots_label, ": ", round(knots_vals, 1), "°C")
  
  par(mar = c(7, 4, 4, 2))
  plot(red, 
       xlab = "Temperatura média (ºC)", 
       ylab = "Risco Relativo",
       main = paste("Curva cumulativa temperatura–mortalidade:", red_name, "\n"),
       xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
  
  axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
  axis(2, at = seq(0.5, 3.0, by = 0.5))
  
  abline(v = temp_media_mediana, lty = 2, col = "#253C9C", lwd = 2.5)
  abline(v = knots_vals, lty = 4, col = "#35b779", lwd = 2)
  abline(v = perc_5, lty = 3, col = "#D64933", lwd = 2)
  abline(v = perc_95, lty = 3, col = "#D64933", lwd = 2)
  
  legend(x = "bottomleft", 
         legend = c(
           paste("Mediana:", round(temp_media_mediana, 1), "°C"),
           paste("Knots (percentil):", paste(knots_text, collapse = "; ")),
           paste("5 e 95 percentil:", round(perc_5, 1), "°C;", round(perc_95, 1), "°C")
         ),
         lty = c(2, 4, 3), col = c("#253C9C", "#35b779", "#D64933"), lwd = c(2.5, 2, 2),
         bty = "n", horiz = FALSE, xpd = TRUE, inset = c(0, -0.25))
  
  file_path <- file.path("plots_temp_mediana", paste0(red_name, ".jpg"))
  jpeg(filename = file_path, width = 12, height = 8, units = "in", res = 300)
  
  par(mar = c(7, 4, 4, 2))
  plot(red, 
       xlab = "Temperatura média (ºC)", 
       ylab = "Risco Relativo",
       main = paste("Curva cumulativa temperatura–mortalidade:", red_name, "\n"),
       xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
  
  axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
  axis(2, at = seq(0.5, 3.0, by = 0.5))
  
  abline(v = temp_media_mediana, lty = 2, col = "#253C9C", lwd = 2.5)
  abline(v = knots_vals, lty = 4, col = "#35b779", lwd = 2)
  abline(v = perc_5, lty = 3, col = "#D64933", lwd = 2)
  abline(v = perc_95, lty = 3, col = "#D64933", lwd = 2)
  
  legend(x = "bottomleft", 
         legend = c(
           paste("Mediana:", round(temp_media_mediana, 1), "°C"),
           paste("Knots (percentil):", paste(knots_text, collapse = "; ")),
           paste("5 e 95 percentil:", round(perc_5, 1), "°C;", round(perc_95, 1), "°C")
         ),
         lty = c(2, 4, 3), col = c("#253C9C", "#35b779", "#D64933"), lwd = c(2.5, 2, 2),
         bty = "n", horiz = FALSE, xpd = TRUE, inset = c(0, -0.25))
  
  dev.off()}


# Crossreduce temperatura máxima

red_max <- list()

for(cb_name in names(cbs_max)) {
  cb <- cbs_max[[cb_name]]
  mod <- modelos_max[[cb_name]]
  red <- crossreduce(cb,
                     mod,
                     at = seq(temp_max_min, temp_max_max, by = 0.1),
                     cen = temp_max_mediana)
  red_max[[cb_name]] <- red}

names(red_max)

# Plot

for(red_name in names(red_max)) {
  red <- red_max[[red_name]]
  
  if(grepl("10_75_90", red_name)) {
    knots_vals <- knots_max$knots_10_75_90
    knots_label <- c("10", "75", "90")
  } else if(grepl("10_50_90", red_name)) {
    knots_vals <- knots_max$knots_10_50_90
    knots_label <- c("10", "50", "90")
  } else if(grepl("25_50_90", red_name)) {
    knots_vals <- knots_max$knots_25_50_90
    knots_label <- c("25", "50", "90")
  } else if(grepl("50_75_90", red_name)) {
    knots_vals <- knots_max$knots_50_75_90
    knots_label <- c("50", "75", "90")
  } else if(grepl("50_90", red_name)) {
    knots_vals <- knots_max$knots_50_90
    knots_label <- c("50", "90")
  }
  
  perc_5 <- quantile(dta$temp_max, 0.05, na.rm = TRUE)
  perc_95 <- quantile(dta$temp_max, 0.95, na.rm = TRUE)
  
  knots_text <- paste0(knots_label, ": ", round(knots_vals, 1), "°C")
  
  par(mar = c(7, 4, 4, 2))
  plot(red, 
       xlab = "Temperatura máxima (ºC)", 
       ylab = "Risco Relativo",
       main = paste("Curva cumulativa temperatura–mortalidade:", red_name, "\n"),
       xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
  
  axis(1, at = seq(floor(temp_max_min), ceiling(temp_max_max), by = 1))
  axis(2, at = seq(0.5, 3.0, by = 0.5))
  
  abline(v = temp_max_mediana, lty = 2, col = "#253C9C", lwd = 2.5)
  abline(v = knots_vals, lty = 4, col = "#35b779", lwd = 2)
  abline(v = perc_5, lty = 3, col = "#D64933", lwd = 2)
  abline(v = perc_95, lty = 3, col = "#D64933", lwd = 2)
  
  legend(x = "bottomleft", 
         legend = c(
           paste("Mediana:", round(temp_max_mediana, 1), "°C"),
           paste("Knots (percentil):", paste(knots_text, collapse = "; ")),
           paste("5 e 95 percentil:", round(perc_5, 1), "°C;", round(perc_95, 1), "°C")
         ),
         lty = c(2, 4, 3), col = c("#253C9C", "#35b779", "#D64933"), lwd = c(2.5, 2, 2),
         bty = "n", horiz = FALSE, xpd = TRUE, inset = c(0, -0.25))
  
  file_path <- file.path("plots_temp_mediana", paste0(red_name, ".jpg"))
  jpeg(filename = file_path, width = 12, height = 8, units = "in", res = 300)
  
  par(mar = c(7, 4, 4, 2))
  plot(red, 
       xlab = "Temperatura máxima (ºC)", 
       ylab = "Risco Relativo",
       main = paste("Curva cumulativa temperatura–mortalidade:", red_name, "\n"),
       xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
  
  axis(1, at = seq(floor(temp_max_min), ceiling(temp_max_max), by = 1))
  axis(2, at = seq(0.5, 3.0, by = 0.5))
  
  abline(v = temp_max_mediana, lty = 2, col = "#253C9C", lwd = 2.5)
  abline(v = knots_vals, lty = 4, col = "#35b779", lwd = 2)
  abline(v = perc_5, lty = 3, col = "#D64933", lwd = 2)
  abline(v = perc_95, lty = 3, col = "#D64933", lwd = 2)
  
  legend(x = "bottomleft", 
         legend = c(
           paste("Mediana:", round(temp_max_mediana, 1), "°C"),
           paste("Knots (percentil):", paste(knots_text, collapse = "; ")),
           paste("5 e 95 percentil:", round(perc_5, 1), "°C;", round(perc_95, 1), "°C")
         ),
         lty = c(2, 4, 3), col = c("#253C9C", "#35b779", "#D64933"), lwd = c(2.5, 2, 2),
         bty = "n", horiz = FALSE, xpd = TRUE, inset = c(0, -0.25))
  
  dev.off()}



#### Crossreduce lag #####
# Redução da superfície temperatura–lag–mortalidade apenas ao longo da dimensão do lag


# Crossreduce temperatura média

dir.create("plots_temp_lag", showWarnings = FALSE)

for (red_name in names(red_media)) {
  
  cb <- cbs_media[[red_name]]
  modelo  <- modelos_media[[red_name]]
  
  percentis <- c(0.05, 0.10, 0.75, 0.90, 0.95)
  percentis_temp <- quantile(temp_media, probs = percentis, na.rm = TRUE)
  
  red_list <- lapply(percentis_temp, function(t) crossreduce(
    cb, modelo, type = "var", cen = temp_media_mediana, value = t))
  
  percentis_labels <- c("5º Percentil", "10º Percentil", "75º Percentil", "90º Percentil", "95º Percentil")
  legend_text <- paste0(percentis_labels, ": ", round(percentis_temp, 1), "°C")
  
  cores <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF", "#E6AB02FF")
  lty <- 1

  plot(red_list[[1]],
       xlab = "Lag (dias)",
       ylab = "Risco Relativo",
       main = paste0("Curvas lag–resposta para diferentes temperaturas (ref. ",
                     round(temp_media_mediana,1), "°C)\n ", red_name),
       ylim = c(0.95, 1.20),
       col = cores[1],
       lty = lty,
       lwd = 2,
       yaxt = "n",
       xaxt = "n",
       ci = "n")
  
  axis(2, at = seq(0.95, 1.20, by = 0.05))
  axis(1, at = seq(0, 21, by = 1))
  
  invisible(lapply(2:length(red_list), function(i){
    lines(red_list[[i]], col = cores[i], lty = lty, lwd = 2)}))
  
  legend("topleft", legend = legend_text, col = cores, lty = lty, lwd = 2, bty = "n", y.intersp = 0.8)

  file_path <- file.path("plots_temp_lag", paste0(red_name, ".jpg"))
  jpeg(filename = file_path, width = 21, height = 12, units = "in", res = 300)

  plot(red_list[[1]],
       xlab = "Lag (dias)",
       ylab = "Risco Relativo",
       main = paste0("Curvas lag–resposta para diferentes temperaturas (ref. ",
                     round(temp_media_mediana,1), "°C)\n ", red_name),
       ylim = c(0.95, 1.20),
       col = cores[1],
       lty = lty,
       lwd = 2,
       yaxt = "n",
       xaxt = "n",
       ci = "n")
  
  axis(2, at = seq(0.95, 1.20, by = 0.05))
  axis(1, at = seq(0, 21, by = 1))
  
  invisible(lapply(2:length(red_list), function(i){
    lines(red_list[[i]], col = cores[i], lty = lty, lwd = 2)}))
  
  legend("topleft", legend = legend_text, col = cores, lty = lty, lwd = 2, bty = "n", y.intersp = 0.8)
  
  dev.off()}



# Crossreduce temperatura máxima

for (red_name in names(red_max)) {
  
  cb <- cbs_max[[red_name]]
  modelo <- modelos_max[[red_name]]
  
  percentis <- c(0.05, 0.10, 0.75, 0.90, 0.95)
  percentis_temp <- quantile(temp_max, probs = percentis, na.rm = TRUE)
  
  red_list <- lapply(percentis_temp, function(t) crossreduce(
    cb, modelo, type = "var", cen = temp_max_mediana, value = t))
  
  percentis_labels <- c("5º Percentil", "10º Percentil", "75º Percentil", "90º Percentil", "95º Percentil")
  legend_text <- paste0(percentis_labels, ": ", round(percentis_temp, 1), "°C")
  
  cores <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF", "#E6AB02FF")
  lty <- 1
  
  plot(red_list[[1]],
       xlab = "Lag (dias)",
       ylab = "Risco Relativo",
       main = paste0("Curvas lag–resposta para diferentes temperaturas (ref. ",
                     round(temp_max_mediana,1), "°C)\n ", red_name),
       ylim = c(0.95, 1.20),
       col = cores[1],
       lty = lty,
       lwd = 2,
       yaxt = "n",
       xaxt = "n",
       ci = "n")
  
  axis(2, at = seq(0.95, 1.20, by = 0.05))
  axis(1, at = seq(0, 21, by = 1))
  
  invisible(lapply(2:length(red_list), function(i){
    lines(red_list[[i]], col = cores[i], lty = lty, lwd = 2)}))
  
  legend("topleft", legend = legend_text, col = cores, lty = lty, lwd = 2, bty = "n", y.intersp = 0.8)

  file_path <- file.path("plots_temp_lag", paste0(red_name, ".jpg"))
  jpeg(filename = file_path, width = 21, height = 12, units = "in", res = 300)

  plot(red_list[[1]],
       xlab = "Lag (dias)",
       ylab = "Risco Relativo",
       main = paste0("Curvas lag–resposta para diferentes temperaturas (ref. ",
                     round(temp_max_mediana,1), "°C)\n ", red_name),
       ylim = c(0.95, 1.20),
       col = cores[1],
       lty = lty,
       lwd = 2,
       yaxt = "n",
       xaxt = "n",
       ci = "n")
  
  axis(2, at = seq(0.95, 1.20, by = 0.05))
  axis(1, at = seq(0, 21, by = 1))
  
  invisible(lapply(2:length(red_list), function(i){
    lines(red_list[[i]], col = cores[i], lty = lty, lwd = 2)}))
  
  legend("topleft", legend = legend_text, col = cores, lty = lty, lwd = 2, bty = "n", y.intersp = 0.8)
  
  dev.off()}



#### Crossreduce lag específicos #####
# Associação entre temperatura e mortalidade em lags específicos


# Crossreduce temperatura média

dir.create("plots_temp_lag_especificos", showWarnings = FALSE)

for(red_name in names(pred_media)) {
  pred <- pred_media[[red_name]]
  
  lags_disponiveis <- 1:ncol(pred$matRRfit)
  lags_plot <- unique(c(1, round(length(lags_disponiveis)/2), length(lags_disponiveis)))
  
  cores <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
  
  plot(NA, xlim = range(pred$predvar), ylim = range(pred$matRRfit, na.rm = TRUE),
       xlab = "Temperatura média (°C)", ylab = "Risco Relativo",
       main = paste0("Associação temperatura–mortalidade em lags específicos\n", red_name),
       xaxt = "n", yaxt = "n", bty = "l")
  
  axis(2)
  axis(1)
  
  abline(h = 1, lty = 1, col = "black")
  
  invisible(lapply(seq_along(lags_plot), function(i){
    lag_atual <- lags_plot[i]
    lines(pred$predvar, pred$matRRfit[, lag_atual],
          col = cores[i], lwd = 2, lty = 1)}))
  
  legend_text <- paste0("Lag ", lags_plot, " dias")
  legend("topleft", legend = legend_text, col = cores, lty = 1, lwd = 2, bty = "n")
  
  file_path <- file.path("plots_temp_lag_especificos", paste0(red_name, ".jpg"))
  jpeg(filename = file_path, width = 21, height = 12, units = "in", res = 300)
  
  plot(NA, xlim = range(pred$predvar), ylim = range(pred$matRRfit, na.rm = TRUE),
       xlab = "Temperatura média (°C)", ylab = "Risco Relativo",
       main = paste0("Associação temperatura–mortalidade em lags específicos\n", red_name),
       xaxt = "n", yaxt = "n", bty = "l")
  
  axis(2)
  axis(1)
  abline(h = 1, lty = 1, col = "black")
  
  invisible(lapply(seq_along(lags_plot), function(i){
    lag_atual <- lags_plot[i]
    lines(pred$predvar, pred$matRRfit[, lag_atual],
          col = cores[i], lwd = 2, lty = 1)}))
  
  legend("topleft", legend = legend_text, col = cores, lty = 1, lwd = 2, bty = "n")
  
  dev.off()}



# Crossreduce temperatura máxima

for(red_name in names(pred_max)) {
  pred <- pred_max[[red_name]]
  
  lags_disponiveis <- 1:ncol(pred$matRRfit)
  lags_plot <- unique(c(1, round(length(lags_disponiveis)/2), length(lags_disponiveis)))
  
  cores <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
  
  # Plot na tela
  plot(NA, xlim = range(pred$predvar), ylim = range(pred$matRRfit, na.rm = TRUE),
       xlab = "Temperatura máxima (°C)", ylab = "Risco Relativo",
       main = paste0("Associação temperatura máxima–mortalidade em lags específicos\n", red_name),
       xaxt = "n", yaxt = "n", bty = "l")
  
  axis(2)
  axis(1)
  abline(h = 1, lty = 1, col = "black")
  
  invisible(lapply(seq_along(lags_plot), function(i){
    lag_atual <- lags_plot[i]
    lines(pred$predvar, pred$matRRfit[, lag_atual],
          col = cores[i], lwd = 2, lty = 1)}))
  
  legend_text <- paste0("Lag ", lags_plot, " dias")
  legend("topleft", legend = legend_text, col = cores, lty = 1, lwd = 2, bty = "n")
  
  file_path <- file.path("plots_temp_lag_especificos", paste0(red_name, ".jpg"))
  jpeg(filename = file_path, width = 21, height = 12, units = "in", res = 300)
  
  plot(NA, xlim = range(pred$predvar), ylim = range(pred$matRRfit, na.rm = TRUE),
       xlab = "Temperatura máxima (°C)", ylab = "Risco Relativo",
       main = paste0("Associação temperatura máxima–mortalidade em lags específicos\n", red_name),
       xaxt = "n", yaxt = "n", bty = "l")
  
  axis(2)
  axis(1)
  abline(h = 1, lty = 1, col = "black")
  
  invisible(lapply(seq_along(lags_plot), function(i){
    lag_atual <- lags_plot[i]
    lines(pred$predvar, pred$matRRfit[, lag_atual],
          col = cores[i], lwd = 2, lty = 1)}))
  
  legend("topleft", legend = legend_text, col = cores, lty = 1, lwd = 2, bty = "n")
  
  dev.off()}



#### MMT ####
# Queremos garantir que a curva de risco relativo seja centralizada na temperatura que apresenta
# o menor risco de mortalidade, chamada MMT (Temperatura de Mortalidade Mínima). Para isso,
# precisamos reexecutar as funções crosspred ou crossreduce com um novo valor de centralização (cen=).

# A função abaixo, get_cen, serve para encontrar a temperatura de menor risco.
# Ela aceita a saída de crosspred ou crossreduce e retorna o valor da temperatura correspondente.

get_cen <- function(crosspred) {
  # Precisamos deste if porque a saída de crosspred e crossreduce é ligeiramente diferente.
  # Queremos que a função funcione para ambos os casos.
  if(!is.null(crosspred$fit)) {
    # Para crosspred: retorna a temperatura com o menor risco relativo.
    crosspred$predvar[which.min(crosspred$fit)]
  } else if(!is.null(crosspred$allfit)) {
    # Para crossreduce: retorna a temperatura com o menor risco relativo.
    crosspred$predvar[which.min(crosspred$allfit)]
  } else {
    # Caso não seja possível localizar o mínimo
    print("Não foi possível localizar o mínimo.")
  }}

# Temperatura média
mmt_media <- lapply(red_media, get_cen)
mmt_media <- unlist(mmt_media)
print(mmt_media)

# Temperatura máxima
mmt_max <- lapply(red_max, get_cen)
mmt_max <- unlist(mmt_max)
print(mmt_max)

# O "by" na função crossreduce controla a precisão da curva e da MMT estimada.
# Quanto menor, mais precisa é a localização do mínimo.
# Você pode redefinir red1 usando by = 0.01 para a grade de temperaturas
# e executar novamente get_cen(red1) para ver como muda a MMT.
# Neste exemplo, a mudança é mínima porque a parte inferior da curva é plana.
# Geralmente, usar by = 0.1 (0,1 °C) já é suficiente, dependendo da faixa de temperaturas observada.

# Crossreduce com MMT
red_media_mmt <- list()
red_max_mmt   <- list()


# Crossreduce temperatura média com MMT

for(cb_name in names(cbs_media)) {
  cb <- cbs_media[[cb_name]]
  mod <- modelos_media[[cb_name]]
  
  tmp_red <- crossreduce(cb, mod,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = temp_media_mediana)
  
  mmt <- get_cen(tmp_red)
  
  red <- crossreduce(cb, mod,
                     at = seq(temp_media_min, temp_media_max, by = 0.1),
                     cen = mmt)
  
  red_media_mmt[[cb_name]] <- red}

# Plot

dir.create("plots_temp_mmt", showWarnings = FALSE)

for(red_name in names(red_media)) {
  red <- red_media[[red_name]]
  
  if(grepl("10_75_90", red_name)) {
    knots_vals <- knots_media$knots_10_75_90
    knots_label <- c("10", "75", "90")
  } else if(grepl("10_50_90", red_name)) {
    knots_vals <- knots_media$knots_10_50_90
    knots_label <- c("10", "50", "90")
  } else if(grepl("25_50_90", red_name)) {
    knots_vals <- knots_media$knots_25_50_90
    knots_label <- c("25", "50", "90")
  } else if(grepl("50_75_90", red_name)) {
    knots_vals <- knots_media$knots_50_75_90
    knots_label <- c("50", "75", "90")
  } else if(grepl("50_90", red_name)) {
    knots_vals <- knots_media$knots_50_90
    knots_label <- c("50", "90")
  }
  
  perc_5 <- quantile(dta$temp_media, 0.05, na.rm = TRUE)
  perc_95 <- quantile(dta$temp_media, 0.95, na.rm = TRUE)
  
  knots_text <- paste0(knots_label, ": ", round(knots_vals, 1), "°C")
  
  mmt <- get_cen(red)
  
  # Criar o arquivo JPEG
  file_path <- file.path("plots_temp_mmt", paste0(red_name, "_mmt.jpg"))
  jpeg(filename = file_path, width = 12, height = 8, units = "in", res = 300)
  
  # Plot principal
  par(mar = c(7, 4, 4, 2))
  plot(red, 
       xlab = "Temperatura média (ºC)", 
       ylab = "Risco Relativo",
       main = paste("Curva cumulativa temperatura–mortalidade (MMT):", red_name),
       xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
  
  axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
  axis(2, at = seq(0.5, 3.0, by = 0.5))
  
  # Linhas de referência
  abline(v = mmt, lty = 2, col = "#253C9C", lwd = 2.5)
  abline(v = knots_vals, lty = 4, col = "#35b779", lwd = 2)
  abline(v = perc_5, lty = 3, col = "#D64933", lwd = 2)
  abline(v = perc_95, lty = 3, col = "#D64933", lwd = 2)
  
  # Legenda
  legend(x = "bottomleft", 
         legend = c(
           paste("MMT:", round(mmt, 1), "°C"),
           paste("Knots (percentil):", paste(knots_text, collapse = "; ")),
           paste("5 e 95 percentil:", round(perc_5, 1), "°C;", round(perc_95, 1), "°C")
         ),
         lty = c(2, 4, 3), col = c("#253C9C", "#35b779", "#D64933"), lwd = c(2.5, 2, 2),
         bty = "n", horiz = FALSE, xpd = TRUE, inset = c(0, -0.25))
  
  dev.off()}


# Crossreduce temperatura máxima com MMT

for(cb_name in names(cbs_max)) {
  cb <- cbs_max[[cb_name]]
  mod <- modelos_max[[cb_name]]
  
  tmp_red <- crossreduce(cb, mod,
                         at = seq(temp_max_min, temp_max_max, by = 0.1),
                         cen = temp_max_mediana)
  
  mmt <- get_cen(tmp_red)
  
  red <- crossreduce(cb, mod,
                     at = seq(temp_max_min, temp_max_max, by = 0.1),
                     cen = mmt)
  
  red_max_mmt[[cb_name]] <- red}

# Plot

for(red_name in names(red_max)) {
  red <- red_max[[red_name]]
  
  if(grepl("10_75_90", red_name)) {
    knots_vals <- knots_max$knots_10_75_90
    knots_label <- c("10", "75", "90")
  } else if(grepl("10_50_90", red_name)) {
    knots_vals <- knots_max$knots_10_50_90
    knots_label <- c("10", "50", "90")
  } else if(grepl("25_50_90", red_name)) {
    knots_vals <- knots_max$knots_25_50_90
    knots_label <- c("25", "50", "90")
  } else if(grepl("50_75_90", red_name)) {
    knots_vals <- knots_max$knots_50_75_90
    knots_label <- c("50", "75", "90")
  } else if(grepl("50_90", red_name)) {
    knots_vals <- knots_max$knots_50_90
    knots_label <- c("50", "90")
  }
  
  perc_5 <- quantile(dta$temp_max, 0.05, na.rm = TRUE)
  perc_95 <- quantile(dta$temp_max, 0.95, na.rm = TRUE)
  
  knots_text <- paste0(knots_label, ": ", round(knots_vals, 1), "°C")
  
  mmt <- get_cen(red)
  
  file_path <- file.path("plots_temp_mmt", paste0(red_name, "_mmt.jpg"))
  jpeg(filename = file_path, width = 12, height = 8, units = "in", res = 300)
  
  par(mar = c(7, 4, 4, 2))
  plot(red, 
       xlab = "Temperatura máxima (ºC)", 
       ylab = "Risco Relativo",
       main = paste("Curva cumulativa temperatura–mortalidade (MMT):", red_name),
       xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
  
  axis(1, at = seq(floor(temp_max_min), ceiling(temp_max_max), by = 1))
  axis(2, at = seq(0.5, 3.0, by = 0.5))
  
  abline(v = mmt, lty = 2, col = "#253C9C", lwd = 2.5)
  abline(v = knots_vals, lty = 4, col = "#35b779", lwd = 2)
  abline(v = perc_5, lty = 3, col = "#D64933", lwd = 2)
  abline(v = perc_95, lty = 3, col = "#D64933", lwd = 2)
  
  legend(x = "bottomleft", 
         legend = c(
           paste("MMT:", round(mmt, 1), "°C"),
           paste("Knots (percentil):", paste(knots_text, collapse = "; ")),
           paste("5 e 95 percentil:", round(perc_5, 1), "°C;", round(perc_95, 1), "°C")
         ),
         lty = c(2, 4, 3), col = c("#253C9C", "#35b779", "#D64933"), lwd = c(2.5, 2, 2),
         bty = "n", horiz = FALSE, xpd = TRUE, inset = c(0, -0.25))
  
  dev.off()}



#### Temperatura e a mortalidade (percentis e absoluta) ####


# Criar pasta para salvar os plots
dir.create("plots_temp_percentis", showWarnings = FALSE)


# Temperatura média

for(red_name in names(red_media)) {
  
  red <- red_media[[red_name]]
  
  df_red <- data.frame(temp = red$predvar,
                       rr = red$RRfit,
                       rr_low = red$RRlow,
                       rr_high = red$RRhigh)

  ecdf_fun <- ecdf(dta$temp_media)
  df_red$temp_percentil <- ecdf_fun(df_red$temp) * 100
  dta$temp_percentil <- ecdf_fun(dta$temp_media) * 100
  
  percentis_v2 <- c(0.01, 0.25, 0.50, 0.75, 0.99)
  percentis_temp_v2 <- quantile(dta$temp_media, percentis_v2, na.rm = TRUE)
  
  vlines <- geom_vline(xintercept = percentis_temp_v2, linetype = "dashed", color = "grey40")
  vlines_perc <- geom_vline(xintercept = percentis_v2 * 100, linetype = "dashed", color = "grey40")
  hline <- geom_hline(yintercept = 1, linetype = "solid", color = "black")
  
  # Plot a: temperatura relativa (percentis)
  p_a <- ggplot(df_red, aes(x = temp_percentil, y = rr)) +
    geom_ribbon(aes(ymin = rr_low, ymax = rr_high), fill = "grey80", alpha = 0.8) +
    geom_line(color = "black", linewidth = 1) +
    vlines_perc + hline +
    scale_y_log10(breaks = c(0.8, 1, 1.5, 2, 2.5)) +
    labs(title = paste("Temperatura relativa (percentis):", red_name),
         x = "", y = "Risco Relativo (log)") +
    theme_minimal(base_size = 14)
  
  # Plot b: temperatura absoluta (°C)
  p_b <- ggplot(df_red, aes(x = temp, y = rr)) +
    geom_ribbon(aes(ymin = rr_low, ymax = rr_high), fill = "grey80", alpha = 0.8) +
    geom_line(color = "black", linewidth = 1) +
    vlines + hline +
    scale_x_continuous(breaks = seq(12, 39, by = 1), limits = c(12, 39)) +
    scale_y_log10(breaks = c(0.8, 1, 1.5, 2, 2.5)) +
    labs(title = paste("Temperatura absoluta (°C):", red_name),
         x = "", y = "") +
    theme_minimal(base_size = 14)
  
  # Plot c: distribuição da temperatura relativa
  p_c <- ggplot(dta, aes(x = temp_percentil)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 2, fill = "grey60", color = "white") +
    vlines_perc +
    scale_y_continuous(breaks = seq(0, 0.15, by = 0.05), limits = c(0, 0.15)) +
    labs(title = "Distribuição temperatura (percentis)",
         x = "Percentis", y = "Densidade") +
    theme_minimal(base_size = 14)
  
  # Plot d: distribuição da temperatura absoluta
  p_d <- ggplot(dta, aes(x = temp_media)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "grey60", color = "white") +
    vlines +
    scale_x_continuous(breaks = seq(12, 39, by = 1), limits = c(12, 39)) +
    labs(title = "Distribuição temperatura (°C)",
         x = "Temperatura (°C)", y = "") +
    theme_minimal(base_size = 14)
  
  # Combinar plots
  final_plot <- (p_a | p_b) / (p_c | p_d) +
    plot_annotation(
      caption = 'Linhas tracejadas: percentis 1º, 25º, 50º, 75º, 99º. Área sombreada: IC 95%.') &
    theme(plot.caption = element_text(hjust = 0, size = 14),
          plot.caption.position = "plot")

  ggsave(filename = file.path("plots_temp_percentis", paste0(red_name, "_temp_mort_log_density.png")),
         plot = final_plot, width = 21, height = 12, units = "in", dpi = 300)}


# Temepratura máxima

for(red_name in names(red_max)) {
  
  red <- red_max[[red_name]]
  
  df_red <- data.frame(temp = red$predvar,
                       rr = red$RRfit,
                       rr_low = red$RRlow,
                       rr_high = red$RRhigh)
  
  ecdf_fun <- ecdf(dta$temp_max)
  df_red$temp_percentil <- ecdf_fun(df_red$temp) * 100
  dta$temp_percentil <- ecdf_fun(dta$temp_max) * 100
  
  percentis_v2 <- c(0.01, 0.25, 0.50, 0.75, 0.99)
  percentis_temp_v2 <- quantile(dta$temp_max, percentis_v2, na.rm = TRUE)
  
  vlines <- geom_vline(xintercept = percentis_temp_v2, linetype = "dashed", color = "grey40")
  vlines_perc <- geom_vline(xintercept = percentis_v2 * 100, linetype = "dashed", color = "grey40")
  hline <- geom_hline(yintercept = 1, linetype = "solid", color = "black")
  
  # Plot a: temperatura relativa (percentis)
  p_a <- ggplot(df_red, aes(x = temp_percentil, y = rr)) +
    geom_ribbon(aes(ymin = rr_low, ymax = rr_high), fill = "grey80", alpha = 0.8) +
    geom_line(color = "black", linewidth = 1) +
    vlines_perc + hline +
    scale_y_log10(breaks = c(0.8, 1, 1.5, 2, 2.5)) +
    labs(title = paste("Temperatura relativa (percentis):", red_name),
         x = "", y = "Risco Relativo (log)") +
    theme_minimal(base_size = 14)
  
  # Plot b: temperatura absoluta (°C)
  p_b <- ggplot(df_red, aes(x = temp, y = rr)) +
    geom_ribbon(aes(ymin = rr_low, ymax = rr_high), fill = "grey80", alpha = 0.8) +
    geom_line(color = "black", linewidth = 1) +
    vlines + hline +
    scale_x_continuous(breaks = seq(12, 39, by = 1), limits = c(12, 39)) +
    scale_y_log10(breaks = c(0.8, 1, 1.5, 2, 2.5)) +
    labs(title = paste("Temperatura absoluta (°C):", red_name),
         x = "", y = "") +
    theme_minimal(base_size = 14)
  
  # Plot c: distribuição da temperatura relativa
  p_c <- ggplot(dta, aes(x = temp_percentil)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 2, fill = "grey60", color = "white") +
    vlines_perc +
    scale_y_continuous(breaks = seq(0, 0.15, by = 0.05), limits = c(0, 0.15)) +
    labs(title = "Distribuição temperatura (percentis)",
         x = "Percentis", y = "Densidade") +
    theme_minimal(base_size = 14)
  
  # Plot d: distribuição da temperatura absoluta
  p_d <- ggplot(dta, aes(x = temp_max)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "grey60", color = "white") +
    vlines +
    scale_x_continuous(breaks = seq(12, 39, by = 1), limits = c(12, 39)) +
    labs(title = "Distribuição temperatura (°C)",
         x = "Temperatura (°C)", y = "") +
    theme_minimal(base_size = 14)
  
  # Combinar plots
  final_plot <- (p_a | p_b) / (p_c | p_d) +
    plot_annotation(
      caption = 'Linhas tracejadas: percentis 1º, 25º, 50º, 75º, 99º. Área sombreada: IC 95%.') &
    theme(plot.caption = element_text(hjust = 0, size = 14),
          plot.caption.position = "plot")
  
  # Salvar no mesmo diretório
  ggsave(filename = file.path("plots_temp_percentis", paste0(red_name, "_temp_max_log_density.png")),
         plot = final_plot, width = 21, height = 12, units = "in", dpi = 300)}










### 2014-2019 ####


# Filtrar os dados
dta_14_19 <- dta |> filter(lubridate::year(data) %in% 2014:2019)

dta_14_19 <- dta_14_19 %>%
  select(-ano, -grupo_anos, -strata, -temp_percentil, -fim_semana, -dia_semana)


# Outliers

# Calcular média e desvio padrão
media <- mean(dta_14_19$temp_media, na.rm = TRUE)
sd <- sd(dta_14_19$temp_media, na.rm = TRUE)

# Intervalo
limite_inferior <- media - 2.4*sd
limite_superior <- media + 2.4*sd

# Número de linhas antes
n_antes <- nrow(dta_14_19)

# Filtrar sobrescrevendo
dta_14_19 <- dta_14_19 %>%
  filter(temp_media >= limite_inferior,
         temp_media <= limite_superior)

# Número de linhas depois
n_depois <- nrow(dta_14_19)

# Quantos foram eliminados
n_eliminados <- n_antes - n_depois
n_eliminados


# Strata
dta_14_19 <- dta_14_19 |> 
  mutate(strata = paste(year(data),
                        month(data),
                        wday(data, label = TRUE), 
                        sep = ":") |> factor())

#View(dta_14_19)


# Nós dos splines de temperatura
pred_knots_14_19 <- quantile(dta_14_19$temp_media, c(10, 75, 90)/100, na.rm = TRUE)
pred_knots_14_19


# Matriz 'cross-basis' que combina o efeito não linear da temperatura e dos lags
cb_14_19 <- crossbasis(dta_14_19$temp_media,
                       lag = n_lag,
                       argvar = list(fun = "ns", knots = pred_knots_14_19),
                       arglag = list(fun = "ns", knots = lag_knots))


# Modelo DLNM condicional (gnm) para o período
modelo_14_19 <- gnm(obitos_total ~ cb_14_19,
                    eliminate = strata,
                    family = quasipoisson(),
                    data = dta_14_19)


# Valores extremos e central da temperatura média

# Temperatura média mínima
temp_media_min <- min(dta_14_19$temp_media)
temp_media_min

# Mediana da temperatura média
temp_media_mediana <- median(dta_14_19$temp_media)
temp_media_mediana

# Temperatura média máxima
temp_media_max <- max(dta_14_19$temp_media)
temp_media_max


# Redução da superfície temperatura–lag–mortalidade apenas para a dimensão da temperatura.
red_14_19 <- crossreduce(cb_14_19,
                         modelo_14_19,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = temp_media_mediana)

plot(red_14_19, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade (2014-2019) \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 4.5), lwd = 2)
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.5, 4.5, by = 0.5))

abline(v = temp_media_mediana, lty = 3, col = "black")

legend("bottomright", inset = c(-0.2, 0), 
       legend = paste("Mediana:", round(temp_media_mediana, 1), "°C"),
       lty = 3, col = "black", bty = "n")

dev.copy(jpeg, filename = "curva_temp_mort_2014_2019.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()


# Reexecutamos e plotamos o modelo usando a nova temperatura de centralização (MMT).

red_14_19 <- crossreduce(cb_14_19,
                         modelo_14_19,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = get_cen(red_14_19))

# Estimar MMT
mmt <- get_cen(red_14_19)
mmt

# Variáveis
temp <- red_14_19$predvar
rr <- red_14_19$RRfit

# Separar abaixo e acima da MMT
abaixo <- temp <= mmt
acima <- temp >= mmt

# Plot
plot(red_14_19, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade (2014-2019) \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 4.5), lwd = 2)
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.5, 4.5, by = 0.5))

lines(temp[acima], rr[acima], col = "#D64933", lwd = 2)
lines(temp[abaixo], rr[abaixo], col = "#253C9C", lwd = 2)

abline(v = mmt, lty = 3)

legend("bottomright", inset = c(-0.496, 0),
       legend = paste0("Temperatura mínima de mortalidade: ", round(mmt, 1), " ºC"),
       lty = 3, col = "black", bty = "n", cex = 1)

dev.copy(jpeg, filename = "curva_temp_mort_MMT_2014_2019.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()





### 2020-2022 ####


# Filtrar os dados
dta_20_22 <- dta |> filter(lubridate::year(data) %in% 2020:2022)

dta_20_22 <- dta_20_22 %>%
  select(-ano, -grupo_anos, -strata, -temp_percentil, -fim_semana, -dia_semana)


# Strata
dta_20_22 <- dta_20_22 |> 
  mutate(strata = paste(year(data),
                        month(data),
                        wday(data, label = TRUE), 
                        sep = ":") |> factor())

View(dta_20_22)


# Nós dos splines de temperatura
pred_knots_20_22 <- quantile(dta_20_22$temp_media, c(10, 75, 90)/100, na.rm = TRUE)
pred_knots_20_22


# Matriz 'cross-basis' que combina o efeito não linear da temperatura e dos lags
cb_20_22 <- crossbasis(dta_20_22$temp_media,
                       lag = n_lag,
                       argvar = list(fun = "ns", knots = pred_knots_20_22),
                       arglag = list(fun = "ns", knots = lag_knots))


# Modelo DLNM condicional (gnm) para o período
modelo_20_22 <- gnm(obitos_total ~ cb_20_22,
                    eliminate = strata,
                    family = quasipoisson(),
                    data = dta_20_22)


# Valores extremos e central da temperatura média

# Temperatura média mínima
temp_media_min <- min(dta_20_22$temp_media)
temp_media_min

# Mediana da temperatura média
temp_media_mediana <- median(dta_20_22$temp_media)
temp_media_mediana

# Temperatura média máxima
temp_media_max <- max(dta_20_22$temp_media)
temp_media_max


# Redução da superfície temperatura–lag–mortalidade apenas para a dimensão da temperatura.
red_20_22 <- crossreduce(cb_20_22,
                         modelo_20_22,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = temp_media_mediana)

plot(red_20_22, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade (2020-2022) \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 4.5), lwd = 2)
axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
axis(2, at = seq(0.5, 4.5, by = 0.5))

abline(v = temp_media_mediana, lty = 3, col = "black")

legend("bottomright", inset = c(-0.2, 0), 
       legend = paste("Mediana:", round(temp_media_mediana, 1), "°C"),
       lty = 3, col = "black", bty = "n")

dev.copy(jpeg, filename = "curva_temp_mort_2020_2022.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()


# Reexecutamos e plotamos o modelo usando a nova temperatura de centralização (MMT).

red_20_22 <- crossreduce(cb_20_22,
                         modelo_20_22,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = get_cen(red_20_22))

# Estimar MMT
mmt <- get_cen(red_20_22)

# Variáveis
temp <- red_20_22$predvar
rr <- red_20_22$RRfit

# Separar abaixo e acima da MMT
abaixo <- temp <= mmt
acima <- temp >= mmt

# Plot final
plot(red_20_22, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade (2020-2022) \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 4.5), lwd = 2)
axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
axis(2, at = seq(0.5, 4.5, by = 0.5))

lines(temp[acima], rr[acima], col = "#D64933", lwd = 2)
lines(temp[abaixo], rr[abaixo], col = "#253C9C", lwd = 2)

abline(v = mmt, lty = 3)

legend("bottomright", inset = c(-0.496, 0),
       legend = paste0("Temperatura mínima de mortalidade: ", round(mmt, 1), " ºC"),
       lty = 3, col = "black", bty = "n", cex = 1)

dev.copy(jpeg, filename = "curva_temp_mort_MMT_2020_2022.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()





### 2023-2024 ####


# Filtrar os dados
dta_23_24 <- dta |> filter(lubridate::year(data) %in% 2023:2024)

dta_23_24 <- dta_23_24 %>%
  select(-ano, -grupo_anos, -strata, -temp_percentil, -fim_semana, -dia_semana)


# Strata
dta_23_24 <- dta_23_24 |> 
  mutate(strata = paste(year(data),
                        month(data),
                        wday(data, label = TRUE), 
                        sep = ":") |> factor())

View(dta_23_24)


# Nós dos splines de temperatura
pred_knots_23_24 <- quantile(dta_23_24$temp_media, c(10, 75, 90)/100, na.rm = TRUE)
pred_knots_23_24


# Matriz 'cross-basis' que combina o efeito não linear da temperatura e dos lags
cb_23_24 <- crossbasis(dta_23_24$temp_media,
                       lag = n_lag,
                       argvar = list(fun = "ns", knots = pred_knots_23_24),
                       arglag = list(fun = "ns", knots = lag_knots))


# Modelo DLNM condicional (gnm) para o período
modelo_23_24 <- gnm(obitos_total ~ cb_23_24,
                    eliminate = strata,
                    family = quasipoisson(),
                    data = dta_23_24)


# Valores extremos e central da temperatura média

# Temperatura média mínima
temp_media_min <- min(dta_23_24$temp_media)
temp_media_min

# Mediana da temperatura média
temp_media_mediana <- median(dta_23_24$temp_media)
temp_media_mediana

# Temperatura média máxima
temp_media_max <- max(dta_23_24$temp_media)
temp_media_max


# Redução da superfície temperatura–lag–mortalidade apenas para a dimensão da temperatura.
red_23_24 <- crossreduce(cb_23_24,
                         modelo_23_24,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = temp_media_mediana)

plot(red_23_24, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade (2023-2024) \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 4.5), lwd = 2)
axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
axis(2, at = seq(0.5, 4.5, by = 0.5))

abline(v = temp_media_mediana, lty = 3, col = "black")

legend("bottomright", inset = c(-0.2, 0), 
       legend = paste("Mediana:", round(temp_media_mediana, 1), "°C"),
       lty = 3, col = "black", bty = "n")

dev.copy(jpeg, filename = "curva_temp_mort_2023_2024.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()


# Reexecutamos e plotamos o modelo usando a nova temperatura de centralização (MMT).

red_23_24 <- crossreduce(cb_23_24,
                         modelo_23_24,
                         at = seq(temp_media_min, temp_media_max, by = 0.1),
                         cen = get_cen(red_23_24))

# Estimar MMT
mmt <- get_cen(red_23_24)

# Variáveis
temp <- red_23_24$predvar
rr <- red_23_24$RRfit

# Separar abaixo e acima da MMT
abaixo <- temp <= mmt
acima <- temp >= mmt

# Plot final
plot(red_23_24, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade (2023-2024) \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 4.5), lwd = 2)
axis(1, at = seq(floor(temp_media_min), ceiling(temp_media_max), by = 1))
axis(2, at = seq(0.5, 4.5, by = 0.5))

lines(temp[acima], rr[acima], col = "#D64933", lwd = 2)
lines(temp[abaixo], rr[abaixo], col = "#253C9C", lwd = 2)

abline(v = mmt, lty = 3)

legend("bottomright", inset = c(-0.496, 0),
       legend = paste0("Temperatura mínima de mortalidade: ", round(mmt, 1), " ºC"),
       lty = 3, col = "black", bty = "n", cex = 1)

dev.copy(jpeg, filename = "curva_temp_mort_MMT_2023_2024.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()

