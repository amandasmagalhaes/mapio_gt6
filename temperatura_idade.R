# MapIO-BH - GT6 #
# Análises descritiva de temperatura e idade - Outubro/2025 #





## Limpar o ambiente ####

rm(list=ls())





## Diretório ####

setwd("C:/Users/amand/Amanda/GitHub/mapio_gt6")
getwd()





## Pacotes ####

pacman::p_load(
  # Importação de dados
  "haven",
  # Manipulação de dados e datas
  "dplyr",
  "lubridate",
  # Visualização
  "ggplot2",
  # Modelagem GAM
  "mgcv")





## Dados ####

dta <- read_dta("temperatura_idade.dta")
attach(dta)

summary(dta)
#View(dta)





## Descritiva ####

# Filtrar os anos desejados
dta_filt <- dta %>%
  filter(ano_obito %in% c(2017, 2018, 2023, 2024))

# Agregar por mês e ano
dta_mensal <- dta_filt %>%
  group_by(ano_obito, mes_obito) %>%
  summarise(obitos = sum(anos_60_mais, na.rm = TRUE)) %>%
  group_by(ano_obito) %>%
  mutate(pct = 100 * obitos / sum(obitos)) %>%
  ungroup()

# Gráfico
plot_mortalidade <- function(data, var, titulo) {
  dta_mensal <- data %>%
    group_by(ano_obito, mes_obito) %>%
    summarise(obitos = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    group_by(ano_obito) %>%
    mutate(pct = 100 * obitos / sum(obitos)) %>%
    ungroup()
  
  ggplot(dta_mensal, aes(x = mes_obito, y = pct, group = factor(ano_obito), color = factor(ano_obito))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = 1:12,
      labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                 "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    ) +
    scale_y_continuous(
      limits = c(6, 12),
      breaks = seq(6, 12, 2),
      labels = scales::percent_format(scale = 1)
    ) +
    labs(
      title = titulo,
      x = "\nMês do óbito",
      y = "Proporção de óbitos (%)\n",
      color = "Ano do óbito"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

# Usar para 60+ e 65+
plot_mortalidade(dta_filt, "anos_60_mais", "Proporção mensal de óbitos (60 anos ou mais)")
plot_mortalidade(dta_filt, "anos_65_mais", "Proporção mensal de óbitos (65 anos ou mais)")




