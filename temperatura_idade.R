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

# Criar coluna de óbitos totais
dta_filt <- dta_filt %>%
  mutate(obitos_total = rowSums(across(starts_with("anos_")), na.rm = TRUE))

# Gráfico de linhas
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
    scale_x_continuous(breaks = 1:12,
                       labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                  "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
    scale_y_continuous(limits = c(6, 12),
                       breaks = seq(6, 12, 2),
                       labels = scales::percent_format(scale = 1)) +
    labs(title = titulo,
         x = "\nMês do óbito",
         y = "Proporção de óbitos (%)\n",
         color = "Ano do óbito") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5))}

# Gerar os gráficos
plot_mortalidade_linha(dta_filt, "anos_60_mais", "Proporção mensal de óbitos (60 anos ou mais) \n")
plot_mortalidade_linha(dta_filt, "anos_65_mais", "Proporção mensal de óbitos (65 anos ou mais) \n")
plot_mortalidade_linha(dta_filt, "obitos_total", "Proporção mensal de óbitos (todas as idades) \n")



# Gráfico de barras
plot_mortalidade_barra <- function(data, var, titulo,
                                   ymin = 0, ymax = 5000, step_y = 1000) {
  dta_mensal <- data %>%
    group_by(ano_obito, mes_obito) %>%
    summarise(obitos = sum(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  ggplot(dta_mensal, aes(x = factor(mes_obito),
                         y = obitos,
                         fill = factor(ano_obito))) +
    geom_col(position = "dodge") +
    scale_x_discrete(labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
    scale_y_continuous(limits = c(ymin, ymax),
                       breaks = seq(ymin, ymax, step_y)) +
    labs(title = titulo,
         x = "\n Mês do óbito",
         y = "Número de óbitos (n) \n",
         fill = "Ano do óbito") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.position = "right")}

# Gerar os gráficos
plot_mortalidade_barra(dta_filt,
                       "obitos_total",
                       "Número mensal de óbitos (todas as idades) \n",
                       ymax = 5000, step_y = 1000)

plot_mortalidade_barra(dta_filt,
                       ymax = 2000, step_y = 500,
                       "anos_60_mais",
                       "Número mensal de óbitos (60 anos ou mais) \n")

plot_mortalidade_barra(dta_filt,
                       ymax = 2000, step_y = 500,
                       "anos_65_mais",
                       "Número mensal de óbitos (65 anos ou mais) \n")

