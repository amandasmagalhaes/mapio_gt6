# FIGURA EDITORIAL (A–B)

# Pacotes
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(patchwork)

# 0) LEITURA E LIMPEZA
mes_map <- c(Jan = 1, Fev = 2, Mar = 3, Abr = 4, Mai = 5, Jun = 6, 
             Jul = 7, Ago = 8, Set = 9, Out = 10, Nov = 11, Dez = 12)

df <- readr::read_delim(
  "bh_monthly_2014_2024.csv",
  delim = ";",
  show_col_types = FALSE
) %>%
  mutate(month = stringr::str_trim(month),
         month = mes_map[month])

month_lbl <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")

# 1) PREPARO: ANOMALIAS E LONG FORMAT
df <- df %>%
  mutate(
    anom_tmax = tmax - tmax_norm,
    anom_tmin = tmin - tmin_norm
  )

df_long_anom <- df %>%
  select(year, month, anom_tmax, anom_tmin) %>%
  pivot_longer(cols = starts_with("anom_"),
               names_to = "var",
               values_to = "anom") %>%
  mutate(
    var = recode(var, "anom_tmax" = "Máxima", "anom_tmin" = "Mínima")
  )

anom_summary_full <- df_long_anom %>%
  group_by(var, month) %>%
  summarise(
    mean_anom = mean(anom, na.rm = TRUE),
    p25 = quantile(anom, 0.25, na.rm = TRUE),
    p75 = quantile(anom, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# 2) PREPARO: TOTAIS E PERCENTIS (PAINEL B)
env_long <- df %>%
  group_by(month) %>%
  summarise(
    tmax_norm = mean(tmax_norm, na.rm = TRUE),
    tmin_norm = mean(tmin_norm, na.rm = TRUE),
    tmax_p25  = quantile(tmax, 0.25, na.rm = TRUE),
    tmax_p75  = quantile(tmax, 0.75, na.rm = TRUE),
    tmin_p25  = quantile(tmin, 0.25, na.rm = TRUE),
    tmin_p75  = quantile(tmin, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -month,
    names_to = "temp_type",
    values_to = "value"
  ) %>%
  mutate(
    Variavel = if_else(grepl("tmax", temp_type), "Máxima", "Mínima"),
    Tipo = case_when(
      grepl("norm", temp_type) ~ "Média histórica",
      grepl("p25", temp_type) ~ "25º Percentil",
      grepl("p75", temp_type) ~ "75º Percentil"
    )
  )

highlight_df <- tibble(xmin = 9 - 0.5, xmax = 11 + 0.5, ymin = -Inf, ymax = Inf)

# 3) PAINEL A (Anomalias)
pA <- ggplot(anom_summary_full, aes(x = month)) +
  geom_line(aes(y = mean_anom, color = var), linewidth = 1) +
  geom_line(aes(y = p25, color = var), linetype = "dotdash", linewidth = 1) +
  geom_line(aes(y = p75, color = var), linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotdash", linewidth = 0.5) +
  scale_x_continuous(breaks = 1:12, labels = month_lbl) +
  scale_y_continuous(limits = c(-2, 4), breaks = seq(-2, 4, 2)) +
  scale_color_manual(values = c("Máxima" = "#d7191c", "Mínima" = "#2c7bb6")) +
  guides(color = "none") + 
  labs(x = NULL, y = "Anomalia (°C) vs. Normais \n", title = "A\n") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 4) PAINEL B (Temperaturas)
pB <- ggplot(env_long, aes(x = month, y = value, color = Variavel, linetype = Tipo)) +
  geom_rect(
    data = highlight_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "Meses"),
    inherit.aes = FALSE, alpha = 0.5
  ) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("Máxima" = "#d7191c", "Mínima" = "#2c7bb6"),
    name = "Temperatura"
  ) +
  scale_linetype_manual(
    values = c("Média histórica" = "solid", "25º Percentil" = "dotdash", "75º Percentil" = "dotted"),
    name = "Marcadores"
  ) +
  scale_fill_manual(
    values = c("Meses" = "grey90"),
    name = "Destaque",
    labels = "Setembro/Novembro"
  ) +
  scale_x_continuous(breaks = 1:12, labels = month_lbl) +
  scale_y_continuous(limits = c(12, 32), breaks = seq(12, 32, 4)) +
  labs(x = "\n Mês (2014/2024)", y = "Temperatura (°C) \n", title = "\nB\n") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 5) COMPOSIÇÃO FINAL
fig_final <- (pA / pB) | guide_area()

fig_final <- fig_final + 
  plot_layout(
    guides = 'collect', 
    widths = c(4, 1)
  ) & 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box = "vertical"
  )

# Visualizar e Salvar (Tamanho de Slide Widescreen)
ggsave("Figura_Editorial_Final.png", fig_final, width = 338, height = 190, units = "mm", dpi = 1200)

