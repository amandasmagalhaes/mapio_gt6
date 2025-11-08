# MapIO-BH - GT6 #
# Análises de temperatura horária - Outubro/2025 #





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

dta <- read_dta("temperatura_horaria.dta")
attach(dta)

summary(dta)
#View(dta)





## Preparar os dados ####

# Converter hora para número decimal
# Ex.: 21:30 -> 21.5
dta <- dta %>%
  mutate(hora_dec = hour(hms(paste0(hora, ":00"))) + 
           minute(hms(paste0(hora, ":00")))/60)

# Transformar nível de alerta em numérico (0 a 5)
dta$nivel_alerta <- as.numeric(dta$nivel_alerta)





## Visualizar a variação horária (exploratória) ####



# Gráfico de linhas de todos os dias de 2024, mostrando variação horária
ggplot(dta %>% filter(data >= as.Date("2024-01-01")), 
       aes(x = hora_dec, y = temperatura_maxima, group = data)) +
  geom_line(alpha = 0.1, color = "darkblue", linewidth = 0.3) +
  geom_smooth(aes(group = 1), color = "red", se = FALSE, linewidth = 0.8) +
  labs(x = "\nHora do dia", 
       y = "Temperatura (°C) \n",
       title = "Variação horária da temperatura (2024) \n") +
  theme_minimal()

# - Linhas azuis finas representam cada dia individualmente
# - Linha vermelha mostra a tendência média horária
# - Isso ajuda a visualizar como a temperatura varia ao longo do dia



# Criar faixas de temperatura
dta <- dta %>%
  mutate(faixa_temp = cut(temperatura_maxima,
                          breaks = c(0, 20, 25, 30, 35, 40),
                          labels = c("<20", "20–25", "25–30", "30–35", ">35"),
                          right = FALSE))

# Contar proporção por hora e faixa (removendo NA)
dist_horaria <- dta %>%
  filter(!is.na(faixa_temp)) %>%
  group_by(hora_dec, faixa_temp) %>%
  summarise(freq = n()) %>%
  group_by(hora_dec) %>%
  mutate(prop = freq / sum(freq))

# Gráfico
ggplot(dist_horaria, aes(x = hora_dec, y = prop, fill = faixa_temp)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  labs(x = "Hora do dia", y = "Proporção",
       fill = "Faixa de temperatura (°C)",
       title = "Distribuição horária das temperaturas") +
  theme_minimal()



# Criar variável de nível de alerta e filtrar NAs e Sem alerta
dist_horaria <- dta %>%
  filter(!is.na(temperatura_maxima)) %>%
  mutate(nivel_alerta_class = case_when(
    temperatura_maxima >= 29.3 & temperatura_maxima <= 31.9 ~ "MMT (29,3–31,9)",
    temperatura_maxima >= 32.0 & temperatura_maxima <= 33.0 ~ "Alerta 1 (32,0–33,0)",
    temperatura_maxima >= 33.1 & temperatura_maxima <= 34.5 ~ "Alerta 2 (33,1–34,5)",
    temperatura_maxima >= 34.6 & temperatura_maxima <= 35.8 ~ "Alerta 3 (34,6–35,8)",
    temperatura_maxima > 35.8 ~ "Alerta 4 (>35,8)"
  )) %>%
  filter(!is.na(nivel_alerta_class)) %>%
  mutate(nivel_alerta_class = factor(nivel_alerta_class, 
                                     levels = c("MMT (29,3–31,9)", 
                                                "Alerta 1 (32,0–33,0)",
                                                "Alerta 2 (33,1–34,5)",
                                                "Alerta 3 (34,6–35,8)",
                                                "Alerta 4 (>35,8)"))) %>%
  group_by(hora, nivel_alerta_class) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(hora) %>%
  mutate(proporcao = n / sum(n))

# Paleta azul → vermelho
paleta_alerta <- c("MMT (29,3–31,9)" = "#D8D8D8",
                   "Alerta 1 (32,0–33,0)" = "#91bfdb",
                   "Alerta 2 (33,1–34,5)" = "#fee090",
                   "Alerta 3 (34,6–35,8)" = "#fc8d59",
                   "Alerta 4 (>35,8)" = "#d73027")

# Plot
dist_horaria <- ggplot(dist_horaria, aes(x = hora, y = n, fill = nivel_alerta_class)) +
  geom_col(position = "stack") +
  labs(x = "\n Hora", y = "Número de observações \n", fill = "Nível de alerta") +
  scale_fill_manual(values = paleta_alerta) +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 250)) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    panel.grid.major = element_line(color = "gray97"),
    panel.grid.minor = element_line(color = "gray97"))
dist_horaria

ggsave(filename = "dist_horaria.jpg", plot = dist_horaria,
       width = 21, height = 12, units = "in", dpi = 300)



## Ajustar GAM (Generalized Additive Model) ####

# Objetivo:
# Modelar a variação da temperatura máxima ao longo do dia e ao longo do tempo,
# capturando padrões não-lineares tanto dentro do dia quanto entre os dias.

# Justificativa do modelo:
# 1. s(hora_dec, bs="cc"): spline cíclico para hora do dia, capturando os picos e vales previsíveis
#    ao longo do ciclo diário e garantindo continuidade entre 0h e 24h.
# 2. s(as.numeric(data), bs="cs"): spline cúbico penalizado para tendência de longo prazo entre dias,
#    permitindo ajustar sazonalidades e variações anuais sem superajuste.
# 3. O GAM permite ajustar curvas suaves (splines) sem assumir forma linear rígida.
# 4. 's()' define uma função spline que suaviza os dados, capturando não-linearidades.

modelo_gam <- gam(temperatura_maxima ~ 
                    s(hora_dec, bs="cc") + 
                    s(as.numeric(data), bs="cs"),
                  data = dta)

# Resumo do modelo
summary(modelo_gam)

# Interpretação do GAM:
# 1. A spline de hora_dec mostra a variação da temperatura ao longo do dia,
#    capturando picos (ex.: meio-dia) e vales (ex.: madrugada).
# 2. A spline de data representa tendências de longo prazo entre dias,
#    incluindo aumento gradual, sazonalidade ou variações anuais.
# 3. A combinação dessas duas componentes permite gerar previsões suaves
#    da temperatura para cada hora de cada dia.
# 4. Esse modelo é útil para analisar a relação com os níveis de alerta,
#    pois fornece estimativas esperadas de temperatura, reduzindo o efeito de ruídos pontuais.





## Gerar previsões ####

# Previsões hora a hora para cada observação
dta <- dta %>%
  mutate(previsao = predict(modelo_gam, newdata = dta))





## Métricas de variação diária ####

metricas_diarias <- dta %>%
  group_by(data) %>%
  summarise(
    amplitude = max(previsao) - min(previsao),  # diferença entre pico e vale do dia
    sd_temp = sd(previsao)                       # dispersão da temperatura durante o dia
  )





## Curva horária média (todos os dias) ####

grid_media <- dta %>%
  group_by(hora_dec) %>%
  summarise(temp_media = mean(previsao))

curva_horaria <- ggplot(grid_media, aes(x = hora_dec, y = temp_media)) +
  geom_line(color = "#d73027", linewidth = 1.2) +
  labs(x = "\n Hora",
       y = "Temperatura máxima (°C) \n") +
  scale_x_continuous(limits = c(0, 23), 
                     breaks = seq(0, 23, by = 1),
                     labels = function(x) sprintf("%02d:00", x)) +
  scale_y_continuous(limits = c(17, 30), breaks = seq(17, 30, by = 2)) +
  theme_minimal(base_size = 16) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.grid.major = element_line(color = "gray97"),
        panel.grid.minor = element_line(color = "gray97"))
curva_horaria

ggsave(filename = "curva_horaria.jpg", plot = curva_horaria,
       width = 21, height = 12, units = "in", dpi = 300)

# Interpretação:
# - Linha vermelha representa a temperatura média ao longo do dia
# - Ajuda a identificar horários de pico de calor





## Correlação com nível de alerta ####


correlacao <- cor(dta$previsao, dta$nivel_alerta, use = "complete.obs")
correlacao

# Interpretação:
# - Valor = 0.31 → correlação positiva moderada
# - Horas com maior temperatura prevista tendem a coincidir com níveis de alerta mais altos
# - Isso confirma que a GAM captura a variação horária relevante para alertas








