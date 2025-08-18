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
  "dplyr", "tidyr", "lubridate", 
  # Visualização de dados
  "ggplot2", "patchwork",
  # Correlação
  "Hmisc", "reshape2",
  # Modelagem
  "splines", "gnm", "dlnm"
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


# Média mensal da temperatura mínima (°C)

summary(dta$temp_min_mes)


# Média mensal da temperatura mínima menos 5°C

summary(dta$temp_min_mes_menos5C)


# Média mensal da temperatura máxima (°C)

summary(dta$temp_max_mes)


# Média mensal da temperatura máxima mais 5°C

summary(dta$temp_max_mes_mais5C)


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



### Plot temperatura média ####

p <- ggplot(dta, aes(x = temp_media)) +
  geom_histogram(bins = 20, fill = "gray45", color = "white") +
  xlab("\nTemperatura média (°C)") +
  ylab("Frequência \n") +
  ggtitle("") +
  scale_y_continuous(breaks = seq(0, 600, by = 100), limits = c(0, 600)) +
  theme_minimal(base_size = 14)
p

ggsave(filename = file.path("plots_temp", "temp_media.jpg"),
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


### Plots temperaturas diárias por ano ####


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
    geom_line(size = 1) +
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
  
  print(p)
}


# Plot 2

dta$data <- as.Date(dta$data)  

p <- ggplot(dta, aes(x = data, y = temp_media)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("\n Anos") +
  ylab("Temperatura média (°C) \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 1))
p

ggsave(filename = file.path("plots_temp", "temp_anos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


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


### Plots óbitos por ano ####

p <- ggplot(dta, aes(x = data, y = obitos_total)) +
  geom_point(color = "gray45", size = 1.5) +
  xlab("\n Anos") +
  ylab("Óbitos totais \n") +
  ggtitle("") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 150, by = 50), limits = c(0, 150)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(hjust = 1))

p

ggsave(filename = file.path("plots_temp_obit", "obitos_anos.jpg"),
       plot = p,
       width = 21, height = 12, units = "in", dpi = 300)


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


### Plot correlação ####


# Preparar dados: remover colunas indesejadas e linhas com NAs, converter para numérico
dta_limpo <- sapply(na.omit(dta[, !names(dta) %in% c("data", "obitos_infantil")]), 
                    as.numeric)

# Calcular correlação de Spearman
cor_res <- rcorr(dta_limpo, type = "spearman")
cor_matrix <- cor_res$r
p_values <- cor_res$P

# Criar matriz de texto com valores arredondados
cor_text <- ifelse(!is.na(cor_matrix), 
                   paste0(round(cor_matrix, 2), ifelse(p_values < 0.05, "*", "")), "")

# Definir nomes das linhas/colunas
colnames(cor_matrix) <- rownames(cor_matrix) <- colnames(dta_limpo)

# Mostrar apenas metade inferior
cor_matrix[upper.tri(cor_matrix)] <- NA
cor_df <- melt(cor_matrix)
cor_df <- cor_df[!upper.tri(cor_matrix),]

# Plot
ggplot(cor_df, aes(x = Var1, y = Var2, fill = value, label = sprintf("%.2f", value))) +
  geom_tile() +
  geom_text(color = "black", size = 5) +
  scale_fill_gradient2(low = "#253C9C", mid = "white", high = "#D64933",
                       midpoint = 0, limits = c(-1, 1), na.value = "gray") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        panel.grid = element_line(color = "gray95")) +
  labs(x = "", y = "")

ggsave("correlacao.jpg", width = 21, height = 12, units = "in", dpi = 300)





## Modelo ####



### 1. Modelo de Poisson condicional com termo DNLM para temperatura ####
# DLNMs: Modelos não lineares de defasagem (lags) distribuída.
# Objetivo: investigar a associação entre temperatura e mortalidade. 
# Cada linha representa uma data.

# Criamos uma variável de estrato (ano, mês e dia da semana) 
# para controlar sazonalidade e variação semanal.

dta <- dta |> 
  mutate(strata = paste(year(data),
                        month(data),
                        wday(data, label = TRUE), 
                        sep = ":") |> factor())

View(dta)


# Splines cúbicos naturais permitem modelar associações não lineares entre temperatura e mortalidade.
# pred_knots define os "nós" do spline, aqui nos quantis 10%, 75% e 90% da temperatura.

pred_knots <- quantile(dta$temp_media, c(10, 75, 90)/100, na.rm = TRUE)
pred_knots # valores dos nós em °C


# Definimos o número máximo de lags (n_lag = 21 dias) e os nós para o spline de lags
# logknots() concentra mais nós nos primeiros dias (efeitos imediatos) 
# e menos nos lags longos (efeitos graduais).

n_lag <- 21

lag_knots <- logknots(n_lag, nk = 3)
lag_knots


# Visualizar a distribuição dos lags e dos nós do spline de lags.
# Nós (lagknots) estão destacados para mostrar onde o efeito pode mudar rapidamente.

df <- data.frame(lag = 1:n_lag, tipo = "Lags")
df_knots <- data.frame(lag = lag_knots, tipo = "Lagknots")

ggplot() +
  geom_point(data = df, aes(x = lag, y = 1, color = tipo), size = 3) +
  geom_point(data = df_knots, aes(x = lag, y = 1, color = tipo), size = 4, shape = 15) +
  scale_color_manual(values = c("Lags" = "#253C9C", "Lagknots" = "#D64933")) +
  scale_x_continuous(breaks = 1:n_lag) +
  labs(x = "\n Lag (dias)", y = "", color = "", 
       title = "Distribuição de 3 nós (lagknots) em 21 lags \n") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("lagknots.jpg", width = 21, height = 12, units = "in", dpi = 300)


# Criamos a cross-basis combinando temperatura (exposição) e lags de forma não linear.
# argvar: spline cúbico natural para temperatura, com nós em pred_knots.
# arglag: spline cúbico natural para lags, com nós em lag_knots.

cb1 <- crossbasis(dta$temp_media,
                  lag = n_lag,
                  argvar = list(fun = "ns", knots = pred_knots),
                  arglag = list(fun = "ns", knots = lag_knots))

summary(cb1)

count(dta)
summary(dta$temp_media)
n_lag
pred_knots
lag_knots


# cb1: cross-basis com 4018 observações (dias) e 20 colunas (graus de liberdade do spline).
# Os 20 coeficientes correspondem a todas as combinações possíveis dos 4 graus de liberdade
# da temperatura com os 5 graus de liberdade dos lags.
# coeficientes = df temperatura × df lag = 4 × 5 = 20.
# As primeiras 21 linhas são NA porque o cálculo dos efeitos considera os 21 dias anteriores, 
# que não estão disponíveis no início da série.

View(cb1)


# Ajuste do modelo CP-DLNM:
# - Inclui a cross-basis (cb1) para temperatura e lags
# - Estrato (strata) controla sazonalidade e variação semanal
# - Quasi-Poisson para lidar com superdispersão em contagens de óbitos

modelo1 <- gnm(obitos_total ~ cb1,
               eliminate = strata,
               family = quasipoisson(),
               data = dta)

summary(modelo1)  # 20 coeficientes correspondentes ao termo cb1


# Valores extremos e central da temperatura média

# Temperatura média mínima
temp_media_min <- min(dta$temp_media)
temp_media_min

# Mediana da temperatura média
temp_media_mediana <- median(dta$temp_media)
temp_media_mediana

# Temperatura média máxima
temp_media_max <- max(dta$temp_media)
temp_media_max


# Gera a superfície temperatura–lag–mortalidade a partir do modelo ajustado:
# - Eixo x: temperatura (°C)
# - Eixo y: lags (dias após a exposição)
# - Eixo z: risco de mortalidade relativo
# A superfície mostra como o efeito da temperatura sobre a mortalidade varia ao longo do tempo.
# Centralizada na temperatura mediana; permite calcular efeitos cumulativos ao longo dos lags.
# Posteriormente, pode-se centralizar na MMT (temperatura de menor mortalidade).

pred1 <- crosspred(cb1,                                     
                   modelo1,
                   at = seq(temp_media_min, temp_media_max, by = 0.1),
                   cen = temp_media_mediana,
                   cumul = TRUE)                    

plot(pred1, xlab = "Temperatura (°C)", 
     ylab = "Lag (dias)", 
     zlab = "Risco Relativo",
     main = "Superfície de associação temperatura–mortalidade")

dev.copy(jpeg, filename = "superficie.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()


# Redução da superfície temperatura–lag–mortalidade apenas para a dimensão da temperatura.
# - Calcula a curva de risco relativo (RR) da temperatura sobre a mortalidade.
# - Resume os coeficientes necessários para reconstruir a curva completa.
# - Grade fina de 0,1 °C, centralizada na temperatura mediana.

red1 <- crossreduce(cb1,
                    modelo1,
                    at = seq(temp_media_min, temp_media_max, by = 0.1),
                    cen = temp_media_mediana)

plot(red1, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.5, 3.0, by = 0.5))

abline(v = temp_media_mediana, lty = 3, col = "black")

legend(x = 27.8, y = 0.9, 
       legend = paste("Mediana:", round(temp_media_mediana, 1), "°C"),
       lty = 3, col = "black", bty = "n")

dev.copy(jpeg, filename = "curva_temp_mort.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()



# Redução da superfície temperatura–lag–mortalidade apenas ao longo da dimensão do lag

# Percentis de interesse
percentis <- c(0.05, 0.10, 0.75, 0.90, 0.95)
percentis_temp <- quantile(temp_media, probs = percentis, na.rm = TRUE)

# Criar lista com curvas lag-resposta
red_list <- lapply(percentis_temp, function(t) crossreduce(
  cb1, modelo1, type = "var", cen = temp_media_mediana, value = t))

# Nomes das curvas
percentis_labels <- c("5º Percentil", "10º Percentil", "75º Percentil", "90º Percentil", "95º Percentil")
legend_text <- paste0(percentis_labels, ": ", round(percentis_temp, 1), "°C")

# Cores e estilo
cores <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF", "#E6AB02FF")
lty <- 1

# Plot inicial
plot(red_list[[1]],
     xlab = "Lag (dias)",
     ylab = "Risco Relativo",
     main = paste0("Curvas lag-resposta para diferentes temperaturas (ref. ", 
                   round(temp_media_mediana,1), "°C) \n"),
     ylim = c(0.95, 1.20),
     col = cores[1],
     lty = lty,
     lwd = 2,
     yaxt = "n",
     xaxt = "n",
     ci = "n")

axis(2, at = seq(0.95, 1.20, by = 0.05))
axis(1, at = seq(0, 21, by = 1))

# Adicionar demais curvas 
invisible(lapply(2:length(red_list), 
                 function(i) lines(red_list[[i]], col = cores[i], lty = lty, lwd = 2)))

# Legenda
legend(x = -0.55, y = 1.22, legend = legend_text, 
       col = cores, lty = lty, lwd = 2, bty = "n", y.intersp = 0.2)

# Salvar
dev.copy(jpeg, filename = "temp_mort_lag.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()



# Associação entre Temperatura e Mortalidade em Lags específicos

# Lags e cores
lags_plot <- c(0, 1, 3, 5, 6, 15)
cores <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF", "#E6AB02FF", "#66A61EFF")

# Criar plot vazio
plot(NA, xlim = range(pred1$predvar), ylim = c(0.95, 1.20), 
     xlab = "Temperatura (°C)", 
     ylab = "Risco Relativo", 
     main = paste0("Associação entre temperatura e mortalidade em lags específicos \n"), 
     xaxt = "n", yaxt = "n",
     bty = "l")

axis(2, at = seq(0.95, 1.20, by = 0.05), labels = TRUE, tck = -0.01)
axis(1, at = seq(12, 32, by = 1), labels = TRUE, tck = -0.01)

abline(h = 1, lty = 1, col = "black")

# Adicionar curvas para cada lag
invisible(lapply(seq_along(lags_plot), function(i){
  lag_atual <- lags_plot[i]
  lines(pred1$predvar, pred1$matRRfit[, lag_atual + 1],
        col = cores[i], lwd = 2, lty = 1)
}))

# Legenda
legend(x = 11.5, y = 1.22,
       legend = paste0("Lag ", lags_plot, " dias"),
       col = cores, lty = 1, lwd = 2, bty = "n", y.intersp = 0.2)

# Salvar
dev.copy(jpeg, filename = "temp_mort_lag_especificos.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()



# No modelo original (modelo1), existem 20 coeficientes que produzem a superfície
# temperatura–lag–mortalidade. Essa superfície combina os 4 graus de liberdade da temperatura
# com os 5 graus de liberdade dos lags. A matriz de variância–covariância é 20x20,
# mostrando a incerteza e a correlação entre todos esses coeficientes.

modelo1$coefficients
vcov(modelo1)

# Após reduzir a superfície ao longo dos lags usando crossreduce (red1),
# apenas 4 coeficientes são necessários para calcular a curva temperatura–mortalidade.
# A matriz de variância–covariância desses 4 coeficientes é 4x4,
# refletindo a incerteza da curva resumida.

red1$coefficients
red1$vcov


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
  }
}


# Neste exemplo, usamos 22.3 °C como temperatura de centralização.
# Neste caso, não difere muito da mediana da série.
# Em outros estudos, a temperatura de menor risco pode ser bem diferente da mediana.

temp_media_mediana
get_cen(red1)      # aqui obtemos a MMT estimada


# O "by" na função crossreduce controla a precisão da curva e da MMT estimada.
# Quanto menor, mais precisa é a localização do mínimo.
# Você pode redefinir red1 usando by = 0.01 para a grade de temperaturas
# e executar novamente get_cen(red1) para ver como muda a MMT.
# Neste exemplo, a mudança é mínima porque a parte inferior da curva é plana.
# Geralmente, usar by = 0.1 (0,1 °C) já é suficiente, dependendo da faixa de temperaturas observada.

# Reexecutamos e plotamos o modelo usando a nova temperatura de centralização (MMT).
red1 <- crossreduce(cb1,
                    modelo1,
                    at = seq(temp_media_min, temp_media_max, by = 0.1),
                    cen = get_cen(red1)) 

# Estimar MMT
mmt <- get_cen(red1)

# Variáveis
temp <- red1$predvar
rr <- red1$RRfit

# Separar abaixo e acima da MMT
abaixo <- temp <= mmt
acima <- temp >= mmt

# Plot
plot(red1, xlab = "Temperatura (ºC)", ylab = "Risco Relativo",
     main = "Curva cumulativa de associação entre temperatura e mortalidade \n",
     xaxt = "n", yaxt = "n", ylim = c(0.5, 3.0), lwd = 2)
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.5, 3.0, by = 0.5))

lines(temp[acima], rr[acima], col = "#D64933", lwd = 2)
lines(temp[abaixo], rr[abaixo], col = "#253C9C", lwd = 2)

abline(v = mmt, lty = 3)

legend(x = 25.7, y = 0.9,
       legend = paste0("Temperatura mínima de mortalidade: ", round(mmt, 1), " ºC"),
       lty = 3, col = "black", bty = "n", cex = 1)

dev.copy(jpeg, filename = "curva_temp_mort_MMT.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()



#### Temperatura e a mortalidade (percentis e absoluta) ####

# Criar data.frame com os resultados da curva temperatura–mortalidade (crossreduce)
df_red1 <- data.frame(temp = red1$predvar,
                      rr = red1$RRfit,
                      rr_low = red1$RRlow,
                      rr_high = red1$RRhigh)

# Função de distribuição empírica (ECDF) para converter temperaturas em percentis
ecdf_fun <- ecdf(dta$temp_media)

# Adicionar coluna de percentis (0–100) no df_red1
df_red1$temp_percentile <- ecdf_fun(df_red1$temp) * 100

# Calcular os valores absolutos de temperatura nos percentis de interesse
percentis_v2 <- c(0.01, 0.25, 0.50, 0.75, 0.99)
percentis_temp_v2 <- quantile(dta$temp_media, percentis_v2, na.rm = TRUE)

# Linhas de referência
vlines <- geom_vline(xintercept = percentis_temp_v2, linetype = "dashed", color = "grey40")
vlines_perc <- geom_vline(xintercept = percentis_v2 * 100, linetype = "dashed", color = "grey40")
hline <- geom_hline(yintercept = 1, linetype = "solid", color = "black")

# Plot a: Associação com Temperatura Relativa (percentis)
p_a <- ggplot(df_red1, aes(x = temp_percentile, y = rr)) +
  geom_ribbon(aes(ymin = rr_low, ymax = rr_high), fill = "grey80", alpha = 0.8) +
  geom_line(color = "black", linewidth = 1) +
  vlines_perc + hline +
  scale_y_log10(breaks = c(0.8, 1, 1.5, 2, 2.5)) +
  labs(title = "Temperatura relativa (percentis) \n",
       x = "",
       y = "Risco Relativo (escala logarítmica) \n") +
  theme_minimal(base_size = 14)
p_a

# Plot b: Associação com Temperatura Absoluta (°C)
p_b <- ggplot(df_red1, aes(x = temp, y = rr)) +
  geom_ribbon(aes(ymin = rr_low, ymax = rr_high), fill = "grey80", alpha = 0.8) +
  geom_line(color = "black", linewidth = 1) +
  vlines + hline +
  scale_x_continuous(breaks = seq(12, 31, by = 1), limits = c(12, 31)) +
  scale_y_log10(breaks = c(0.8, 1, 1.5, 2, 2.5)) +
  labs(title = "Temperatura absoluta (°C) \n",
       x = "",
       y = "") +
  theme_minimal(base_size = 14)
p_b

# Plot c: Distribuição da Temperatura Relativa (percentis)
p_c <- ggplot(dta, aes(x = temp_percentile)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, fill = "grey60", color = "white") +
  vlines_perc +
  scale_y_continuous(breaks = seq(0.00, 0.15, by = 0.05), limits = c(0.00, 0.15)) +
  labs(title = "Distribuição da temperatura (percentis) \n",
       x = " \n Percentis de temperatura \n",
       y = "Densidade \n") +
  theme_minimal(base_size = 14)
p_c

# Plot d: Distribuição da Temperatura Absoluta (°C)
p_d <- ggplot(dta, aes(x = temp_media)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "grey60", color = "white") +
  vlines +
  scale_x_continuous(breaks = seq(12, 31, by = 1), limits = c(12, 31)) +
  labs(title = "Distribuição da temperatura (°C) \n",
       x = "\n Temperatura (°C) \n",
       y = "") +
  theme_minimal(base_size = 14)
p_d

# Combinar os plots
final_plot <- (p_a | p_b) / (p_c | p_d) +
  plot_annotation(
    caption = 'As linhas verticais tracejadas indicam os percentis 1º, 25º, 50º, 75º e 99º.
A área sombreada representa o intervalo de confiança de 95%.'
  ) &
  theme(plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot")

final_plot

ggsave("temp_mort_log_density.png", 
       plot = final_plot, 
       width = 21, 
       height = 12, 
       units = "in", 
       dpi = 300)



### 2. Adicionando um termo de interação com um modificador de nível diário ####

# Podemos querer investigar se há uma interação entre a temperatura e alguma
# outra variável diária ao modelar a mortalidade. Como exemplo, vamos verificar
# se esses efeitos são mais intensos durante o fim de semana.


# Primeiro, criamos uma variável dummy para indicar se o dia é fim de semana ou não
dta <- dta |> 
  mutate(
    # variável auxiliar: segunda=1, ..., domingo=7
    wday = wday(data, week_start = 1),
    # fim de semana = sábado(6) ou domingo(7)
    fim_semana = ifelse(wday %in% c(6,7), 1, 0),
    # dia de semana = segunda(1) a sexta(5)
    dia_semana = ifelse(wday %in% 1:5, 1, 0)
  )

dta <- dta |> select(-wday)


# Em seguida, definimos uma cross-basis de interação. 
# Fazemos isso separadamente para dias de semana e finais de semana, 
# para que possamos obter facilmente ambos os efeitos. 
# Por "facilmente", queremos dizer que o modelo será ajustado duas vezes, 
# em vez de precisar combinar os coeficientes do efeito "principal" da temperatura (cb1) 
# com os coeficientes dos termos de interação.

cb_int1 <- cb1 * dta$fim_semana
cb_int2 <- cb1 * dta$dia_semana


# Para visualizar o efeito de interação, criamos dois novos modelos. 
# Ambos são versões "atualizadas" do modelo1, que modela a associação 
# entre temperatura e mortalidade sem termos de interação.

# modelo_int1 inclui um termo de interação para dias de semana. 
# Isso significa que os parâmetros do efeito principal (de cb1) 
# capturam a associação entre temperatura e mortalidade nos *fins de semana*, 
# ou seja, neste modelo_int1, os fins de semana são a categoria de referência.

# modelo_int2 inclui um termo de interação para fins de semana. 
# Isso significa que os parâmetros do efeito principal (de cb2) 
# capturam a associação entre temperatura e mortalidade nos *dias de semana*.

# É possível predizer ambas as curvas temperatura-mortalidade com um único modelo; 
# entretanto, é mais fácil estimar múltiplos modelos, recodificando o grupo de referência cada vez. 
# Esses dois modelos são numericamente equivalentes (mesmo ajuste), 
# mas a interpretação do termo cb muda de forma conveniente para nossa análise.

modelo_int1 <- update(modelo1, . ~ . + cb_int2) # grupo de referência: Fim de semana
modelo_int2 <- update(modelo1, . ~ . + cb_int1) # grupo de referência: Dia de semana


# Neste caso, o modelo não pode incluir um “efeito principal”
# para o indicador fim de semana/dia de semana, porque dentro dos estratos, 
# o indicador fim de semana/dia de semana é constante dentro dos estratos de ano/mês/dia da semana. 
# No entanto, um termo de interação pode ser adicionado. 
# Em outras palavras, espera-se que o coeficiente para fim de semana não esteja disponível
# se você incluí-lo diretamente no modelo:
# modelo_int1 <- update(modelo1, . ~ . + as.factor(weekend) + cb_int2)  

# Comparação estatística entre o modelo sem termo de interação e o modelo com termo de interação,
# para testar se fim de semana vs. dia de semana é um modificador significativo.
anova(modelo1, modelo_int1, test = "Chisq")
# Comparar modelo1 e modelo_int2 produz o mesmo valor de p, 
# então apenas uma comparação é necessária.
anova(modelo1, modelo_int2, test = "Chisq")


# Estimamos a curva temperatura-mortalidade para cada modelo.
red_int1 <- crossreduce(cb1, modelo_int1, at = seq(
  temp_media_min, temp_media_max, by = 0.1), cen = temp_media_mediana)
red_int2 <- crossreduce(cb1, modelo_int2, at = seq(
  temp_media_min, temp_media_max, by = 0.1), cen = temp_media_mediana)


# As MMTs podem não ser exatamente iguais para cada curva:
get_cen(red_int1)
get_cen(red_int2)

# Podemos centralizar cada curva em sua própria MMT
red_int1_own_MMT <- crossreduce(cb1, modelo_int1, cen = get_cen(red_int1))  
red_int2_own_MMT <- crossreduce(cb1, modelo_int2, cen = get_cen(red_int2))  


# Agora podemos plotar a curva temperatura-mortalidade para os casos de fim de semana e dia de semana.
# Ao interpretar este gráfico, ambas as curvas são plotadas em referência à sua própria
# temperatura de mortalidade mínima.

# Vamos sobrepor as duas curvas, então é útil escolher uma combinação de cores fácil de ler.
plot_col <- viridisLite::viridis(4)
plot_col 

plot(red_int1_own_MMT, 
     xlab = "\nTemperatura (ºC)", 
     ylab = "Risco Relativo", 
     main = "Curva temperatura–mortalidade",
     col = plot_col[1], 
     ci.arg = list(col = alpha(plot_col[1], 0.2)),
     xaxt = "n", yaxt = "n", lwd = 2)
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.5, 3.0, by = 0.5))

lines(red_int2_own_MMT, ci = "area", col = plot_col[3], 
      ci.arg = list(col = alpha(plot_col[3], 0.2), lwd = 2))

legend(x = 11.3, y = 3.05,
       legend = c("Fim de semana", "Dia de semana"),
       col = c(plot_col[1], plot_col[3]), 
       lty = 1, 
       lwd = 2,
       bty = "n",
       cex = 1.1,
       y.intersp = 0.2)

# Valores das MMTs
mmt_int1 <- round(get_cen(red_int1), 1)  # MMT fim de semana
mmt_int2 <- round(get_cen(red_int2), 1)  # MMT dia de semana

# Valor p do teste qui-quadrado
anova_result <- anova(modelo1, modelo_int1, test = "Chisq")
p_value <- signif(anova_result$`Pr(>Chi)`[2], 3)  # valor p do segundo modelo comparado

# Legenda MMTs
legend(x = 27.8, y = 0.97,
       legend = c(paste0("MMT Fim de semana: ", mmt_int1, " ºC"),
                  paste0("MMT Dia de semana: ", mmt_int2, " ºC")),
       bty = "n",
       cex = 1,
       y.intersp = 0.2)

# Legenda do teste de comparação
legend(x = 27.65, y = 0.95,
       legend = paste0("Teste interação (Chi2): p = ", p_value),
       bty = "n", cex = 1)

dev.copy(jpeg, filename = "curva_temp_mort_MMT_semana.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()




