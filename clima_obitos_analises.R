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
  "ggplot2",
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


### Plot correlação ####


# Preparar dados: remover colunas indesejadas e linhas com NAs, converter para numérico
dta_clean <- sapply(na.omit(dta[, !names(dta) %in% c("data", "obitos_infantil")]), 
                    as.numeric)

# Calcular correlação de Spearman
cor_res <- rcorr(dta_clean, type = "spearman")
cor_matrix <- cor_res$r
p_values <- cor_res$P

# Criar matriz de texto com valores arredondados
cor_text <- ifelse(!is.na(cor_matrix), 
                   paste0(round(cor_matrix, 2), ifelse(p_values < 0.05, "*", "")), "")

# Definir nomes das linhas/colunas
colnames(cor_matrix) <- rownames(cor_matrix) <- colnames(dta_clean)

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
# DLNMs: Modelos não lineares de defasagem distribuída.



# Objetivo: investigar a associação entre temperatura e mortalidade. 
# Cada linha representa uma data.
# Criamos uma variável de estrato (ano, mês e dia da semana) 
# para controlar sazonalidade e variação semanal.

dta <- dta |> 
  mutate(strata = paste(year(data),
                        month(data),
                        wday(data, label = TRUE), 
                        sep = ":") |> factor())



# Splines cúbicos naturais permitem modelar associações não lineares entre temperatura e mortalidade.
# pred_knots define os "nós" do spline, aqui nos quantis 10%, 75% e 90% da temperatura.

pred_knots <- quantile(dta$temp_media, c(10, 75, 90)/100, na.rm = TRUE)
pred_knots # valores dos nós em °C



# Definimos o número máximo de lags (n_lag = 21 dias) e os nós para o spline de defasagem.
# logknots() concentra mais nós nos primeiros dias (efeitos imediatos) 
# e menos nos lags longos (efeitos graduais).

n_lag <- 21

lag_knots <- logknots(n_lag, nk = 3)
lag_knots



# Visualizar a distribuição dos lags e dos nós do spline de defasagem.
# Nós (lagknots) estão destacados para mostrar onde o efeito pode mudar rapidamente.

# Data frame com todos os lags
df <- data.frame(lag = 1:n_lag, tipo = "Lags")

# Data frame apenas com os nós
df_knots <- data.frame(lag = lag_knots, tipo = "Lagknots")

# Plot
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



# Criamos a cross-basis combinando temperatura (exposição) e defasagem (lags) de forma não linear.
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
# Os 20 coeficientes correspondem a todas as combinações possíveis dos 4 graus de liberdade da temperatura 
# com os 5 graus de liberdade da defasagem (lags).
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

# Temperatura média máxima
temp_media_max <- max(dta$temp_media)
temp_media_max

# Mediana da temperatura média
temp_media_mediana <- median(dta$temp_media)
temp_media_mediana



# Gera a superfície temperatura–lag–mortalidade a partir do modelo ajustado:
# - Eixo x: temperatura (°C)
# - Eixo y: lags (dias após a exposição)
# - Eixo z: risco de mortalidade relativo
# A superfície mostra como o efeito da temperatura sobre a mortalidade varia ao longo do tempo.
# Inicialmente, centralizada na temperatura mediana; permite calcular efeitos cumulativos ao longo dos lags.
# Posteriormente, pode-se centralizar na MMT (temperatura de menor mortalidade).

pred1 <- crosspred(cb1,                                     
                   modelo1,
                   at = seq(temp_media_min, temp_media_max, by = 0.1),
                   cen = temp_media_mediana,
                   cumul = TRUE)                    

# Plot

plot(pred1, xlab = "Temperatura (°C)", 
     ylab = "Lag (dias)", 
     zlab = "RR",
     main = "Superfície de Associação Temperatura–Mortalidade")

dev.copy(jpeg, filename = "superficie.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()



# Redução da superfície temperatura–lag–mortalidade apenas para a dimensão da temperatura
# - Calcula a curva de risco relativo (RR) da temperatura sobre a mortalidade
# - Resume os coeficientes necessários para reconstruir a curva completa
# - Grade fina de 0,1 °C, centralizada na temperatura mediana

red1 <- crossreduce(cb1,
                    modelo1,
                    at = seq(temp_media_min, temp_media_max, by = 0.1),
                    cen = temp_media_mediana)

# Plot

plot(red1, xlab = "Temperatura (°C)", ylab = "Risco Relativo",
     main = "Curva de Associação Temperatura–Mortalidade",
     xaxt = "n", yaxt = "n", ylim = c(0.8, 2.4))
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.8, 2.4, by = 0.2))

# Sobrepõe pontos vermelhos para mostrar exatamente em quais valores de temperatura 
# a curva de risco relativo foi estimada.
points(red1$predvar, red1$RRfit, col = "#D64933")

legend(x = 29, y = 0.9,
       legend = "Valores estimados", 
       col = "#D64933", pch = 1, bty = "n")

dev.copy(jpeg, filename = "curva_temp_mort.jpg", width = 21, height = 12, units = "in", res = 300)
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





# Queremos garantir que a curva de risco relativo seja centralizada na temperatura 
# que apresenta o menor risco de mortalidade, chamada MMT (Temperatura de Mortalidade Mínima). 
# Para isso, precisamos reexecutar as funções crosspred ou crossreduce com um novo valor de centralização (cen=).


# A função abaixo, get_cen, serve para encontrar a temperatura de menor risco.
# Ela aceita a saída de crosspred ou crossreduce e retorna o valor da temperatura correspondente.

get_cen <- function(crosspred) {
  # Precisamos deste if porque a saída de crosspred e crossreduce é ligeiramente diferente.
  # Queremos que a função funcione para ambos os casos.
  if(!is.null(crosspred$fit)) {
    # Para crosspred: retorna a temperatura com o menor risco relativo
    crosspred$predvar[which.min(crosspred$fit)]
  } else if(!is.null(crosspred$allfit)) {
    # Para crossreduce: retorna a temperatura com o menor risco relativo
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
get_cen(red1)  # aqui obtemos a MMT estimada


# Observação didática sobre o parâmetro "by":
# O passo "by" na função crossreduce controla a precisão da curva e da MMT estimada.
# Quanto menor o passo, mais precisa é a localização do mínimo.
# Como exercício, você pode redefinir red1 usando by = 0.01 para a grade de temperaturas
# e executar novamente get_cen(red1) para ver como muda a MMT.
# Neste exemplo, a mudança é mínima porque a parte inferior da curva é plana.
# Geralmente, usar by = 0.1 (0,1 °C) já é suficiente, dependendo da faixa de temperaturas observada.





# Reexecutamos e plotamos o modelo usando a nova temperatura de centralização (MMT).
# Não é estritamente necessário usar uma grade mais fina aqui,
# a grade padrão já produz uma curva suave e adequada.

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
below <- temp <= mmt
above <- temp > mmt

# Plot base do red1
plot(red1, xlab = "Temperatura", ylab = "Risco Relativo (RR)",
     main = "Curva Temperatura–Mortalidade",
     xaxt = "n", yaxt = "n", ylim = c(0.8, 2.4))
axis(1, at = seq(12, 32, by = 1))
axis(2, at = seq(0.8, 2.4, by = 0.2))

# Adicionar linhas coloridas
lines(temp[below], rr[below], col = "#253C9C", lwd = 2)
lines(temp[above], rr[above], col = "#D64933", lwd = 2)

# Linha pontilhada na MMT
abline(v = mmt, lty = 3)

dev.copy(jpeg, filename = "curva_temp_mort_MMT.jpg", width = 21, height = 12, units = "in", res = 300)
dev.off()




