#------------------------------------------------------------------------------#
#                                                                              #
#                      Problem set 2 - Predicting poverty                      #
#                          Descriptive Statistics                              #
#------------------------------------------------------------------------------#

#1. Preliminares

rm(list=ls())

if (!require("pacman")) install.packages("pacman")
p_load(rio, # import/export data
       tidyverse, # tidy-data
       caret, # For predictive model assessment
       gridExtra, # arrange plots
       skimr, # summarize data 
       stargazer, #model viz and descriptive statistics
       rvest,
       ggcorrplot,
       ggplot2,
       broom,
       knitr,
       mosaic,
       stats,
       boot,
       ggbeeswarm,
       ggdist,
       gghalves,
       moments,
       corrplot,
       visdat,
       kabbleExtra,
       summarytools,
       patchwork
) 

#2. Importación 

train <- read.csv("stores/train.csv")
train_deleted <- read.csv("stores/train_deleted.csv")
test <- read.csv("stores/test.csv")

head(test)
head(train)
head(test)
head(train)


#3. CLasificación variables

train <- train %>%
  mutate(propia = if_else(propia_pagada == "Sí" | propia_paga == "Sí", 1, 0))

test <- test %>%
  mutate(propia = if_else(propia_pagada == "Sí" | propia_paga == "Sí", 1, 0))

train <- train %>%
  mutate(propia = factor(propia, levels = c(0, 1), labels = c("No", "Sí")))

test <- test %>%
  mutate(propia = factor(propia, levels = c(0, 1), labels = c("No", "Sí")))

var_bin <- c("Pobre", "H_Head_mujer", "H_Head_trabaja", "arrienda", "propia")


var_t <- c(
  "Npersug",
  "personas_cuarto_total",
  "n_mujeres",
  "n_menores",
  "n_mayores_60",
  "n_trabaja",
  "n_busca_trab",
  "n_afiliado_salud",
  "n_estudiantes",
  "n_ocupados",
  "n_desocupados",
  "n_inactivos"
)

var_num <- c("Ingpcug", "arriendo_estimado_pc", "Prom_anios_educ")

#4. Estadísticas descriptivas

# Tabla

label(train$Npersug) <- "Personas en el hogar"
label(train$personas_cuarto_total) <- "Personas por cuarto"
label(train$n_mujeres) <- "Número de mujeres"
label(train$n_menores) <- "Número de menores"
label(train$n_mayores_60) <- "Número de mayores de 60"
label(train$n_trabaja) <- "Trabajan en el hogar"
label(train$n_busca_trab) <- "Buscan trabajo"
label(train$n_afiliado_salud) <- "Afiliados a salud"
label(train$n_estudiantes) <- "Estudiantes"
label(train$n_ocupados) <- "Ocupados"
label(train$n_desocupados) <- "Desocupados"
label(train$n_inactivos) <- "Inactivos"


tabla_descr <- descr(train[var_t], stats = c("mean", "sd", "med", "min", "max"), transpose = TRUE)
sink("views/tabla_descriptiva.tex")
print(tabla_descr, report = "latex")
sink()

# Variables binarias

train <- train %>%
  mutate(H_Head_mujer      = factor(H_Head_mujer, levels = c("Hombre", "Mujer"), labels = c("No", "Sí")))

labels_bin <- c(
        Pobre = "Household in poverty",
        H_Head_mujer = "Female head",
        H_Head_trabaja = "Head works",
        arrienda = "Rents home",
        propia = "Owns home"
)



df_bin_long <- train %>%
                select(all_of(var_bin)) %>%
                pivot_longer(
                  cols = everything(),
                  names_to = "Variable",
                  values_to = "Valor"
                ) %>%
                group_by(Variable, Valor) %>%
                summarise(n = n(), .groups = "drop") %>%
                group_by(Variable) %>%
                mutate(porcentaje = n / sum(n) * 100)

df_bin_long <- df_bin_long %>%
  mutate(Variable_label = labels_bin[Variable])

# Gráfico de barras apiladas
gr_bin <- ggplot(df_bin_long, aes(x = Variable_label, y = porcentaje, fill = Valor)) +
          geom_col(position = "stack") +
          geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
                    position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
                labs(
                  title = "Percentage Distribution of Responses (Yes / No)",
                  x = "",
                  y = "Percentage",
                  fill = "Response"
                ) +
          theme_minimal() +
          scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#a6cee3"))

ggsave("views/gr_bin.png", plot = gr_bin, width = 10, height = 5, dpi = 300, bg = "white")

# Variable numéricas

# Distribución de ingreso

p1 <- ggplot(train_deleted %>%
               filter(Ingtotug < quantile(Ingtotug, 0.95, na.rm = TRUE)),
             aes(x = Ingtotug)) +
              geom_histogram(binwidth = 200000, fill = "#2c7fb8", color = "white", alpha = 0.8) +
                    labs(
                      title = "Total Household Income (< 95th Percentile)",
                      x = "Pesos",
                      y = "Frequency"
                    ) +
              theme_minimal()

# Histograma 2: Ingreso per cápita por unidad de gasto (en millones)
p2 <- ggplot(train_deleted %>%
               filter(Ingpcug < quantile(Ingpcug, 0.95, na.rm = TRUE)),
             aes(x = Ingpcug)) +
                geom_histogram(binwidth = 200000, fill = "#2c7fb8", color = "white", alpha = 0.8) +
                    labs(
                      title = "Per Capita Income by Consumption Unit (< 95th Percentile)",
                      x = "Pesos",
                      y = "Frequency"
                    ) +
                theme_minimal()

# Combinar con patchwork
gr_ingresos <- p1 + p2
ggsave("views/gr_ingresos.png", plot = gr_ingresos, width = 10, height = 5, dpi = 300)


