#------------------------------------------------------------------------------#
#                                                                              #
#                      Problem set 2 - Predicting poverty                      #
#                         Data import and procesing                            #
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
       kabbleExtra
) 

#2. Importación 

train_hogares <- read.csv("stores/train_hogares.csv")
test_hogares <- read.csv("stores/test_hogares.csv")
train_personas <- read.csv("stores/train_personas.csv")
test_personas <- read.csv("stores/test_personas.csv")

head(test_hogares)
head(train_hogares)
head(test_personas)
head(train_personas)

### 3.Preprocesamiento

## 3.1. Identificación de diferencias de las bases train y test

# Base hogares
skim(train_hogares) %>% head() # 23 variables, 164960 observaciones
skim(test_hogares) %>% head() # 16 variables, 66168 observaciones
var_faltantes_hogares <- setdiff(names(train_hogares), names(test_hogares)) # Se tienen 7 variables menos
var_faltantes_hogares #  "Ingtotug" "Ingtotugarr" "Ingpcug" "Pobre" "Indigente" "Npobres" "Nindigentes"
var_comunes_hogares <- intersect(colnames(train_hogares), colnames(test_hogares))
var_comunes_hogares


# Base personas 
skim(train_personas) %>% head() # 135 variables, 543109 observaciones
skim(test_personas) %>% head() # 63 variables, 219644 observaciones
var_faltantes_personas <- setdiff(names(train_personas), names(test_personas)) # Se tienen 72 variables menos
var_faltantes_personas # Relevantes: Estrato1, P65-66-70-71...(Ingresos), Ingtot...   
var_comunes_personas <- intersect(colnames(train_personas), colnames(test_personas))
var_comunes_personas

skim(train_personas[var_comunes_personas])

## 3.2. Prepocesamiento base personas

# 3.2.1 Se define una función estandar para el preprocesamiento de la base personas

pre_process_personas <- function(data) {
  
  data <- data %>%
    mutate(
      mujer             = ifelse(!is.na(P6020), ifelse(P6020 == 2, 1, 0), NA),         # 1 si es mujer
      menor             = ifelse(!is.na(P6040), ifelse(P6040 <= 6, 1, 0), NA),         # 1 si es menor de 6 años
      mayor_60          = ifelse(!is.na(P6040), ifelse(P6040 >= 60, 1, 0), NA),        # 1 si es mayor de 60 años
      H_Head            = ifelse(!is.na(P6050), ifelse(P6050 == 1, 1, 0), NA),         # 1 si es jefe del hogar
      EducLevel         = ifelse(!is.na(P6050), ifelse(P6210==9,0,P6210), NA),         # Replace 9 with 0
      anios_educ        = ifelse(!is.na(P6210s1) & !P6210s1 %in% c(98, 99), P6210s1, NA), # Años de educación aprobados
      trabaja           = ifelse(!is.na(P6240), ifelse(P6240 == 1, 1, 0), NA),         # 1 si trabaja
      busca_trab        = ifelse(!is.na(P6240), ifelse(P6240 == 2, 1, 0), NA),         # 1 si busca trabajo
      ingresos_adic     = ifelse(!is.na(P6545), ifelse(P6545 == 1, 1, 0), NA),         # 1 si tuvo ingresos adicionales
      afiliado_salud    = ifelse(!is.na(P6090), ifelse(P6090 == 1, 1, 0), NA),         # 1 si está afiliado a salud
      estudiante        = ifelse(!is.na(P6240), ifelse(P6240 == 3, 1, 0), NA),         # 1 si es estudiante
      afiliado_pension  = ifelse(!is.na(P6920), ifelse(P6920 == 1, 1, 0), NA),         # 1 si está afiliado a pensión
      pensionado        = ifelse(!is.na(P6920), ifelse(P6920 == 3, 1, 0), NA),         # 1 si pensionado
      ingreso_arriendo  = ifelse(!is.na(P7495), ifelse(P7495 == 1, 1, 0), NA),         # 1 si tiene ingreso por arriendo
      ingreso_dividendos= ifelse(!is.na(P7510s5), ifelse(P7510s5 == 1, 1, 0), NA),     # 1 si tiene ingreso por dividendos
      ayuda_fam_int     = ifelse(!is.na(P7510s1), ifelse(P7510s1 == 1, 1, 0), NA),     # 1 si recibe ayuda familiar interna
      ayuda_fam_ext     = ifelse(!is.na(P7510s2), ifelse(P7510s2 == 1, 1, 0), NA),     # 1 si recibe remesas
      subsidio_gobierno = ifelse(!is.na(P7510s3), ifelse(P7510s3 == 1, 1, 0), NA),     # 1 si recibe subsidio del gobierno
      otro_subsidio     = ifelse(!is.na(P7510s7), ifelse(P7510s7 == 1, 1, 0), NA),     # 1 si recibe otro subsidio
      ocupado           = ifelse(!is.na(Oc), 1, 0),                                     # 1 si tiene ocupación registrada
      puede_trabajar    = ifelse(!is.na(Pet), 1, 0),                                    # 1 si está en edad de trabajar
      desocupado        = ifelse(!is.na(Des), 1, 0),                                    # 1 si está desocupado
      ina               = ifelse(!is.na(Ina), 1, 0)                                     # 1 si está inactivo
    ) %>%
    select(
      id, Orden, mujer, menor, mayor_60, H_Head, EducLevel, anios_educ,
      trabaja, busca_trab, ingresos_adic, afiliado_salud, estudiante,
      afiliado_pension, pensionado, ingreso_arriendo, ingreso_dividendos,
      ayuda_fam_int, ayuda_fam_ext, subsidio_gobierno, otro_subsidio,
      ocupado, puede_trabajar, desocupado, ina
    )
  
  return(data)
}


train_personas <- pre_process_personas(train_personas)
test_personas <- pre_process_personas(test_personas)


# 3.2.2 Agregación de características a nivel hogar

personas_nivel_hogar <- function(df_personas) {
  df_personas %>%
    group_by(id) %>%
    summarize(
      n_personas       = n(),
      n_mujeres        = sum(mujer, na.rm = TRUE),
      n_menores        = sum(menor, na.rm = TRUE),
      n_mayores_60     = sum(mayor_60, na.rm = TRUE),
      max_educ_level   = max(EducLevel, na.rm = TRUE),
      prom_anios_educ  = mean(anios_educ, na.rm = TRUE),
      n_trabaja        = sum(trabaja, na.rm = TRUE),
      n_busca_trab     = sum(busca_trab, na.rm = TRUE),
      n_ingresos_adic  = sum(ingresos_adic, na.rm = TRUE),
      n_afiliado_salud = sum(afiliado_salud, na.rm = TRUE),
      n_estudiantes    = sum(estudiante, na.rm = TRUE),
      n_afiliado_pens  = sum(afiliado_pension, na.rm = TRUE),
      n_pensionados    = sum(pensionado, na.rm = TRUE),
      n_arriendo       = sum(ingreso_arriendo, na.rm = TRUE),
      n_dividendos     = sum(ingreso_dividendos, na.rm = TRUE),
      n_ayuda_int      = sum(ayuda_fam_int, na.rm = TRUE),
      n_ayuda_ext      = sum(ayuda_fam_ext, na.rm = TRUE),
      n_subsidio_gob   = sum(subsidio_gobierno, na.rm = TRUE),
      n_otro_subsidio  = sum(otro_subsidio, na.rm = TRUE),
      n_ocupados       = sum(ocupado, na.rm = TRUE),
      n_puede_trab     = sum(puede_trabajar, na.rm = TRUE),
      n_desocupados    = sum(desocupado, na.rm = TRUE),
      n_inactivos      = sum(ina, na.rm = TRUE)
    ) %>%
    ungroup()
}

# Base training personas

train_personas_hogar <- train_personas %>% 
                        filter(H_Head == 1) %>%  # Selecciona al jefe del hogar (1 fila por hogar)
                        select(
                          id, mujer, EducLevel, ocupado, trabaja, estudiante, pensionado
                        ) %>% 
                        rename(
                          H_Head_mujer        = mujer,
                          H_Head_EducLevel    = EducLevel,
                          H_Head_ocupado      = ocupado,
                          H_Head_trabaja      = trabaja,
                          H_Head_estudiante   = estudiante,
                          H_Head_pensionado   = pensionado
                        ) %>% 
                        left_join(personas_nivel_hogar(train_personas), by = "id")


# Base test personas


test_personas_hogar <- test_personas %>% 
                          filter(H_Head == 1) %>%  # Selecciona al jefe del hogar (1 fila por hogar)
                          select(
                            id, mujer, EducLevel, ocupado, trabaja, estudiante, pensionado
                          ) %>% 
                          rename(
                            H_Head_mujer        = mujer,
                            H_Head_EducLevel    = EducLevel,
                            H_Head_ocupado      = ocupado,
                            H_Head_trabaja      = trabaja,
                            H_Head_estudiante   = estudiante,
                            H_Head_pensionado   = pensionado
                          ) %>% 
                          left_join(personas_nivel_hogar(test_personas), by = "id")

## 3.3 Preprocesamiento base hogares


# Creación variables

pre_process_hogares <- function(data) {
  data <- data %>%
    mutate(
      arrienda       = ifelse(P5090 == 3, 1, 0),
      propia_pagada  = ifelse(P5090 == 1, 1, 0),
      propia_paga    = ifelse(P5090 == 2, 1, 0),
      usufructo      = ifelse(P5090 == 4, 1, 0),
      sin_titulo     = ifelse(P5090 == 5, 1, 0),
      otra_tenencia  = ifelse(P5090 == 6, 1, 0),
      personas_cuarto_total  = ifelse(P5000 > 0, Nper / P5000, NA),
      arriendo_estimado_pc   = ifelse(Nper > 0, P5130 / Nper, NA),
      paga_arriendo          = ifelse(!is.na(P5140) & P5140 > 0, 1, 0),
      cuota_amortiza         = ifelse(!is.na(P5100) & P5100 > 0, 1, 0)
    )
  
  # Variables que siempre deben estar
  base_vars <- c(
    "id", "Clase", "Dominio", "Depto", "Nper", "Npersug", 
    "arrienda", "propia_pagada", "propia_paga", "usufructo", 
    "sin_titulo", "otra_tenencia", "personas_cuarto_total", 
    "arriendo_estimado_pc", "paga_arriendo", "cuota_amortiza",
    "Li", "Lp", "Fex_c", "Fex_dpto"
  )
  
  # Variables opcionales (solo si existen)
  optional_vars <- c(
    "Ingtotug", "Ingtotugarr", "Ingpcug", 
    "Pobre", "Indigente", "Npobres", "Nindigentes"
  )
  
  # Verifica qué variables existen en el data
  available_optional_vars <- optional_vars[optional_vars %in% names(data)]
  
  # Selecciona las variables disponibles
  data <- data %>%
    select(all_of(c(base_vars, available_optional_vars)))
  
  return(data)
}

train_hogares <- pre_process_hogares(train_hogares) 

test_hogares <- pre_process_hogares(test_hogares) 

#4. Unión de bases

train <- train_hogares %>% 
        left_join(train_personas_hogar, by = "id") %>% 
        select(-id) #no longer need id

test <- test_hogares %>% 
  left_join(test_personas_hogar, by = "id")

# 4.1. Tratamiento de missings values

# Variable arriendo pc

train %>% 
  count(paga_arriendo, sort = TRUE)

train %>% 
  count(arriendo_estimado_pc, sort = TRUE)

train <- train %>%
  mutate(
    arriendo_estimado_pc = if_else(
      is.na(arriendo_estimado_pc),
      mean(arriendo_estimado_pc, na.rm = TRUE),
      arriendo_estimado_pc
    )
  )

test<- test %>%
  mutate(
    arriendo_estimado_pc = if_else(
      is.na(arriendo_estimado_pc),
      mean(arriendo_estimado_pc, na.rm = TRUE),
      arriendo_estimado_pc
    )
  )

train <- train %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

test <- test %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))


#5. Alistamiento 

train_deleted <- train %>% select(Ingtotug, Ingtotugarr, Indigente, Npobres, Nindigentes, Ingpcug)

train <- train %>% 
  select(-Ingtotug, -Ingtotugarr, -Indigente, -Npobres, -Nindigentes, -Ingpcug) # Ingcup puede servirme para predecir

# Conversión de las variables categórica a factores

train <- train %>% 
            mutate(
              # Variable objetivo (binaria)
              Pobre = factor(Pobre, levels = c(0,1), labels = c("No", "Sí")),
              
              # Factores categóricos
              Dominio = factor(Dominio),
              Clase = factor(Clase, levels = c(1, 2), labels = c("Cabecera", "Centro poblado / Rural disperso")),
              
              # Tenencia de vivienda y condiciones del hogar (dummies binarios)
              arrienda        = factor(arrienda, levels = c(0, 1), labels = c("No", "Sí")),
              propia_pagada   = factor(propia_pagada, levels = c(0, 1), labels = c("No", "Sí")),
              propia_paga     = factor(propia_paga, levels = c(0, 1), labels = c("No", "Sí")),
              usufructo       = factor(usufructo, levels = c(0, 1), labels = c("No", "Sí")),
              sin_titulo      = factor(sin_titulo, levels = c(0, 1), labels = c("No", "Sí")),
              otra_tenencia   = factor(otra_tenencia, levels = c(0, 1), labels = c("No", "Sí")),
              paga_arriendo   = factor(paga_arriendo, levels = c(0, 1), labels = c("No", "Sí")),
              cuota_amortiza  = factor(cuota_amortiza, levels = c(0, 1), labels = c("No", "Sí")),
              
              # Variables de composición del hogar
              H_Head_mujer      = factor(H_Head_mujer, levels = c(0, 1), labels = c("Hombre", "Mujer")),
              H_Head_ocupado    = factor(H_Head_ocupado, levels = c(0, 1), labels = c("No", "Sí")),
              H_Head_trabaja    = factor(H_Head_trabaja, levels = c(0, 1), labels = c("No", "Sí")),
              H_Head_estudiante = factor(H_Head_estudiante, levels = c(0, 1), labels = c("No", "Sí")),
              H_Head_pensionado = factor(H_Head_pensionado, levels = c(0, 1), labels = c("No", "Sí")),
              
              # Niveles educativos del jefe del hogar
              H_Head_EducLevel  = factor(H_Head_EducLevel, levels = 0:6,
                                         labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
              max_educ_level    = factor(max_educ_level, levels = 0:6,
                                         labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
          )

test <- test %>%
          mutate(
        
          # Factores categóricos
          Dominio = factor(Dominio),
          Clase = factor(Clase, levels = c(1, 2), labels = c("Cabecera", "Centro poblado / Rural disperso")),
          
          # Tenencia de vivienda y condiciones del hogar (dummies binarios)
          arrienda        = factor(arrienda, levels = c(0, 1), labels = c("No", "Sí")),
          propia_pagada   = factor(propia_pagada, levels = c(0, 1), labels = c("No", "Sí")),
          propia_paga     = factor(propia_paga, levels = c(0, 1), labels = c("No", "Sí")),
          usufructo       = factor(usufructo, levels = c(0, 1), labels = c("No", "Sí")),
          sin_titulo      = factor(sin_titulo, levels = c(0, 1), labels = c("No", "Sí")),
          otra_tenencia   = factor(otra_tenencia, levels = c(0, 1), labels = c("No", "Sí")),
          paga_arriendo   = factor(paga_arriendo, levels = c(0, 1), labels = c("No", "Sí")),
          cuota_amortiza  = factor(cuota_amortiza, levels = c(0, 1), labels = c("No", "Sí")),
          
          # Variables de composición del hogar
          H_Head_mujer      = factor(H_Head_mujer, levels = c(0, 1), labels = c("Hombre", "Mujer")),
          H_Head_ocupado    = factor(H_Head_ocupado, levels = c(0, 1), labels = c("No", "Sí")),
          H_Head_trabaja    = factor(H_Head_trabaja, levels = c(0, 1), labels = c("No", "Sí")),
          H_Head_estudiante = factor(H_Head_estudiante, levels = c(0, 1), labels = c("No", "Sí")),
          H_Head_pensionado = factor(H_Head_pensionado, levels = c(0, 1), labels = c("No", "Sí")),
          
          # Niveles educativos del jefe del hogar
          H_Head_EducLevel  = factor(H_Head_EducLevel, levels = 0:6,
                                     labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
          max_educ_level    = factor(max_educ_level, levels = 0:6,
                                     labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
        )




#6. Saving the data

write.csv(train, "stores/train.csv", row.names = FALSE)
write.csv(test, "stores/test.csv", row.names = FALSE)
write.csv(train_deleted, "stores/train_deleted.csv", row.names = FALSE )








