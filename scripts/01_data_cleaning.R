##########################################################
# Data processing script
# Authors: Grupo 12
# Script description: 
##########################################################

# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo

#set working director
setwd("~/Documents/Documents - Maya’s MacBook Air/GitHub/BDML_PS2_g12")

# Load Packages -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,
       tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # creating predictive models
       Metrics
)

set.seed(1011)  # Fijar semilla para reproducibilidad de resultados.

# Load data ---------------------------------------------------------------
train_hogares<-read.csv("~/Documents/Documents - Maya’s MacBook Air/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/train_hogares.csv")
train_personas<-read.csv("~/Documents/Documents - Maya’s MacBook Air/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/train_personas.csv")
test_hogares<-read.csv("~/Documents/Documents - Maya’s MacBook Air/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/test_hogares.csv")
test_personas<-read.csv("~/Documents/Documents - Maya’s MacBook Air/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/test_personas.csv")
sample<-read.csv("~/Documents/Documents - Maya’s MacBook Air/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/sample_submission.csv")

# Data pre-processing ------------------------------------------------------

summary(train_hogares)
pairs(train_hogares)

new_hogar_variable <- train_personas  %>% 
  group_by(id) %>%
  summarize(h_Inactivos=sum(Ina,na.rm=TRUE),
            h_Pet= sum(Pet, na.rm = TRUE)   ) %>%
  mutate(h_Inactivosp=h_Inactivos/h_Pet ) %>%
  ungroup()

pre_process_personas <- function(data) {
  data <- data %>% mutate(
    mujer = ifelse(P6020 == 2, 1, 0), 
    H_Head = ifelse(P6050 == 1, 1, 0), # Jefe de hogar
    menor = ifelse(P6040 <= 6, 1, 0),  # Menores de edad
    EducLevel = ifelse(P6210 == 9, 0, P6210), # Nivel educativo (9 reemplazado por 0)
    ocupado = ifelse(is.na(Oc), 0, 1), # Ocupación (asume NA = no ocupado)
    regSalud = ifelse(P6090 == 9 | is.na(P6090), 0, P6090) # Afiliación salud (NA o 9 = 0)
  ) %>%
    select(id, Orden, mujer, H_Head, menor, EducLevel, ocupado, regSalud)
  
  # Agregaciones a nivel de hogar
  data_nivelHogar <- data %>% 
    group_by(id) %>% 
    summarize(
      nmujeres = sum(mujer, na.rm = TRUE),
      nmenores = sum(menor, na.rm = TRUE),
      maxEducLevel = max(EducLevel, na.rm = TRUE),
      nocupados = sum(ocupado, na.rm = TRUE),
      PropSalud = mean(regSalud > 0, na.rm = TRUE)
    )
  
  # Información del jefe de hogar
  data_cabezaHogar <- data %>%
    filter(H_Head == 1) %>% 
    select(id, mujer, EducLevel, ocupado,Orden) %>% 
    rename(
      H_Head_mujer = mujer,
      H_Head_Educ_level = EducLevel,
      H_Head_ocupado = ocupado
    )
  
  # Unir y retornar: UNA FILA POR HOGAR
  data_joined <- data_nivelHogar %>%
    left_join(data_cabezaHogar, by = "id")
  
  return(data_joined)
}

train_personas <- pre_process_personas(train_personas)
test_personas <-  pre_process_personas(test_personas)


pre_process_personas <- function(data) {
  data <- data %>% mutate(
    mujer = ifelse(P6020 == 2, 1, 0), 
    H_Head = ifelse(P6050 == 1, 1, 0), # Jefe de hogar
    menor = ifelse(P6040 <= 6, 1, 0),  # Menores de edad
    EducLevel = ifelse(P6210 == 9, 0, P6210), # Nivel educativo (9 reemplazado por 0)
    ocupado = ifelse(is.na(Oc), 0, 1), # Ocupación (asume NA = no ocupado)
    regSalud = ifelse(P6090 == 9 | is.na(P6090), 0, P6090) # Afiliación salud (NA o 9 = 0)
  ) %>%
    select(id, Orden, mujer, H_Head, menor, EducLevel, ocupado, regSalud)

  # Agregaciones a nivel de hogar
  data_nivelHogar <- data %>% 
    group_by(id) %>% 
    summarize(
      nmujeres = sum(mujer, na.rm = TRUE),
      nmenores = sum(menor, na.rm = TRUE),
      maxEducLevel = max(EducLevel, na.rm = TRUE),
      nocupados = sum(ocupado, na.rm = TRUE)
    )

  # Información del jefe de hogar
  data_cabezaHogar <- data %>%
    filter(H_Head == 1) %>% 
    select(id, mujer, EducLevel, ocupado) %>% 
    rename(
      H_Head_mujer = mujer,
      H_Head_Educ_level = EducLevel,
      H_Head_ocupado = ocupado
    )

  # Unir y retornar: UNA FILA POR HOGAR
  data_joined <- data_nivelHogar %>%
    left_join(data_cabezaHogar, by = "id")

  return(data_joined)
} 

train <- pre_process_hogar(train_hogares, train_personas, is_train = TRUE)
test <- pre_process_hogar(test_hogares, test_personas, is_train = FALSE)

colnames(train)

train_1<- train %>% 
  select(Pobre, Dominio, arrienda, Orden, PropSalud, nmujeres, nmenores, maxEducLevel, nocupados, H_Head_mujer, H_Head_Educ_level, H_Head_ocupado, Nper)

test_1<- test %>% 
  select(Dominio, arrienda, Orden, PropSalud, nmujeres, nmenores, maxEducLevel, nocupados, H_Head_mujer, H_Head_Educ_level, H_Head_ocupado, Nper)

colSums(is.na(train))

#Save clean data
write_rds(train,"train.rds")
write_rds(test,"test.rds")
