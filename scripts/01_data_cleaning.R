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

table(test_personas$P6430)

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
    regSalud = ifelse(P6090 == 9 | is.na(P6090), 0, P6090), # Afiliación salud (NA o 9 = 0)
    otrosingre= ifelse(P7505 ==2 | is.na(P7505),0, P7505),
    Investments = ifelse(P7510s5 == 2 | is.na(P7510s5),0, P7510s5),
    Typejob = ifelse(is.na(P6430),0, P6430),
    firmsize = ifelse(is.na(P6870),0, P6870)
  ) %>%
    select(id, Orden, mujer, H_Head, menor, EducLevel, ocupado, regSalud,otrosingre,Investments,Typejob,firmsize)
  
  # Agregaciones a nivel de hogar
  data_nivelHogar <- data %>% 
    group_by(id) %>% 
    summarize(
      nmujeres        = sum(mujer, na.rm = TRUE),
      nmenores        = sum(menor, na.rm = TRUE),
      maxEducLevel    = max(EducLevel, na.rm = TRUE),
      nocupados       = sum(ocupado, na.rm = TRUE),
      PropSalud       = mean(regSalud > 0, na.rm = TRUE),
      
      # para binarias: proporción y/o conteo
      propOtrosIngre  = mean(otrosingre, na.rm = TRUE),
      cntOtrosIngre   = sum(otrosingre, na.rm = TRUE),
      propInvestments = mean(Investments, na.rm = TRUE),
      cntInvestments  = sum(Investments, na.rm = TRUE),
      
      # para categóricas: moda
      Typejob_mode    = as.integer(
        names(
          sort(table(Typejob), decreasing = TRUE)
        )[1]
      ),
      firmsize_mode   = as.integer(
        names(
          sort(table(firmsize), decreasing = TRUE)
        )[1]
      )
    )
  
  
  # Información del jefe de hogar
  data_cabezaHogar <- data %>%
    filter(H_Head == 1) %>% 
    select(id, mujer, EducLevel, ocupado,Orden,otrosingre,Investments,Typejob,firmsize) %>% 
    rename(
      H_Head_mujer = mujer,
      H_Head_Educ_level = EducLevel,
      H_Head_ocupado = ocupado,
      H_Head_otrosingre  = otrosingre,
      H_Head_Investments = Investments,
      H_Head_Typejob     = Typejob,
      H_Head_firmsize    = firmsize
    )
  
  # Unir y retornar: UNA FILA POR HOGAR
  data_joined <- data_nivelHogar %>%
    left_join(data_cabezaHogar, by = "id")
  
  return(data_joined)
}

train_personas <- pre_process_personas(train_personas)
test_personas <-  pre_process_personas(test_personas)

pre_process_hogar<-  function(data_hogares, data_personas, is_train) {
  data_hogares <- data_hogares %>%
    mutate(arrienda = ifelse(P5090 == 3, 1, 0), #Arriendo
           viviendaPropia = ifelse(P5090 == 1, 1, 0), #Vivienda propia totalmente pagada
           dueno = ifelse(P5090==1 |P5090==2 ,1,0), # es dueño o no
           invasion = ifelse(P5090==5 ,1,0), #Posesion sin titulo
           numCuartos = P5000,
           Dormitorios = P5010,
           Valarriendo = ifelse(is.na(P5140),0,P5140) ## valor de 0 a los NA 
           ) ## Numero de cuartos en la casa 
  # Conditionally include Pobre only for train data
  if (is_train) {
    data_hogares <- data_hogares %>% dplyr::select(id, Dominio, arrienda, viviendaPropia, dueno, invasion, numCuartos, Nper, Npersug,Dormitorios,Valarriendo,Pobre)
  } else {
    data_hogares <- data_hogares %>% dplyr::select(id, Dominio, arrienda, viviendaPropia, dueno, invasion, numCuartos, Nper,Npersug,Dormitorios,Valarriendo)
  }
  
  data_joined <- data_hogares %>% 
    left_join(data_personas, by = "id")
  print(colnames(data_joined))
  
  # Remove 'id' only for train data
  if (is_train) {
    data_joined <- data_joined %>% dplyr::select(-id)
  }
  
  #Convertir a factores
  data_joined <- data_joined %>% 
    mutate(Dominio=factor(Dominio),
           arrienda=factor(arrienda, levels=c(0,1), labels=c("No","Yes")),
           viviendaPropia=factor(viviendaPropia, levels=c(0,1), labels=c("No","Yes")),
           dueno=factor(dueno, levels=c(0,1), labels=c("No","Yes")), 
           invasion=factor(invasion, levels=c(0,1), labels=c("No","Yes")),
           H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
           maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
           )
  if (is_train) {
    data_joined <- data_joined %>% 
      mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")))
  } 
  return(data_joined)
}

train <- pre_process_hogar(train_hogares, train_personas, is_train = TRUE)
test <- pre_process_hogar(test_hogares, test_personas, is_train = FALSE)


#### Data Vizualization  

hist(test_personas$P7510s5,
     main = "Histograma de ahorros",
     xlab = "CDTS",
     col = "lightblue",
     border = "white")

## Creación de nuevas variables 


niveles_educ <- c("Ns" = 0,
                  "Ninguno" = 0,
                  "Preescolar"= 1, 
                  "Primaria" = 2, 
                  "Secundaria" = 3, 
                  "Media" = 4,
                  "Universitaria"=5)

train <- train %>%
  mutate(
    DensHabit = Nper/Dormitorios,
    Ratiocuartos = Nper/numCuartos,
    H_Head_Educ_level_num = niveles_educ[H_Head_Educ_level],
    EducMujer = H_Head_Educ_level_num * H_Head_mujer,
    Educ_ocu = H_Head_ocupado * H_Head_Educ_level_num
  )

test <- test %>%
  mutate(
    DensHabit = Nper/Dormitorios,
    Ratiocuartos = Nper/numCuartos,
    H_Head_Educ_level_num = niveles_educ[H_Head_Educ_level],
    EducMujer = H_Head_Educ_level_num * H_Head_mujer,
    Educ_ocu = H_Head_ocupado * H_Head_Educ_level_num
  )

## save data 
saveRDS(train, "train.rds")
saveRDS(test, "test.rds")



