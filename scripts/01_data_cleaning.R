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


new_hogar_variable <- train_personas  %>% 
  group_by(id) %>%
  summarize(h_Inactivos=sum(Ina,na.rm=TRUE),
            h_Pet= sum(Pet, na.rm = TRUE)   ) %>%
  mutate(h_Inactivosp=h_Inactivos/h_Pet ) %>%
  ungroup()

pre_process_personas <- function(data) {
  data <- data %>% 
    mutate(
      mujer             = ifelse(!is.na(P6020), ifelse(P6020 == 2, 1, 0), NA),
      menor             = ifelse(!is.na(P6040), ifelse(P6040 <= 6, 1, 0), NA),
      mayor_60          = ifelse(!is.na(P6040), ifelse(P6040 >= 60, 1, 0), NA),
      H_Head            = ifelse(!is.na(P6050), ifelse(P6050 == 1, 1, 0), NA),
      EducLevel         = ifelse(!is.na(P6210), ifelse(P6210 == 9, 0, P6210), NA),
      anios_educ        = ifelse(!is.na(P6210s1) & !P6210s1 %in% c(98, 99), P6210s1, NA),
      trabaja           = ifelse(!is.na(P6240), ifelse(P6240 == 1, 1, 0), NA),
      busca_trab        = ifelse(!is.na(P6240), ifelse(P6240 == 2, 1, 0), NA),
      estudiante        = ifelse(!is.na(P6240), ifelse(P6240 == 3, 1, 0), NA),
      ingresos_adic     = ifelse(!is.na(P6545) & P6545 != 9, ifelse(P6545 == 1, 1, 0), 0),
      afiliado_salud    = ifelse(!is.na(P6090) & P6090 != 9, ifelse(P6090 == 1, 1, 0), 0),
      afiliado_pension  = ifelse(!is.na(P6920), ifelse(P6920 == 1, 1, 0), 0),
      pensionado        = ifelse(!is.na(P6920), ifelse(P6920 == 3, 1, 0), 0),
      ingreso_arriendo  = ifelse(P7495 == 1, 1, 0),
      ingreso_dividendos= ifelse(!is.na(P7510s5) & P7510s5 != 9, ifelse(P7510s5 == 1, 1, 0), 0),
      ayuda_fam_int     = ifelse(!is.na(P7510s1) & P7510s1 != 9, ifelse(P7510s1 == 1, 1, 0), 0),
      ayuda_fam_ext     = ifelse(!is.na(P7510s2) & P7510s2 != 9, ifelse(P7510s2 == 1, 1, 0), 0),
      subsidio_gobierno = ifelse(!is.na(P7510s3) & P7510s3 != 9, ifelse(P7510s3 == 1, 1, 0), 0),
      otro_subsidio     = ifelse(!is.na(P7510s7), ifelse(P7510s7 == 1, 1, 0), NA),
      ocupado           = ifelse(is.na(Oc), 0, Oc),
      puede_trabajar    = ifelse(is.na(Pet), 0, Pet),
      desocupado        = ifelse(!is.na(Des), Des, 0),
      ina               = ifelse(!is.na(Ina), Ina , 0),
      otrosingre= ifelse(P7505 ==2 | is.na(P7505),0, P7505),
      
      Typejob           = ifelse(is.na(P6430), 0, P6430),
      firmsize          = ifelse(is.na(P6870), 0, P6870),
      
      S1 = ifelse(is.na(P6585s1) | P6585s1 %in% c(2,9), 0, P6585s1),
      S2 = ifelse(is.na(P6585s2) | P6585s2 %in% c(2,9), 0, P6585s2),
      S3 = ifelse(is.na(P6585s3) | P6585s3 %in% c(2,9), 0, P6585s3),
      S4 = ifelse(is.na(P6585s4) | P6585s4 %in% c(2,9), 0, P6585s4),
      subsidios_persona = S1 + S2 + S3 + S4,
      
      Horas_trabaja     = ifelse(is.na(P6800), 0, P6800)
    ) %>%
    select(
      id, Orden, mujer, menor, mayor_60, H_Head, EducLevel, anios_educ,
      trabaja, busca_trab, estudiante, ingresos_adic,
      afiliado_salud, afiliado_pension, pensionado,
      ingreso_arriendo, ingreso_dividendos,
      ayuda_fam_int, ayuda_fam_ext, subsidio_gobierno, otro_subsidio,
      ocupado, puede_trabajar, desocupado, ina,
      Typejob, firmsize, S1, S2, S3, S4, subsidios_persona, Horas_trabaja
    )
  
  # Agregaciones por hogar
  data_nivelHogar <- data %>% 
    group_by(id) %>%
    summarise(
      n_personas           = n(),
      n_mujeres            = sum(mujer, na.rm = TRUE),
      n_menores            = sum(menor, na.rm = TRUE),
      n_mayores_60         = sum(mayor_60, na.rm = TRUE),
      max_educ_level       = max(EducLevel, na.rm = TRUE),
      prom_anios_educ      = mean(anios_educ, na.rm = TRUE),
      
      n_ocupados           = sum(ocupado, na.rm = TRUE),
      n_trabaja            = sum(trabaja, na.rm = TRUE),
      n_busca_trab         = sum(busca_trab, na.rm = TRUE),
      n_desocupados        = sum(desocupado, na.rm = TRUE),
      n_inactivos          = sum(ina, na.rm = TRUE),
      n_puede_trab         = sum(puede_trabajar, na.rm = TRUE),
      
      n_estudiantes        = sum(estudiante, na.rm = TRUE),
      HW_mean              = mean(Horas_trabaja > 0, na.rm = TRUE),
      
      TotalSubsidiosHogar  = sum(subsidios_persona, na.rm = TRUE),
      
      # Afiliaciones y pensiones
      n_afiliado_salud     = sum(afiliado_salud, na.rm = TRUE),
      n_afiliado_pension   = sum(afiliado_pension, na.rm = TRUE),
      n_pensionados        = sum(pensionado, na.rm = TRUE),
      
      # Ingresos adicionales y ayudas
      n_ingresos_adic      = sum(ingresos_adic, na.rm = TRUE),
      n_arriendo           = sum(ingreso_arriendo, na.rm = TRUE),
      n_dividendos         = sum(ingreso_dividendos, na.rm = TRUE),
      n_ayuda_int          = sum(ayuda_fam_int, na.rm = TRUE),
      n_ayuda_ext          = sum(ayuda_fam_ext, na.rm = TRUE),
      n_subsidio_gob       = sum(subsidio_gobierno, na.rm = TRUE),
      n_otro_subsidio      = sum(otro_subsidio, na.rm = TRUE),
      
      # Proporciones
      prop_afiliado_salud   = mean(afiliado_salud, na.rm = TRUE),
      prop_afiliado_pension = mean(afiliado_pension, na.rm = TRUE),
      prop_pensionado       = mean(pensionado, na.rm = TRUE),
      prop_ingresos_adic    = mean(ingresos_adic, na.rm = TRUE),
      prop_arriendo         = mean(ingreso_arriendo, na.rm = TRUE),
      prop_dividendos       = mean(ingreso_dividendos, na.rm = TRUE),
      prop_ayuda_int        = mean(ayuda_fam_int, na.rm = TRUE),
      prop_ayuda_ext        = mean(ayuda_fam_ext, na.rm = TRUE),
      prop_subsidio_gob     = mean(subsidio_gobierno, na.rm = TRUE),
      prop_otro_subsidio    = mean(otro_subsidio, na.rm = TRUE),
      
      # Modas
      Typejob_mode          = as.integer(names(sort(table(Typejob), decreasing = TRUE))[1]),
      firmsize_mode         = as.integer(names(sort(table(firmsize), decreasing = TRUE))[1])
    )
  
  # Datos del jefe de hogar
  data_cabezaHogar <- data %>%
    filter(H_Head == 1) %>% 
    select(id, mujer, EducLevel, ocupado,Orden,Typejob,firmsize,subsidios_persona,Horas_trabaja,estudiante,pensionado) %>% 
    rename(
      H_Head_mujer = mujer,
      H_Head_Educ_level = EducLevel,
      H_Head_ocupado = ocupado,
      H_Head_Typejob     = Typejob,
      H_Head_firmsize    = firmsize,
      H_Head_subsidio = subsidios_persona,
      H_Head_hw = Horas_trabaja,
      H_Head_estudiante   = estudiante,
      H_Head_pensionado   = pensionado
    )
  
  
  # Unión final
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
           Valarriendo = ifelse(is.na(P5140),0,P5140), ## valor de 0 a los NA
           arriendo_estimado_pc   = ifelse(Nper > 0, P5130 / Nper, 0),
           paga_arriendo          = ifelse(!is.na(P5140) & P5140 > 0, 1, 0),
           cuota_amortiza         = ifelse(!is.na(P5100) & P5100 > 0, 1, 0)
           ) ## Numero de cuartos en la casa 
  # Conditionally include Pobre only for train data
  if (is_train) {
    data_hogares <- data_hogares %>% dplyr::select(id, Dominio, arrienda, viviendaPropia, dueno, invasion, numCuartos, Nper, Npersug,Dormitorios,Valarriendo,arriendo_estimado_pc, paga_arriendo,
                                                   cuota_amortiza, Pobre)
  } else {
    data_hogares <- data_hogares %>% dplyr::select(id, Dominio, arrienda, viviendaPropia, dueno, invasion, numCuartos, Nper,Npersug,Dormitorios,Valarriendo, arriendo_estimado_pc, paga_arriendo,
                                                   cuota_amortiza )          
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
           max_educ_level=factor(max_educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
           )
  if (is_train) {
    data_joined <- data_joined %>% 
      mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")))
  } 
  return(data_joined)
}

train <- pre_process_hogar(train_hogares, train_personas, is_train = TRUE)
test <- pre_process_hogar(test_hogares, test_personas, is_train = FALSE)




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
    Educ_ocu = H_Head_ocupado * H_Head_Educ_level_num,
  )

test <- test %>%
  mutate(
    DensHabit = Nper/Dormitorios,
    Ratiocuartos = Nper/numCuartos,
    H_Head_Educ_level_num = niveles_educ[H_Head_Educ_level],
    EducMujer = H_Head_Educ_level_num * H_Head_mujer,
    Educ_ocu = H_Head_ocupado * H_Head_Educ_level_num,

  )

#### tratamiento de los missing 

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


## Verificar NAs 
colSums(is.na(train))


saveRDS(train, "train.rds")
saveRDS(test, "test.rds")


