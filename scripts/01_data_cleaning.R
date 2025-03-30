##########################################################
# Example Script
# Authors: Grupo 12
# Script description: 
##########################################################

# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo

#set working director
setwd("~/Documents/GitHub/BDML_PS2_g12")

# Load Packages -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # creating predictive models
       Metrics
)

set.seed(1011)  # Fijar semilla para reproducibilidad de resultados.

# Load data ---------------------------------------------------------------
train_hogares<-read.csv("~/Documents/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/train_hogares.csv")
train_personas<-read.csv("~/Documents/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/train_personas.csv")
test_hogares<-read.csv("~/Documents/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/test_hogares.csv")
test_personas<-read.csv("~/Documents/GitHub/BDML_PS2_g12/uniandes-bdml-202510-ps-2/test_personas.csv")

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
    H_Head = ifelse(P6050 == 1, 1, 0), # Household head
    menor = ifelse(P6040 <= 6, 1, 0), # Menores
    EducLevel = ifelse(P6210 == 9, 0, P6210), # Replace 9 with 0
    ocupado = ifelse(is.na(Oc), 0, 1),
    regSalud = ifelse(P6090 == 9 | is.na(P6090), 0, P6090) # Replace 9 and NA with 0
  ) %>%
    select(id, Orden,mujer,H_Head,menor,EducLevel,ocupado,regSalud)
  
  # Household-level aggregations
  data_nivelHogar <- data %>% 
    group_by(id) %>% 
    summarize(
      nmujeres = sum(mujer, na.rm = TRUE),
      nmenores = sum(menor, na.rm = TRUE),
      maxEducLevel = max(EducLevel, na.rm = TRUE),
      nocupados = sum(ocupado, na.rm = TRUE)
    )
  
  # Get household head info
  data_cabezaHogar <- data %>%
    filter(H_Head == 1) %>% 
    select(id, mujer, EducLevel, ocupado) %>% 
    rename(
      H_Head_mujer = mujer,
      H_Head_Educ_level = EducLevel,
      H_Head_ocupado = ocupado
    )
  
  # Merge back to the full dataset
  data_joined <- data %>%
    left_join(data_nivelHogar, by = "id") %>%
    left_join(data_cabezaHogar, by = "id")
  
  return(data_joined)
}


train_personas <- pre_process_personas(train_personas)
test_personas <-  pre_process_personas(test_personas)


pre_process_hogar<-  function(data_hogares, data_personas, is_train) {
  data <- data_hogares %>%
    mutate(arrienda = ifelse(P5090 == 3, 1, 0)) #Arriendo
    # Conditionally include Pobre only for train data
    if (is_train) {
      data <- data %>% select(id, Dominio, arrienda, Pobre)
    } else {
      data <- data %>% select(id, Dominio, arrienda)
    }
  
  data <- data %>% 
    left_join(data_personas, by = "id")
  
  # Remove 'id' only for train data
  if (is_train) {
    data <- data %>% select(-id)
  }
  
  #Convertir a factores
  data<- data %>% 
    mutate(Dominio=factor(Dominio),
           arrienda=factor(arrienda, levels=(0,1), labels=c("No","Yes")),
           H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
           maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
    )%>%
    select(Dominio, arrienda, Pobre, Orden, mujer, H_head, menor, ocupado, redSalud
  if (is_train) {
   data<- data %>% 
    mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")))
  } 
  return(data)
}


train <- pre_process_hogar(train_hogares, train_personas, is_train = TRUE)
test <- pre_process_hogar(test_hogares, test_personas, is_train = FALSE)


train_1<- train %>% 
  select(Pobre, Dominio, arrienda, Orden, ocupado, regSalud, nmujeres, nmenores, maxEducLevel, nocupados, H_Head_mujer, H_Head_Educ_level, H_Head_ocupado)

test_1<- test %>% 
  select(Dominio, arrienda, Orden, ocupado, regSalud, nmujeres, nmenores, maxEducLevel, nocupados, H_Head_mujer, H_Head_Educ_level, H_Head_ocupado)

colSums(is.na(train))

###### Model training
ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = T)

set.seed(098063)

model1 <- train(Pobre~.,
                data=train,
                metric = "Accuracy",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
)
model1

#Otras metricas

p_load(Metrics)

fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

#install.packages("MLmetrics")
#require(MLmetrics)

set.seed(098063)
model1 <- train(Pobre~.,
                data=train_1,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model1

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample) 

# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "/.", "_", 
  as.character(round(model1$bestTune$lambda, 4)))
alpha_str <- gsub("/.", "_", as.character(model1$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)