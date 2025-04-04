##########################################################
# Model design script
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

# Load data
train <- read_rds("train.rds")
test <- read_rds("test.rds")

head(train, 5)
head(test, 5)

# Model training
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
                data=train,
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

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

ctrl_multiStats <- trainControl(
  method = "cv",  # Usamos validación cruzada
  number = 5,  # 5-fold cross-validation
  summaryFunction = multiStats,  # Usamos la función de evaluación personalizada
  classProbs = TRUE,  # Habilita el cálculo de probabilidades para cada clase
  verbose = FALSE,  # Evita mensajes innecesarios en la consola
  savePredictions = TRUE  # Guarda las predicciones para análisis posterior
)

model2 <- train(Pobre~.,
                data=train,
                metric = "Accuracy",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
)
model2

colSums(is.na(train))

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw"))    ## predicted class labels  
         %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample) 

# Matriz de Confusión
logit_allvar<-confusionMatrix(data = test$pobre_lab, 
                               reference = test$Pobre, 
                               positive="Pobre", 
                               mode = "prec_recall")
logit_allvar


# Guardo en un data frame
df_logit1 <- data.frame(
  Model = "Logit (all variables)",
  Accuracy = model1$overall["Accuracy"],
  Precision = model1$byClass["Precision"],
  Recall = model1$byClass["Recall"],
  F1_Score = model1$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit1)<-NULL
df_logit1


# Comparamos
metrics_df <- rbind(df_logit1, df_logit2)

metrics_df
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