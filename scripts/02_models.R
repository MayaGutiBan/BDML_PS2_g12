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

### DATA VIZUALIZATION 


ggplot(train, aes(x = Pobre, fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +  
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +  
  labs(x = "", y = "Number of individuals") ## + "add title" 

## There are class imbalances, way less individuals are poor 


###### Model training -------

## Create a validation set 

set.seed(123)
train_index <- createDataPartition(train$Pobre, p = 0.8, list = FALSE)
train_set <- train[train_index, ]   # Used for training & cross-validation
val_set   <- train[-train_index, ]  

### BASIC Logit model 

## Model 1 with elastic net were we are trying to max accuracy 

## CV method : 

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = TRUE)

set.seed(098063)

EN_Acu <- train(Pobre~.,
                data=train_set,
                metric = "Accuracy",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
)

EN_Acu  

# Predictions on Validation Set
Predict_EN_Acu <- data.frame(
  obs = val_set$Pobre,                                    # Observed class labels
  predict(EN_Acu, newdata = val_set, type = "prob"),      # Predicted probabilities
  pred = predict(EN_Acu, newdata = val_set, type = "raw") # Predicted class labels
)

# Add Predictions to Validation Set
val_set <- val_set %>% 
  mutate(Pobre_hat_EN_Acu = predict(EN_Acu, newdata = val_set, type = "raw"))

# Compute Confusion Matrix
cm_EN_Acu <- confusionMatrix(
  data = val_set$Pobre_hat_EN_Acu, 
  reference = val_set$Pobre, 
  positive = "Yes",  # Specify the positive class (adjust as needed)
  mode = "everything"
)

# Store Performance Metrics
model1 <- data.frame(
  Model = "EN_Acu",
  Accuracy = cm_EN_Acu$overall["Accuracy"],
  Precision = cm_EN_Acu$byClass["Precision"],
  Recall = cm_EN_Acu$byClass["Recall"],
  F1_Score = cm_EN_Acu$byClass["F1"]
)


## Max other metrics, we aim to max the F-1 score 

p_load(Metrics)

fiveStats <- function(...)  c(prSummary(...))  

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

set.seed(098063)
EN_F <- train(Pobre~.,
                data=train,
                metric = "F1",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
) ## alpha 0.5 lambda = 0.004641589


coef_Enet=coef(EN_F$finalModel,  EN_F$bestTune$lambda)

Predict_EN_F <- data.frame(
  obs = val_set$Pobre,                                    # Observed class labels
  predict(EN_F, newdata = val_set, type = "prob"),      # Predicted probabilities
  pred = predict(EN_F, newdata = val_set, type = "raw") # Predicted class labels
)

# Add Predictions to Validation Set
val_set <- val_set %>% 
  mutate(Pobre_hat_EN_F = predict(EN_F, newdata = val_set, type = "raw"))

# Compute Confusion Matrix
cm_EN_F <- confusionMatrix(
  data = val_set$Pobre_hat_EN_F, 
  reference = val_set$Pobre, 
  positive = "Yes",  # Specify the positive class (adjust as needed)
  mode = "everything"
)

# Store Performance Metrics
model2 <- data.frame(
  Model = "EN_F",
  Accuracy = cm_EN_F$overall["Accuracy"],
  Precision = cm_EN_F$byClass["Precision"],
  Recall = cm_EN_F$byClass["Recall"],
  F1_Score = cm_EN_F$byClass["F1"]
)

model1
model2 ## model 2 improve recall,precision and F1-Score 

## Model were we change the threshold 

roc_EN_F <- roc(response = Predict_EN_F$obs, 
                predictor = Predict_EN_F$Yes, 
                levels = c("No", "Yes")) 

plot(roc_EN_F, col = "blue", lwd = 2, main = "ROC Curve for EN_F Model")

rrfThresh_en <- coords(roc_EN_F, x = "best", best.method = "closest.topleft")
best_threshold <- as.numeric(rfThresh_en$threshold)


# Print best threshold value
print(best_threshold)

# Apply new threshold to validation set


# Evaluando en el Test Set aplicando el nuevo umbral

val_set<- val_set%>% mutate(Pobre_hat_EN_F_c=predict(EN_F,newdata = val_set,
                                                             type = "prob")[,"Yes"])



# Clasificamos los Casos Usando el Nuevo Umbral
val_set<- val_set  %>% mutate(Pobre_hat_EN_F_cutoff=factor(ifelse(Pobre_hat_EN_F_c>= best_threshold ,1,0),
                                                                  levels = c(0,1), labels = c("No","Yes")))



### Matriz de Confusión
cm_EN_F_cutoff <- confusionMatrix(
  data = val_set$Pobre_hat_EN_F_cutoff, 
  reference = val_set$Pobre, 
  positive="Yes", 
  mode = "everything"
  )

model3 <- data.frame(
  Model = "EN_F_DCutoff",
  Accuracy = cm_EN_F_cutoff$overall["Accuracy"],
  Precision = cm_EN_F_cutoff$byClass["Precision"],
  Recall = cm_EN_F_cutoff$byClass["Recall"],
  F1_Score = cm_EN_F_cutoff$byClass["F1"]
)


### PROC 



prec_recall<-data.frame(coords(roc_EN_F, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
prec_recall$threshold[which.max(prec_recall$F1)]


## Clasificamos con el nuevo threshold  
val_set<- val_set  %>% mutate(Pobre_hat_EN_F_cutoff_PROC=factor(ifelse(Pobre_hat_EN_F_c>= prec_recall$threshold[which.max(prec_recall$F1)] ,1,0),
                                                           levels = c(0,1), labels = c("No","Yes")))
cm_EN_F_cutoff_PROC <- confusionMatrix(
  data = val_set$Pobre_hat_EN_F_cutoff_PROC, 
  reference = val_set$Pobre, 
  positive="Yes", 
  mode = "everything"
)

model4 <- data.frame(
  Model = "EN_F_DCutoff_PROC",
  Accuracy = cm_EN_F_cutoff_PROC$overall["Accuracy"],
  Precision = cm_EN_F_cutoff_PROC$byClass["Precision"],
  Recall = cm_EN_F_cutoff_PROC$byClass["Recall"],
  F1_Score = cm_EN_F_cutoff_PROC$byClass["F1"]
)

##### Resampling 


### TREES MODELS ----------

p_load(Metrics)

fiveStats <- function(...)  c(prSummary(...))  

fitControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions = T )

set.seed(123)
tree_lenght <- train(
  Pobre ~ ., ## Variables used 
  data=train,
  method = "rpart", 
  metric = "ROC",  # Optimize for F1 
  trControl = fitControl, 
  tuneGrid = expand.grid(cp = seq(0.001707763, 0.001707765, length.out = 200))
)


tree_lenght

p_load("rpart.plot")

prp(tree_lenght$finalModel, 
    under = TRUE, 
    branch.lty = 2, 
    yesno = 2, 
    faclen = 0, 
    varlen = 15, 
    tweak = 1.2, 
    clip.facs = TRUE, 
    box.palette = "Greens", 
    compress = FALSE, 
    ycompress = FALSE,
    node.fun = function(x, labs, digits, varlen) {
      clases <- ifelse(x$frame$yval == 1, "Pobre", "No Pobre")  # Adjust depending on encoding
      paste("Clase: ", clases)
    }
)

## predict 

Predict_Trees <- data.frame(
  obs = val_set$Pobre,                                    # Observed class labels
  predict(tree_lenght, newdata = val_set, type = "prob"),      # Predicted probabilities
  pred = predict(tree_lenght, newdata = val_set, type = "raw") # Predicted class labels
)

val_set <- val_set %>% 
  mutate(Predict_Trees = predict(tree_lenght, newdata = val_set, type = "raw"))

# Compute Confusion Matrix
cm_Trees<- confusionMatrix(
  data = val_set$Predict_Trees, 
  reference = val_set$Pobre, 
  positive = "Yes",  # Specify the positive class (adjust as needed)
  mode = "everything"
)

# Store Performance Metrics
model5 <- data.frame(
  Model = "TREES_AUC",
  Accuracy = cm_Trees$overall["Accuracy"],
  Precision = cm_Trees$byClass["Precision"],
  Recall = cm_Trees$byClass["Recall"],
  F1_Score = cm_Trees$byClass["F1"]
)


###kaggle -------- 

## crate the prediccitions 

predictSample <- test   %>% 
  mutate(pobre_lab = predict(EN_F, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample) 

# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "/.", "_", 
  as.character(round(EN_F$bestTune$lambda, 4)))
alpha_str <- gsub("/.", "_", as.character(EN_F$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)

getwd()

### model 3 search for the best threshold, using the ROC curve 

# 1. Predecir las probabilidades de pobreza (clase "Yes")
test <- test %>%
  mutate(pobre_prob = predict(EN_F, newdata = test, type = "prob")[, "Yes"])

# 2. Aplicar el umbral óptimo para clasificar pobreza (0 o 1)
test <- test %>%
  mutate(pobre = ifelse(pobre_prob >= best_threshold, 1, 0)) %>%
  select(id, pobre)  # Solo mantener 'id' y la predicción

# 3. Generar un nombre único para el archivo basado en los hiperparámetros
lambda_str <- gsub("\\.", "_", as.character(round(EN_F$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(EN_F$bestTune$alpha))
file_name <- paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv")

# 4. Guardar el archivo CSV en el formato requerido para Kaggle
write.csv(test, file_name, row.names = FALSE)

### model 4 the same EN model, but now search for the threshold that max the F-1 score using the PROC curve 

# 1. Predecir las probabilidades de pobreza (clase "Yes")
test <- test %>%
  mutate(pobre_prob = predict(EN_F, newdata = test, type = "prob")[, "Yes"])

# 2. Aplicar el umbral óptimo para clasificar pobreza (0 o 1)
test <- test %>%
  mutate(pobre = ifelse(pobre_prob >= prec_recall$threshold[which.max(prec_recall$F1)], 1, 0)) %>%
  select(id, pobre)  # Solo mantener 'id' y la predicción

# 3. Generar un nombre único para el archivo basado en los hiperparámetros
lambda_str <- gsub("\\.", "_", as.character(round(EN_F$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(EN_F$bestTune$alpha))
file_name <- paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv")

# 4. Guardar el archivo CSV en el formato requerido para Kaggle
write.csv(test, file_name, row.names = FALSE)


## trees 

predictSample <- test   %>% 
  mutate(pobre_lab = predict(EN_F, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample) 

# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "/.", "_", 
  as.character(round(EN_F$bestTune$lambda, 4)))
alpha_str <- gsub("/.", "_", as.character(EN_F$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)
