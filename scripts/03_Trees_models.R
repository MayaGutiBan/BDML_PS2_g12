if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # creating predictive models
       Metrics,
       dplyr,
       pROC,
       rpart,
       rpart.plot
)

setwd("/Users/carlosmanjarres/Desktop/Big_data")

p_load("ranger")
p_load("caret") ## to adjust the hiperparameters 

train <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/train.rds")


### Data partition to validate F1 score 
getwd()
set.seed(123)
train_index <- createDataPartition(train$Pobre, p = 0.8, list = FALSE)
train_set <- train[train_index, ]   # Used for training & cross-validation
val_set   <- train[-train_index, ]  

## store data set to improve efficiency 

saveRDS(val_set,file = "val_set.RDS" )
saveRDS(train_set, file = "train_set.RDS")

###
train_set <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/train_set.RDS")


### 1. RANDOM FOREST ---------------  

colnames(train_set)

rf <- ranger::ranger(
   Pobre ~ .,  
  data = train_set,
  num.trees = 1000,
  mtry = 6,
  min.node.size = 1,
  importance = "impurity",
  probability = TRUE
)

colnames(train_set)

# Obtener predicciones de probabilidades y clases
pred_rf <- predict(rf, data = val_set)

# Convertir probabilidades a clase predicha (asumiendo "Yes" como clase positiva)
# Suponiendo que Pobre tiene valores "Yes" y "No"
pred_class <- ifelse(pred_rf$predictions[,"Yes"] >= 0.5, "Yes", "No")

# Agregar predicción al validation set
val_set <- val_set %>%
  mutate(Pobre_rf = factor(pred_class, levels = levels(Pobre)))  # Asegura que tenga mismos niveles que la original

# Matriz de confusión
cm_RF <- confusionMatrix(
  data = val_set$Pobre_rf, 
  reference = val_set$Pobre, 
  positive = "Yes",
  mode = "everything"
)

# Guardar métricas de desempeño
model10 <- data.frame(
  Model = "RF",
  Accuracy = cm_RF$overall["Accuracy"],
  Precision = cm_RF$byClass["Precision"],
  Recall = cm_RF$byClass["Recall"],
  F1_Score = cm_RF$byClass["F1"]
)

### Change the threshold 

Predict_rf <- data.frame(
  obs = val_set$Pobre, 
  Yes = pred_rf$predictions[,"Yes"]
)

roc_rf <- roc(
  response = Predict_rf$obs,
  predictor = Predict_rf$Yes,
  levels = c("No", "Yes")
)

# Plot ROC curve
plot(roc_rf, col = "orange", lwd = 2, main = "ROC Curve - Random Forest")


rfThresh_rf <- coords(roc_rf, x = "best", best.method = "closest.topleft")
best_threshold_rf <- as.numeric(rfThresh_rf["threshold"])

# Print best threshold
print(best_threshold_rf)

### model with diferent threshold 

pred_class_cutoff <- ifelse(pred_rf$predictions[,"Yes"] >= best_threshold_rf , "Yes", "No")

# Agregar predicción al validation set
val_set <- val_set %>%
  mutate(Pobre_rf_cutoff = factor(pred_class_cutoff, levels = levels(Pobre)))  # Asegura que tenga mismos niveles que la original

# Matriz de confusión
cm_RF_cutoff<- confusionMatrix(
  data = val_set$Pobre_rf_cutoff, 
  reference = val_set$Pobre, 
  positive = "Yes",
  mode = "everything"
)

# Guardar métricas de desempeño
model10 <- data.frame(
  Model = "RF cutoff",
  Accuracy = cm_RF_cutoff$overall["Accuracy"],
  Precision = cm_RF_cutoff$byClass["Precision"],
  Recall = cm_RF_cutoff$byClass["Recall"],
  F1_Score = cm_RF_cutoff$byClass["F1"]
)


### 2. with CV opt the Hyper-parameters -------

fiveStats <- function(...) c(prSummary(...))

fitControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  savePredictions = "final"
)

RF_Grid <- train(
  Pobre ~ ., 
  data = train_set,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = 4,
    splitrule = "gini",
    min.node.size = c(3, 5,8)
  ),
  metric = "F", 
  importance = "impurity"
) #### mtry = 4 splitrule = gini and min.node.size = 5



##### RF GRID PREDICTIONS -------

val_set <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/val_set.RDS")

pred_prob <- predict(RF_Grid, newdata = val_set, type = "prob")

umbral <- 0.5
pred_clasificada <- ifelse(pred_prob$Yes > umbral, "Yes", "No")
pred_clasificada <- factor(pred_clasificada, levels = c("No", "Yes"))

val_set <- val_set %>%
  mutate(Pobre_rf_GRID = factor(pred_clasificada, levels = levels(Pobre)))

cm_RF_GRID <- confusionMatrix(
  data = val_set$Pobre_rf, 
  reference = val_set$Pobre, 
  positive = "Yes",
  mode = "everything"
)

model14 <- data.frame(
  Model = "RF Opti GRID",
  Accuracy = cm_RF_GRID$overall["Accuracy"],
  Precision = cm_RF_GRID$byClass["Precision"],
  Recall = cm_RF_GRID$byClass["Recall"],
  F1_Score = cm_RF_GRID$byClass["F1"]
)

## Improve the threshold 

## Construct a data frame of the predictions 

Pobre_rf_GRID <- data.frame(
  obs = val_set$Pobre,                                    # Observed class labels
  predict(RF_Grid, newdata = val_set, type = "prob"),      # Predicted probabilities
  pred = predict(RF_Grid, newdata = val_set, type = "raw") # Predicted class labels
)


## construct the ROC for RF_GRID model 
roc_RF_GRID <- roc(response = Pobre_rf_GRID$obs, 
                predictor = Pobre_rf_GRID$Yes, 
                levels = c("No", "Yes")) 

plot(roc_RF_GRID, col = "green", lwd = 2, main = "ROC Curve for RF Model with hyperparameter optimized")

## Construct PROC
prec_recall<-data.frame(coords(roc_RF_GRID, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
prec_recall$threshold[which.max(prec_recall$F1)]

pobre_hat <- Pobre_rf_GRID$Yes

val_set<- val_set  %>% mutate(Pobre_hat_RF_cutoff_PROC=factor(ifelse(pobre_hat >= prec_recall$threshold[which.max(prec_recall$F1)] ,1,0),
                                                                levels = c(0,1), labels = c("No","Yes")))


cm_RF_cutoff_PROC <- confusionMatrix(
  data = val_set$Pobre_hat_RF_cutoff_PROC, 
  reference = val_set$Pobre, 
  positive="Yes", 
  mode = "everything"
)

model_RF <- data.frame(
  Model = "RF_GRID_DCutoff_PROC",
  Accuracy = cm_RF_cutoff_PROC$overall["Accuracy"],
  Precision = cm_RF_cutoff_PROC$byClass["Precision"],
  Recall = cm_RF_cutoff_PROC$byClass["Recall"],
  F1_Score = cm_RF_cutoff_PROC$byClass["F1"]
)

resultados <- rbind(model_RF, model14)

### SUBIR MODELO A KAGGLE :) mejor predicción 

test <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/test.rds")
## Kaggle 

# 1. Predecir las probabilidades de pobreza (clase "Yes")
test <- test %>%
  mutate(pobre_prob = predict(RF_Grid, newdata = test, type = "prob")[, "Yes"])

# 2. Aplicar el umbral óptimo para clasificar pobreza (0 o 1)
test <- test %>%
  mutate(pobre = ifelse(pobre_prob >= prec_recall$threshold[which.max(prec_recall$F1)], 1, 0)) %>%
  select(id, pobre)  # Solo mantener 'id' y la predicción

# 3. Generar un nombre único para el archivo (puedes incluir mtry o algo relevante)
mtry_str <- as.character(RF_Grid$bestTune$mtry)
file_name <- paste0("RF_mtry_", mtry_str, ".csv")

# 4. Guardar el archivo CSV en el formato requerido para Kaggle
write.csv(test, file_name, row.names = FALSE)


## 3. Baging :) ------


Baging <- ranger(
  Pobre ~ .,  
  data = train_set,
  num.trees = 500,  
  mtry = 65,
  min.node.size = 4,
  importance = "impurity",
  probability = TRUE
)


# Obtener predicciones de probabilidades y clases
pred_bagging <- predict(Baging, data = val_set)

# Convertir probabilidades a clase predicha (asumiendo "Yes" como clase positiva)
# Suponiendo que Pobre tiene valores "Yes" y "No"
pred_class <- ifelse(pred_bagging$predictions[,"Yes"] >= 0.5, "Yes", "No")

# Agregar predicción al validation set
val_set <- val_set %>%
  mutate(Pobre_bagging = factor(pred_class, levels = levels(Pobre)))  # Asegura que tenga mismos niveles que la original

# Matriz de confusión
cm_Bagging <- confusionMatrix(
  data = val_set$Pobre_bagging, 
  reference = val_set$Pobre, 
  positive = "Yes",
  mode = "everything"
)

# Guardar métricas de desempeño

model_bagingg<- data.frame(
  Model = "Bagging",
  Accuracy = cm_Bagging$overall["Accuracy"],
  Precision = cm_Bagging$byClass["Precision"],
  Recall = cm_Bagging$byClass["Recall"],
  F1_Score = cm_Bagging$byClass["F1"]
)

### guardar performance modelos 

resultados <- rbind(model_bagingg,resultados)





### 4. Boosting =)  ---------


## definir la gramilla de entrenamiento 

train_set <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/train_set.RDS")

p_load('xgboost')

# Definir el grid de hiperparámetros para XGBoost enfocado en clasificación
grid_xgb <- expand.grid(
  nrounds = c(250,500), ## 500 rounds 
  max_depth = 4, 
  eta = 0.01, 
  gamma =c(0, 1), ### gamma 0 
  min_child_weight = c(5, 10),  ### 5 
  colsample_bytree = c(0.33, 0.66), ## 0,66
  subsample = c(0.4)
)
fiveStats <- function(...) c(prSummary(...)) # Configurar el control de validación cruzada para clasificación
fitControl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,                    # Para calcular probabilidades de clase
  summaryFunction = fiveStats,    
  verboseIter = FALSE
)

set.seed(1011) # Entrenar el modelo XGBoost para clasificación
xgb_model <- train(
  Pobre ~ .,
  data = train_set,
  method = "xgbTree",
  trControl = fitControl,
  tuneGrid = grid_xgb,
  metric = "F",       ## buscamos max el F1 score 
  verbose = FALSE
)

# Mostrar resultados
print(xgb_model)

val_set <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/val_set.RDS")

pred_prob <- predict(xgb_model, newdata = val_set, type = "prob")

umbral <- 0.5
pred_clasificada <- ifelse(pred_prob$Yes > umbral, "Yes", "No")
pred_clasificada <- factor(pred_clasificada, levels = c("No", "Yes"))

val_set <- val_set %>%
  mutate(Pobre_xgb_GRID = factor(pred_clasificada, levels = levels(Pobre)))

cm_XGB_GRID <- confusionMatrix(
  data = val_set$Pobre_xgb_GRID, 
  reference = val_set$Pobre, 
  positive = "Yes",
  mode = "everything"
)

model14 <- data.frame(
  Model = "Boosting XGB",
  Accuracy = cm_XGB_GRID$overall["Accuracy"],
  Precision = cm_XGB_GRID$byClass["Precision"],
  Recall = cm_XGB_GRID$byClass["Recall"],
  F1_Score = cm_XGB_GRID$byClass["F1"]
)

resultados <- rbind(resultados,model14)

### improve 

Pobre_XGB_GRID <- data.frame(
  obs = val_set$Pobre,                                    # Observed class labels
  predict(xgb_model, newdata = val_set, type = "prob"),      # Predicted probabilities
  pred = predict(xgb_model, newdata = val_set, type = "raw") # Predicted class labels
)


## construct the ROC for RF_GRID model 
roc_XGB_GRID <- roc(response = Pobre_XGB_GRID$obs, 
                   predictor = Pobre_XGB_GRID$Yes, 
                   levels = c("No", "Yes")) 

plot(roc_RF_GRID, col = "green", lwd = 2, main = "ROC Curve for RF Model with hyperparameter optimized")

## Construct PROC
prec_recall<-data.frame(coords(roc_XGB_GRID, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
prec_recall$threshold[which.max(prec_recall$F1)]

pobre_hat <- Pobre_XGB_GRID$Yes

val_set<- val_set  %>% mutate(Pobre_hat_XGB_cutoff_PROC=factor(ifelse(pobre_hat >= prec_recall$threshold[which.max(prec_recall$F1)] ,1,0),
                                                              levels = c(0,1), labels = c("No","Yes")))


cm_XGB_cutoff_PROC <- confusionMatrix(
  data = val_set$Pobre_hat_XGB_cutoff_PROC, 
  reference = val_set$Pobre, 
  positive="Yes", 
  mode = "everything"
)

model_XGB <- data.frame(
  Model = "XGB GRID PROC",
  Accuracy = cm_XGB_cutoff_PROC$overall["Accuracy"],
  Precision = cm_XGB_cutoff_PROC$byClass["Precision"],
  Recall = cm_XGB_cutoff_PROC$byClass["Recall"],
  F1_Score = cm_XGB_cutoff_PROC$byClass["F1"]
)


resultados <- rbind(model_XGB,resultados )

write.csv(resultados, "resultados_modelos.csv", row.names = FALSE)
