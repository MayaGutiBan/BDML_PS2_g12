if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # creating predictive models
       Metrics,
       ggplot2,
       dplyr,
       pROC,
       rpart,
       rpart.plot,
       smotefamily
)

## load data 
train <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/train.rds")

set.seed(123)
train_index <- createDataPartition(train$Pobre, p = 0.8, list = FALSE)
train_set <- train[train_index, ]   # Used for training & cross-validation
val_set   <- train[-train_index, ]  


# Crea la matriz de predictores (sin intercepto)
x_train <- model.matrix(Pobre ~ Dominio + arrienda + viviendaPropia + dueno +
                          invasion + numCuartos + Nper + Npersug + Dormitorios + Valarriendo +
                          nmujeres + nmenores + maxEducLevel + nocupados + PropSalud + propOtrosIngre +
                          cntOtrosIngre + propInvestments + cntInvestments + Typejob_mode + firmsize_mode +
                          H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + Orden +
                          H_Head_otrosingre + H_Head_Investments + H_Head_Typejob + H_Head_firmsize,
                        data = train_set)
x_train <- x_train[,-1]

x_train <- as.data.frame(x_train)

train$Pobre <- as.factor(train$Pobre)
print(table(train$Pobre)) 


colnames(x_train)<-paste0("X",seq(1,dim(x_train)[2]))

# Ejecutar SMOTE
smote_data <- SMOTE(X = x_train, target = as.factor(train$Pobre), K = 5)

train_smote<- smote_data$data

table(smote$class)

## create the data partitions 

### 1. elastic net ------

fiveStats <- function(...)  c(prSummary(...))  

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)


EN_F_smote<- train(class~.,
                   data=train_smote,
                   metric = "F",
                   method = "glmnet",
                   trControl = ctrl,
                   family="binomial",
                   tuneGrid=expand.grid(
                     alpha = seq(0,1,by=.5),
                     lambda =10^seq(-1, -3, length = 20)
                   )
) 

colnames(test)

coef_Enet=coef(EN_F_smote$finalModel,  EN_F_smote$bestTune$lambda)

Predict_EN_F_smote <- data.frame(
  obs = Val_set_smote$class,                                    # Observed class labels
  predict(EN_F_smote, newdata = Val_set_smote, type = "prob"),      # Predicted probabilities
  pred = predict(EN_F_smote, newdata = Val_set_smote, type = "raw") # Predicted class labels
)

# Add Predictions to Validation Set
Val_set_smote <- Val_set_smote %>% 
  mutate(Pobre_hat_EN_F_smote = predict(EN_F_smote, newdata = Val_set_smote, type = "raw"))

data <- factor(Val_set_smote$Pobre_hat_EN_F_smote, levels = c("No", "Yes"))
reference <- factor(Val_set_smote$class, levels = c("No", "Yes"))

# Compute Confusion Matrix
cm_EN_F_smote <- confusionMatrix(
  data = data,
  reference = reference,
  positive = "Yes",
  mode = "everything"
)

# Store Performance Metrics
model_smote <- data.frame(
  Model = "EN_F_smote",
  Accuracy = cm_EN_F_smote$overall["Accuracy"],
  Precision = cm_EN_F_smote$byClass["Precision"],
  Recall = cm_EN_F_smote$byClass["Recall"],
  F1_Score = cm_EN_F_smote$byClass["F1"]
)

### improve threshold 

roc_EN_F_smote <- roc(
  response = Predict_EN_F_smote$obs,
  predictor = Predict_EN_F_smote$Yes,
  levels = c("No", "Yes"),
  direction = "<"  # Common default; make sure this fits your case
)

plot(roc_EN_F_smote, col = "blue", main = "ROC Curve - EN_F_smote")

prec_recall<-data.frame(coords(roc_EN_F_smote, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
prec_recall$threshold[which.max(prec_recall$F1)]



#### Val_set_smote 
Val_set_smote <- model.matrix(~ Dominio + arrienda + viviendaPropia + dueno +
                         invasion + numCuartos + Nper + Npersug + Dormitorios + Valarriendo +
                         nmujeres + nmenores + maxEducLevel + nocupados + PropSalud + propOtrosIngre +
                         cntOtrosIngre + propInvestments + cntInvestments + Typejob_mode + firmsize_mode +
                         H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + Orden +
                         H_Head_otrosingre + H_Head_Investments + H_Head_Typejob + H_Head_firmsize,
                       data = val_set)

Val_set_smote <- as.data.frame(Val_set_smote)

colnames(Val_set_smote) <- colnames(x_train)



### 
Predict_EN_F_smote_c <- data.frame(
  obs = Val_set_smote$class,                                 
  predict(EN_F_smote, newdata = Val_set_smote, type = "prob")[,"Yes"])

## store the prob
Val_set_smote <- Val_set_smote %>% 
  mutate(Predict_EN_F_smote_c = predict(EN_F_smote, newdata = Val_set_smote, type = "prob")[,"Yes"])

Val_set_smote<- Val_set_smote  %>% mutate(Pobre_hat_EN_F_SMOTE_cutoff_PROC=factor(ifelse(Predict_EN_F_smote_c>= prec_recall$threshold[which.max(prec_recall$F1)] ,1,0),
                                                                                  levels = c(0,1), labels = c("No","Yes")))

## 

data <- factor(Val_set_smote$Pobre_hat_EN_F_SMOTE_cutoff_PROC, levels = c("No", "Yes"))
reference <- factor(Val_set_smote$class, levels = c("No", "Yes"))

# Compute Confusion Matrix
cm_EN_F_smote_cutoff <- confusionMatrix(
  data = data,
  reference = reference,
  positive = "Yes",
  mode = "everything"
)

# Store Performance Metrics
model_smote_cutoff<- data.frame(
  Model = "EN_SMOTE_PROC",
  Accuracy = cm_EN_F_smote_cutoff$overall["Accuracy"],
  Precision = cm_EN_F_smote_cutoff$byClass["Precision"],
  Recall = cm_EN_F_smote_cutoff$byClass["Recall"],
  F1_Score = cm_EN_F_smote_cutoff$byClass["F1"]
)

### models performance 0.7625969 improved threshold 0.7768264

## Kaggle for SMOTE 

test <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/test.rds")
x_test <- model.matrix(~ Dominio + arrienda + viviendaPropia + dueno +
                         invasion + numCuartos + Nper + Npersug + Dormitorios + Valarriendo +
                         nmujeres + nmenores + maxEducLevel + nocupados + PropSalud + propOtrosIngre +
                         cntOtrosIngre + propInvestments + cntInvestments + Typejob_mode + firmsize_mode +
                         H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + Orden +
                         H_Head_otrosingre + H_Head_Investments + H_Head_Typejob + H_Head_firmsize,
                       data = test)


x_test <- as.data.frame(x_test)
colnames(x_test) <- paste0("X", 1:ncol(x_test))



test_preds <- data.frame(prob_pobre = predict(EN_F_smote, newdata = x_test, type = "prob")[, "Yes"])

test_preds <- test_preds %>%
  mutate(Pobre_hat_EN_F = factor(
    ifelse(prob_pobre >= prec_recall$threshold[which.max(prec_recall$F1)],"Yes","No"),
    levels = c("No", "Yes")
  ))

head(test_preds)

submission <- data.frame(
  id = test$id,
  Pobre = test_preds$Pobre_hat_EN_F
)

submission <-  submission%>% mutate(Pobre = ifelse(Pobre == "No",0,1)) 



name <- "Elastic_net_SMOTE_PROC.csv"
write.csv(submission,name, row.names = FALSE)

getwd()

colnames(x_test) <- colnames(x_train)

### 2. Random forest + SMOTE #######

name <- "SMOTE_TRAIN_DS.RDS"
saveRDS(train_smote,file= name)

train_smote <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/SMOTE_TRAIN_DS.RDS")

## model 

# Paso 1: Cargar librerías 
library(parallel)

library(doParallel)

# Paso 2: Detectar núcleos y crear clúster
num_cores <- detectCores() - 2  # Usa todos menos 1 para que no se congele el sistema
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

# Paso 3: Ahora corré tu train() normalment

fiveStats <- function(...) c(prSummary(...))

fitControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  savePredictions = "final"
)


RF_Grid_SMOTE <- train(
  class ~ ., 
  data = train_smote,
  method = "ranger",
  trControl = fitControl,
  tuneGrid = expand.grid(
    mtry = 4,
    splitrule = "gini",
    min.node.size = c(3, 5,8) ## 3 
  ),
  metric = "F", 
  importance = "impurity"
) #### mtry = 4 splitrule = gini and min.node.size = 5

# Paso 4: Cuando termine, apagá el clúster
stopCluster(cl)
registerDoSEQ()

### predictions 

Val_set_smote <- Val_set_smote %>% 
  mutate(Predict_RF_smote_c = predict(RF_Grid_SMOTE, newdata = Val_set_smote, type = "prob")[,"Yes"])

Val_set_smote <- Val_set_smote %>% 
  mutate(Pobre_hat_RF_SMOTE_c = factor(
    ifelse(Predict_RF_smote_c >= 0.5, 1, 0),
    levels = c(0, 1),
    labels = c("No", "Yes")
  ))

## 

data <- factor(Val_set_smote$Pobre_hat_RF_SMOTE_c, levels = c("No", "Yes"))
reference <- factor(Val_set_smote$class, levels = c("No", "Yes"))

# Compute Confusion Matrix
cm_RF_smote <- confusionMatrix(
  data = data,
  reference = reference,
  positive = "Yes",
  mode = "everything"
)

# Store Performance Metrics
model_RF_smote_cutoff<- data.frame(
  Model = "RF_SMOTE",
  Accuracy = cm_RF_smote$overall["Accuracy"],
  Precision = cm_RF_smote$byClass["Precision"],
  Recall = cm_RF_smote$byClass["Recall"],
  F1_Score = cm_RF_smote$byClass["F1"]
)


#### KAGLE 

test <- readRDS("/Users/carlosmanjarres/Desktop/Big_data/test.rds")
x_test <- model.matrix(~ Dominio + arrienda + viviendaPropia + dueno +
                         invasion + numCuartos + Nper + Npersug + Dormitorios + Valarriendo +
                         nmujeres + nmenores + maxEducLevel + nocupados + PropSalud + propOtrosIngre +
                         cntOtrosIngre + propInvestments + cntInvestments + Typejob_mode + firmsize_mode +
                         H_Head_mujer + H_Head_Educ_level + H_Head_ocupado + Orden +
                         H_Head_otrosingre + H_Head_Investments + H_Head_Typejob + H_Head_firmsize,
                       data = test)


x_test <- as.data.frame(x_test)
colnames(x_test) <- colnames(x_train)


test_preds <- data.frame(prob_pobre = predict(RF_Grid_SMOTE, newdata = x_test, type = "raw"))

##test_preds <- test_preds %>%
#  mutate(Pobre_hat_EN_F = factor(
#    ifelse(prob_pobre >= prec_recall$threshold[which.max(prec_recall$F1)],"Yes","No"),
#    levels = c("No", "Yes")
#  ))

head(test_preds)

submission <- data.frame(
  id = test$id,
  Pobre = test_preds$prob_pobre
)

submission <-  submission%>% mutate(Pobre = ifelse(Pobre == "No",0,1)) 



name <- "Random_forest_SMOTE.csv"
write.csv(submission,name, row.names = FALSE)
