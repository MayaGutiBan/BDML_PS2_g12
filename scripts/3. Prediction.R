#------------------------------------------------------------------------------#
#                                                                              #
#                      Problem set 2 - Predicting poverty                      #
#                                 Prediction                                   #
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

#3. Predicción 

# Elastic net
p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(98462)
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

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)

lambda_str <- gsub("\\.", "_", as.character(round(model1$bestTune$lambda, 4)))
alpha_str  <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name <- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv")

write.csv(predictSample, file = file.path("stores", name), row.names = FALSE)

#3.2. Gradient boosting

p_load("caret")
p_load('gbm')

grid_gbm<-expand.grid(n.trees=c(200,250,300),
                      interaction.depth=c(4,6),
                      shrinkage=c(0.001,0.01),
                      n.minobsinnode = c(10,30))


grid_gbm

fitControl<-trainControl(method ="cv",
                         number=5)


set.seed(3759)
gbm_tree <- train( Pobre ~ H_Head_mujer + H_Head_trabaja + prom_anios_educ + n_ocupados ,
                   data=train,
                   method = "gbm", 
                   trControl = fitControl,
                   tuneGrid=grid_gbm,
                   verbose = FALSE
)  

predictSample <- test   %>% 
  mutate(pobre_lab = predict(gbm_tree, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)

write.csv(predictSample, file = file.path("stores", "gradient_boosting"), row.names = FALSE)

