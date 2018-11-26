library(dplyr)
library(tidyr)
library(ggplot2)
library(gbm)
library(ROCR)

train <- readRDS("/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/train.rds")
test <- readRDS("/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/test.rds")

set.seed(98765)
fit0 <- gbm(churn_target ~ 1 + 
              gender_factor +
              SeniorCitizen +
              partner_factor +
              dependents_factor +
              tenure +
              phoneService_factor +
              multipleLines_factor +
              internetService_factor +
              onlineSecurity_factor +
              onlineBackup_factor +
              deviceProtection_factor +
              techSupport_factor +
              streamingTV_factor +
              streamingMovies_factor +
              contract_factor +
              paperlessBilling_factor +
              paymentMethod_factor +
              feeChange_factor,
            data = train,
            distribution = "bernoulli",
            n.trees = 2500,
            interaction.depth = 4,
            shrinkage = 0.01,
            train.fraction = 0.8,
            bag.fraction = 0.6,
            verbose = TRUE)
best.iter.0 <- gbm.perf(object = fit0, method = "test"); best.iter.0
summary0 <- data.frame(summary.gbm(object = fit0,n.trees = best.iter.0), row.names = NULL)
train$pred0 <- predict.gbm(object = fit0,newdata = train,n.trees = best.iter.0,type = "response")
test$pred0 <- predict.gbm(object = fit0,newdata = test,n.trees = best.iter.0,type = "response")

getAUC <- function(pred, target){
  library(ROCR)
  pred <- prediction(pred, target)
  auc.tmp <- performance(pred,"auc"); 
  auc <- as.numeric(auc.tmp@y.values)
  return(auc)
}
describe(train$churn_target)
describe(test$churn_target)
getAUC(train$pred0, train$churn_target)
getAUC(test$pred0, test$churn_target)


pdp <- function(mod, varName, numTrees, numPoints, is.factor){
  library(gbm)
  # library(dplyr)
  library(ggplot2)
  
  dat <- plot.gbm(x = mod, i.var = varName, n.trees = numTrees, continuous.resolution = numPoints, type = "response",return.grid = TRUE)
  
  if(is.factor){
    p <- ggplot(dat, aes_string(varName, "y")) + 
      geom_bar(stat = "identity") +
      labs(title = "Partial Dependence Plot", subtitle = varName, y = "P(attrition)") +
      theme(text = element_text(size=20))
  } else {
    p <- ggplot(dat, aes_string(varName, "y")) + 
      geom_point() + 
      geom_line() +
      labs(title = "Partial Dependence Plot", subtitle = varName, y = "P(attrition)") +
      theme(text = element_text(size=20))
  }
  return(p)
}
pdp(fit0, "contract_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "tenure", best.iter.0, 2000, FALSE)  
pdp(fit0, "internetService_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "onlineSecurity_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "paymentMethod_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "techSupport_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "feeChange_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "streamingTV_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "paperlessBilling_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "multipleLines_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "streamingTV_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "SeniorCitizen", best.iter.0, 2000, FALSE)
pdp(fit0, "dependents_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "partner_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "deviceProtection_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "onlineBackup_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "gender_factor", best.iter.0, 2000, TRUE)  
pdp(fit0, "phoneService_factor", best.iter.0, 2000, TRUE)  

## Simpler version. Remove drivers with rel.inf < 1 and apply neg mono. to tenure ####
set.seed(98765)
fit1 <- gbm(churn_target ~ 1 + 
              # gender_factor +
              SeniorCitizen +
              # partner_factor +
              # dependents_factor +
              tenure +
              # phoneService_factor +
              multipleLines_factor +
              internetService_factor +
              onlineSecurity_factor +
              # onlineBackup_factor +
              # deviceProtection_factor +
              techSupport_factor +
              streamingTV_factor +
              streamingMovies_factor +
              contract_factor +
              paperlessBilling_factor +
              paymentMethod_factor +
              feeChange_factor,
            data = train,
            var.monotone = c(0, -1, 0,0,0,0,0,0,0,0,0,0),
            distribution = "bernoulli",
            n.trees = 2500,
            interaction.depth = 4,
            shrinkage = 0.01,
            train.fraction = 0.8,
            bag.fraction = 0.6,
            verbose = TRUE)
best.iter.1 <- gbm.perf(object = fit1, method = "test"); best.iter.1
summary1 <- data.frame(summary.gbm(object = fit1,n.trees = best.iter.1), row.names = NULL)
train$pred1 <- predict.gbm(object = fit1,newdata = train,n.trees = best.iter.1,type = "response")
test$pred1 <- predict.gbm(object = fit1,newdata = test,n.trees = best.iter.1,type = "response")

getAUC(train$pred1, train$churn_target)
getAUC(test$pred1, test$churn_target)

pdp(fit1, "contract_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "tenure", best.iter.1, 2000, FALSE)  
pdp(fit1, "onlineSecurity_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "internetService_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "paymentMethod_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "feeChange_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "techSupport_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "streamingTV_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "paperlessBilling_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "multipleLines_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "streamingTV_factor", best.iter.1, 2000, TRUE)  
pdp(fit1, "SeniorCitizen", best.iter.1, 2000, FALSE)

## Calculate interactions for fit1 ####
intMat <- matrix(0, length(fit1$var.names), length(fit1$var.names))
for(i in 1:12){
  for(j in 1:(i)){
    cat("i = ", i, ". j = ", j, ".\n",sep = "")
    intMat[i,j] = interact.gbm(x = fit1,data = train,i.var = c(i, j),n.trees = best.iter.1)
  }
}

intDF <- intMat %>% 
  data.frame() %>% 
  mutate(var1 = fit1$var.names) %>% 
  select(var1, everything())
names(intDF) <- c("var1", fit1$var.names)
intDF_gathered <- intDF %>% 
  gather(var2, H_stat, -var1) %>% 
  filter(H_stat != 1, H_stat > 0) %>% 
  filter(H_stat > 0.1) %>% 
  arrange(desc(H_stat))

## Plot interactions for fit1 ####
pdp2 <-  function(mod, var1, var2, numTrees, numPoints, is.factor1, is.factor2){
  library(gbm)
  library(ggplot2)
  dat <- plot.gbm(x = mod, i.var = c(var1, var2), n.trees = numTrees, continuous.resolution = numPoints, type = "response",return.grid = TRUE)
  if(is.factor1 & !is.factor2){
    p <- ggplot(dat, aes_string(var2, "y", group = var1)) + 
      geom_line(aes_string(col = var1), stat = "identity") +
      geom_point(aes_string(shape = var1, col = var1)) +
      labs(title = "2-way Partial Dependence Plot", subtitle = paste0(var1, " and ", var2), y = "P(attrition)") +
      theme(text = element_text(size=20))
  } else if (is.factor1 & is.factor2){
    p <- ggplot(dat, aes_string(var2, "y")) + 
      geom_bar(stat = "identity") + 
      facet_grid(~enquo(var1)) +
      labs(title = "2-way Partial Dependence Plot", subtitle = paste0(var1, " and ", var2), y = "P(attrition)") +
      theme(text = element_text(size=20))
  } else{
    p <- NULL
  }
  
  return(p)
  
}
pdp2(fit1, "paymentMethod_factor", "SeniorCitizen", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "paymentMethod_factor", "multipleLines_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "multipleLines_factor", "SeniorCitizen", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "streamingTV_factor", "SeniorCitizen", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "paymentMethod_factor", "streamingMovies_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "paperlessBilling_factor", "multipleLines_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "paymentMethod_factor", "internetService_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "feeChange_factor", "paymentMethod_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "contract_factor", "streamingMovies_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "paymentMethod_factor", "streamingTV_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "feeChange_factor", "SeniorCitizen", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "feeChange_factor", "streamingMovies_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "feeChange_factor", "internetService_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "contract_factor", "internetService_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "contract_factor", "tenure", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "feeChange_factor", "onlineSecurity_factor", best.iter.1, 20, TRUE, FALSE)
pdp2(fit1, "paperlessBilling_factor", "techSupport_factor", best.iter.1, 20, TRUE, FALSE)

## 3-way pdps #### 
intMat3 <- array(0, dim = c(length(fit1$var.names), length(fit1$var.names), length(fit1$var.names)))
for(i in 1:12){
  for(j in 1:(i)){
    for(k in 1:j){
    cat("i = ", i, ". j = ", j, ". k = ", k, ".\n",sep = "")
    intMat3[i,j,k] = interact.gbm(x = fit1,data = train,i.var = c(i, j, k),n.trees = best.iter.1)
    }
  }
}
options(scipen=999)
intMat3[,,1]
nameKeys <- data.frame(abbrevs = LETTERS[1:length(fit1$var.names)], varNames = fit1$var.names, stringsAsFactors = FALSE)
## There doesnt seem to be any non-redundant 3-way interactions ####
as.data.frame.table(intMat3, responseName = "H_stat", stringsAsFactors = FALSE, dnn = c(fit1$var.names, fit1$var.names, fit1$var.names)) %>% 
  left_join(nameKeys, by = c("Var1" = "abbrevs")) %>% 
  rename(varNames1 = varNames) %>% 
  left_join(nameKeys, by = c("Var2" = "abbrevs")) %>% 
  rename(varNames2 = varNames) %>% 
  left_join(nameKeys, by = c("Var3" = "abbrevs")) %>% 
  rename(varNames3 = varNames) %>% 
  select(varNames1, varNames2, varNames3, H_stat) %>% 
  filter(varNames1 != varNames2 ) %>%
  filter(varNames2 != varNames3 ) %>%
  filter(varNames1 != varNames3 ) %>%
  filter(H_stat < 0.999, H_stat > 0) %>% 
  filter(H_stat > 0.02) %>%
  arrange(desc(H_stat)) %>% # View()
  dim()
## Notice we get the same here:
interact.gbm(x = fit1,data = train,i.var = c("paymentMethod_factor", "SeniorCitizen", "SeniorCitizen"),n.trees = best.iter.1)
interact.gbm(x = fit1,data = train,i.var = c("paymentMethod_factor", "SeniorCitizen"),n.trees = best.iter.1)

## Create "reasons" for attrition ####
offers <- c("offer1", "offer2", "offer3", "offer4", "offer5")
reasons <- c("dislike_hardware", "too_expensive", "moving", NA)

## Assign offers and reasons at random ####
## Assign high/med/low risk by test1 pred values ####
set.seed(2345)
train2 <- train %>% 
  mutate(offer = factor(sample(offers, size = nrow(.), replace = TRUE, prob = rep(1/length(offers), length(offers)))),
         reason = factor(sample(reasons, size = nrow(.), replace = TRUE, prob = rep(1/length(reasons), length(reasons)))),
         risk = factor(case_when(pred1 + runif(1, -0.3, 0.3) >= 0.7 ~ "High",
                                 pred1 + runif(1, -0.3, 0.3) < 0.7 & pred1 > 0.3 ~ "Medium",
                                 pred1 + runif(1, -0.3, 0.3) <= 0.3 ~ "Low",
                                 TRUE ~ "Low"),
                       levels = c("High", "Medium", "Low"))
         )
set.seed(2345)
test2 <- test %>% 
  mutate(offer = factor(sample(offers, size = nrow(.), replace = TRUE, prob = rep(1/length(offers), length(offers)))),
         reason = factor(sample(reasons, size = nrow(.), replace = TRUE, prob = rep(1/length(reasons), length(reasons)))),
         risk = factor(case_when(pred1 + runif(1, -0.3, 0.3) >= 0.7 ~ "High",
                                 pred1 + runif(1, -0.3, 0.3) < 0.7 & pred1 > 0.3 ~ "Medium",
                                 pred1 + runif(1, -0.3, 0.3) <= 0.3 ~ "Low",
                                 TRUE ~ "Low"),
                       levels = c("High", "Medium", "Low"))
  )


## Which offers are best/least effective overall? ####
train2 %>% 
  group_by(offer) %>% 
  summarise(percentAttrition = 100*sum(churn_target)/length(churn_target)) %>% 
  ungroup() %>% 
  arrange(percentAttrition)
test2 %>% 
  group_by(offer) %>% 
  summarise(percentAttrition = 100*sum(churn_target)/length(churn_target)) %>% 
  ungroup() %>% 
  arrange(percentAttrition)

## Which offers are best/least effective by reason? ####
train2 %>% 
  group_by(offer, reason) %>% 
  summarise(percentAttrition = 100*sum(churn_target)/length(churn_target)) %>% 
  ungroup() %>% 
  arrange(reason, percentAttrition)
test2 %>% 
  group_by(offer, reason) %>% 
  summarise(percentAttrition = 100*sum(churn_target)/length(churn_target)) %>% 
  ungroup() %>% 
  arrange(reason, percentAttrition)


## Which offer performs best in the context of a model? ####
set.seed(98765)
fit2 <- gbm(churn_target ~ 1 + 
              # gender_factor +
              SeniorCitizen +
              # partner_factor +
              # dependents_factor +
              tenure +
              # phoneService_factor +
              multipleLines_factor +
              internetService_factor +
              onlineSecurity_factor +
              # onlineBackup_factor +
              # deviceProtection_factor +
              techSupport_factor +
              streamingTV_factor +
              streamingMovies_factor +
              contract_factor +
              paperlessBilling_factor +
              paymentMethod_factor +
              feeChange_factor +
              offer +
              reason +
              risk,
            data = train2,
            var.monotone = c(0, -1, 0,0,0,0,0,0,0,0,0,0, 0,0,0),
            distribution = "bernoulli",
            n.trees = 2500,
            interaction.depth = 4,
            shrinkage = 0.01,
            train.fraction = 0.8,
            bag.fraction = 0.6,
            verbose = TRUE)
best.iter.2 <- gbm.perf(object = fit2, method = "test"); best.iter.2
summary2 <- data.frame(summary.gbm(object = fit2,n.trees = best.iter.2), row.names = NULL)
train2$pred2 <- predict.gbm(object = fit2,newdata = train2,n.trees = best.iter.2,type = "response")
test2$pred2 <- predict.gbm(object = fit2,newdata = test2,n.trees = best.iter.2,type = "response")

getAUC(train2$pred2, train2$churn_target)
getAUC(test2$pred2, test2$churn_target)

pdp(fit2, "risk", best.iter.2, 2000, TRUE)  
pdp(fit2, "offer", best.iter.2, 2000, TRUE)  
pdp(fit2, "tenure", best.iter.2, 2000, FALSE)  
pdp(fit2, "contract_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "reason", best.iter.2, 2000, TRUE)  
pdp(fit2, "paymentMethod_factor", best.iter.2, 2000, TRUE)
pdp(fit2, "feeChange_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "internetService_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "onlineSecurity_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "multipleLines_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "streamingMovies_factor", best.iter.2, 2000, TRUE)
pdp(fit2, "paperlessBilling_factor", best.iter.2, 2000, TRUE)
pdp(fit2, "techSupport_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "streamingTV_factor", best.iter.2, 2000, TRUE)  
pdp(fit2, "SeniorCitizen", best.iter.2, 2000, FALSE)

## Calculate interactions for fit1 ####
intMat2 <- matrix(0, length(fit2$var.names), length(fit2$var.names))
for(i in 1:length(fit2$var.names)){
  for(j in 1:(i)){
    cat("i = ", i, ". j = ", j, ".\n",sep = "")
    intMat2[i,j] = interact.gbm(x = fit2,data = train2,i.var = c(i, j),n.trees = best.iter.2)
  }
}

intDF2 <- intMat2 %>% 
  data.frame() %>% 
  mutate(var1 = fit2$var.names) %>% 
  select(var1, everything())
names(intDF2) <- c("var1", fit2$var.names)
intDF2_gathered <- intDF2 %>% 
  gather(var2, H_stat, -var1) %>% 
  filter(H_stat != 1, H_stat > 0) %>% 
  filter(H_stat > 0.1) %>% 
  arrange(desc(H_stat))

pdp2(fit2, "reason", "paymentMethod_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "paymentMethod_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "reason", "feeChange_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "reason", "offer", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "reason", "multipleLines_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "multipleLines_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "feeChange_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "reason", "streamingMovies_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "paymentMethod_factor", "SeniorCitizen", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "techSupport_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "paperlessBilling_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "reason", "SeniorCitizen", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "onlineSecurity_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "streamingMovies_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "offer", "internetService_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "reason", "tenure", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "paymentMethod_factor", "streamingMovies_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "paymentMethod_factor", "multipleLines_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "risk", "contract_factor", best.iter.2, 20, TRUE, FALSE)
pdp2(fit2, "contract_factor", "streamingMovies_factor", best.iter.2, 20, TRUE, FALSE)

## 3-way pdps #### 
intMat23 <- array(0, dim = c(length(fit2$var.names), length(fit2$var.names), length(fit2$var.names)))
for(i in 1:length(fit2$var.names)){
  for(j in 1:(i)){
    for(k in 1:j){
      cat("i = ", i, ". j = ", j, ". k = ", k, ".\n",sep = "")
      intMat23[i,j,k] = interact.gbm(x = fit2,data = train2,i.var = c(i, j, k),n.trees = best.iter.2)
    }
  }
}
options(scipen=999)
intMat23[,,1]
nameKeys2 <- data.frame(abbrevs = LETTERS[1:length(fit2$var.names)], varNames = fit2$var.names, stringsAsFactors = FALSE)
## There doesnt seem to be any non-redundant 3-way interactions ####
interactions_3way <- as.data.frame.table(intMat23, responseName = "H_stat", stringsAsFactors = FALSE, dnn = c(fit2$var.names, fit2$var.names, fit2$var.names)) %>% 
  left_join(nameKeys2, by = c("Var1" = "abbrevs")) %>% 
  rename(varNames1 = varNames) %>% 
  left_join(nameKeys2, by = c("Var2" = "abbrevs")) %>% 
  rename(varNames2 = varNames) %>% 
  left_join(nameKeys2, by = c("Var3" = "abbrevs")) %>% 
  rename(varNames3 = varNames) %>% 
  select(varNames1, varNames2, varNames3, H_stat) %>% 
  filter(varNames1 != varNames2 ) %>%
  filter(varNames2 != varNames3 ) %>%
  filter(varNames1 != varNames3 ) %>%
  filter(H_stat < 0.999, H_stat > 0) %>% 
  filter(H_stat > 0.01) %>% 
  arrange(desc(H_stat)) # %>% View()
  dim()
# save.image(file = "/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/workspace.RData")
# load(file = "/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/workspace.RData")
