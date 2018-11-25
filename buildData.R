## packages used ####
library(dplyr)
library(Hmisc)
library(ggplot2)

## Pull kaggle data ####
## https://www.kaggle.com/blastchar/telco-customer-churn/version/1 ####
rawdata <- read.csv(file = "/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/WA_Fn-UseC_-Telco-Customer-Churn.csv",stringsAsFactors = FALSE)
str(rawdata)
describe(rawdata)

## Form data into factor so that reference level is the level occurring most frequently ####
data <- rawdata %>% 
  mutate(gender_factor = factor(gender, levels = c("Male", "Female")),
         partner_factor = factor(Partner, levels = c("No", "Yes")),
         dependents_factor = factor(Dependents, levels = c("No", "Yes")),
         phoneService_factor = factor(PhoneService, levels = c("Yes", "No")),
         multipleLines_factor = factor(MultipleLines, levels = c("No", "Yes", "No phone service")),
         internetService_factor = factor(InternetService, levels = c("Fiber optic", "DSL", "No")),
         onlineSecurity_factor = factor(OnlineSecurity, levels = c("No", "Yes", "No internet service")),
         onlineBackup_factor = factor(OnlineBackup, levels = c("No", "Yes", "No internet service")),
         deviceProtection_factor = factor(DeviceProtection, levels = c("No", "Yes", "No internet service")),
         techSupport_factor = factor(TechSupport, levels = c("No", "Yes", "No internet service")),
         streamingTV_factor = factor(StreamingTV, levels = c("No", "Yes", "No internet service")),
         streamingMovies_factor = factor(StreamingMovies, levels = c("No", "Yes", "No internet service")),
         contract_factor = factor(Contract, levels = c("Month-to-month", "Two year", "One year")),
         paperlessBilling_factor = factor(PaperlessBilling, levels = c("Yes", "No")),
         paymentMethod_factor = factor(PaymentMethod, levels = c("Electronic check", "Mailed check", "Bank transfer (automatic)", "Credit card (automatic")),
         tenureTimesMonthly = tenure*MonthlyCharges,
         feeChange = case_when(TotalCharges < tenureTimesMonthly ~ "Prior Increase",
                               TotalCharges > tenureTimesMonthly ~ "Prior Decrease",
                               TotalCharges == tenureTimesMonthly ~ "No Change",
                               TRUE ~ "NA"),
         feeChange = na_if(feeChange, "NA"),
         feeChange_factor = factor(feeChange, levels = c("Prior Increase", "Prior Decrease", "No Change")),
         churn_target = if_else(Churn=="Yes", 1, 0))

## How close is total charges to tenure x monthly charges? ####
describe(data$tenureTimesMonthly)
## If total charges less than current monthly bill x tenure, an increase took place
## If total charges greater than current monthly bill x tenure, a decrease took place 
data %>% 
  ggplot(., aes(tenureTimesMonthly, TotalCharges)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red")
## How many are exactly equal? ####
data %>% filter(tenureTimesMonthly == TotalCharges) %>% nrow()
describe(data$feeChange)

## Build a model ####
set.seed(12345)
## Create train/test data
data_split <- data %>% 
  mutate(unif = runif(n = nrow(.)),
         dat = if_else(unif <= 0.7, "Train", "Test"))
train <- data_split %>% filter(dat == "Train")
test <- data_split %>% filter(dat == "Test")

## Save train/test to separate rds files ####
saveRDS(train, "/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/train.rds")
saveRDS(test, "/Users/robertgordon/Documents/equifaxCaseStudy/sampleAttritionModel/data/test.rds")
