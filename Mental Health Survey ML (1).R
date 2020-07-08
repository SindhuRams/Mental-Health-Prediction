#Final Project - Predictive Analytics
#Team Members - Indrayani Deshmukh, Snehal Bende, Sindhu Ramaswamy
#Mental Health Survey data OSMI

#Installing the packages and loading them.
install_load <- function (packages)  {   
  
  for(package in packages){
    
    # If package is installed
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # If package is not installed
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}

# loading the required librarires
libs <- c("ggplot2", "maps","magrittr","plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer","tidyverse","gridExtra")
install_load(libs)

# Loading specific methods from libraries
libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)


survey_data <- read.csv(file.choose())
#Structure of the survey data
str(survey_data)
View(survey_data)
summary(survey_data)# summary of the survey data
dim(survey_data) #dimension of the survey data

library(dplyr)

survey <- survey_data %>% select(treatment, Age, Gender, family_history, no_employees, tech_company) 
# check instructure of data including selected variables
str(survey)

#Logistic Regression model considereing 80% of the variables 
lm1 <- glm(treatment ~ Timestamp + Age + Gender + Country + state + self_employed + family_history + work_interfere + no_employees + remote_work + tech_company + benefits + care_options + leave + mental_vs_physical+coworkers +seek_help , data = survey_data, family = "binomial" )
summary(lm1)

#Studying each variable in detail 
table(survey$treatment)
table(survey$family_history) 
table(survey$tech_company)


#no_employees is not an ordered variable.
#Ordering the no_employees variable
summary(survey$no_employees)
employees_level_order <- factor(survey$no_employees, levels = c("1-5","6-25","26-100","100-500", "500-1000","More than 1000"))

# Company distribution graph
survey %>% ggplot(aes(x=employees_level_order))+
  geom_bar(fill = "lightgreen") + ggtitle("Company Size: Ordered")
# Total companies in each size range
table(survey$no_employees)

# categorizing gender variable

Male <- c("Male ","Cis Man", "Malr", "Male", "male", "M", "m", "Male-ish", "maile", "Mal", "Male (CIS)", "Cis Male", "Make", "Male", "Man", "msle", "Mail", "cis male")
Female <- c("Female ","femail","Female (cis)","female","Female","F","Woman","f","Femake","woman","Female","cis-female/femme", "Cis Female", "Trans-female", "Female (trans)", "Trans woman")
Queer <-c ("ostensibly male, unsure what that really means","p","A little about you","queer","Neuter","something kinda male?","non-binary","Nah","All","Enby","fluid","Genderqueer","Androgyne","Agender","Guy (-ish) ^_^","male leaning androgynous", "queer/she/they")


survey$Gender <- sapply(
  as.vector(survey$Gender),
  function(x) if(x %in% Male) "Male" else x ) 

survey$Gender <- sapply(
  as.vector(survey$Gender),
  function(x) if(x %in% Female) "Female" else x ) 

survey$Gender <- sapply(
  as.vector(survey$Gender),
  function(x) if(x %in% Queer) "Queer" else x ) 
survey$Gender <- as.factor(survey$Gender)

# Records in each category
table(survey$Gender)
table(survey$Gender)/length(survey$Gender) #studying the relative frequency of the gender variable

# Visualize the number of subjects in each gender type  

Gender_df <- ggplot(gender_diversity, aes(x = Gender, y = count, fill = Gender)) +  
  geom_bar(stat = "identity", alpha = 0.5) +
  xlab("Gender Diversity") + 
  ylab("Number of People") + 
  ggtitle("Gender Diversity in Tech Survey")

Gender_df

# Age Variable --Outlier management: replacing with median value
survey$Age[which(survey$Age<0)]<- median(survey$Age)
survey$Age[which(survey$Age>100)]<- median(survey$Age)
Age_one <- survey$Age
Age_one
# Summary Age
summary(survey$Age)

g2 <- ggplot(survey,aes(x=Age))+geom_histogram(aes(y=..density..), fill="pink")+geom_density(col="#3438BD",alpha = 0.5)+labs(x="Age",title="Transformed Age Distribution")
g2
# Age variable categorization
survey$Age<-cut(survey$Age, breaks = c(0, 16, 34, 60, 75), labels = c('Fresh', 'Junior', 'Senior', 'Super'))
table(survey$Age) 

# Group by Age Group and count each group

age_group <- survey %>%
  group_by(Age) %>%
  dplyr::summarize(count = n())
age_group

g3 <- ggplot(age_group, aes(x = Age, y = count, fill = Age)) +  
  geom_bar(stat = "identity", alpha = 0.5) +
  xlab("Age Group") + 
  ylab("Number of People") + 
  ggtitle("Age Group in the Tech Survey")

g3
summary(survey)

# treatment ratio for tech companies
survey %>% ggplot(aes(x=tech_company, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Treatment Ratio in the Tech companies")

# Focusing only on the data related to the Tech company 
Tech <- survey %>% select(treatment, Age, Gender, family_history, no_employees, tech_company) %>% filter(tech_company == "Yes")
summary(Tech)

# Age Distribution graph
Age_1 <- survey %>% ggplot(aes(x=Age_one, fill = factor(treatment))) +
  geom_density(alpha = 0.5) + ggtitle("Distribution of Age")
Age_1

# Comparing treatment ratio in Age groups
Age_2 <- survey %>% ggplot(aes(x=Age, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Treatment Ratio per Age Groups")
Age_2

# Comparing treatment ratio in Age groups focusing on tech field
Age_3 <- Tech %>% ggplot(aes(x=Age, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Treatment Ratio per Age Groups based on tech field")
Age_3


# Comparing treatment ratio in Gender groups
g1 <- survey %>% ggplot(aes(x=Gender, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Treatment Ratio")
g1
# Comparing treatment ratio in Gender groups focusing on tech industry
g2 <- Tech %>% ggplot(aes(x=Gender, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Treatment Ratio based on tech industry")

g2

# studying the family_history variable
f1 <- survey %>% ggplot(aes(x=family_history)) +
  geom_bar(fill = "pink")
f1
# Comparing Family_history treatment ratio
f2 <- survey %>% ggplot(aes(x=family_history, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Family_history Treatment Ratio for the entire data")
f2
# Comparing Family_history treatment ratio focusing on tech industy
f3 <- Tech %>% ggplot(aes(x=family_history, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Family_history Treatment Ratio for Tech field")
f3

# level_order
level_order1 <- factor(Tech$no_employees, levels = c("1-5","6-25","26-100","100-500", "500-1000","More than 1000"))


#Treatment ratio in the  tech industry
z1 <- Tech %>% ggplot(aes(x=level_order1, fill = (treatment))) +
  geom_bar(position = "fill") + ggtitle("Treatment Ratio in the Tech Industry")
z1

#Final dataframe with important variables focusing on answering the project goal
summary(Tech)

#LOGISTIC REGRESSION MODEL
# Fit logistic model to the data required to answer the project goal
lm <- glm( treatment ~ Age + Gender + no_employees + family_history, data = Tech, family = "binomial" )
summary(lm)
coef(lm)
# Fit logistic model to full data
lm1 <- glm(treatment ~ ., data = survey_data, family = "binomial" )
summary(lm1)
coef(lm1)

#splitting the dataset to training and testing 
i <- nrow(Tech)
i
train_ind <- sample(seq_len(i), size = floor(0.8*i))

Tech_training <- Tech[train_ind, ]
Tech_testing <- Tech[-train_ind, ]

# TRANSFORMATION FUNCTION FOR FEATURE ENGINEERING
transformations <- function(Tech) {
  # Gender
  # Create the list of three categories
  Male <- c("Male ","Cis Man", "Malr", "Male", "male", "M", "m", "Male-ish", "maile", "Mal", "Male (CIS)", "Cis Male", "Make", "Male", "Man", "msle", "Mail", "cis male")
  Female <- c("Female ","femail","Female (cis)","female","Female","F","Woman","f","Femake","woman","Female","cis-female/femme", "Cis Female", "Trans-female", "Female (trans)", "Trans woman")
  Queer <-c ("ostensibly male, unsure what that really means","p","A little about you","queer","Neuter","something kinda male?","non-binary","Nah","All","Enby","fluid","Genderqueer","Androgyne","Agender","Guy (-ish) ^_^","male leaning androgynous", "queer/she/they")
  # Categorize genders
  Tech$Gender <- sapply(
    as.vector(Tech$Gender),
    function(x) if(x %in% Male) "Male" else x ) 
  
  Tech$Gender <- sapply(
    as.vector(Tech$Gender),
    function(x) if(x %in% Female) "Female" else x ) 
  
  Tech$Gender <- sapply(
    as.vector(Tech$Gender),
    function(x) if(x %in% Queer) "Queer" else x ) 
  # Age
  # Replacing negative values and outliers with median
  Tech$Age <- as.numeric(Tech$Age)
  Tech$Age[which(Tech$Age<0)]<- median(Tech$Age)
  Tech$Age[which(Tech$Age>100)]<- median(Tech$Age)
  
  # Summary Age
  summary(Tech$Age)
  
  # Age categorization#
  Tech$Age1 <- cut(Tech$Age, breaks = c(0, 16, 34, 60, 75), labels = c('Fresh', 'Junior', 'Senior', 'Super'))
  
  # Verify Age group
  Tech$Age1 %>% table
  
  # Return the transformed dataframe
  return(Tech)
}
# Feature Engineerung for Test and Train Dataset
Tech_training <- Tech_training %>% transformations
Tech_testing <- Tech_testing %>% transformations

# Train Data
Tech_training %>% head(2)

# Test data
Tech_testing %>% head(2)

# Training the logistic regression model  with feature engineering 
lm_train <- glm(treatment ~ Age + Gender + family_history + no_employees, data = Tech_training, family = "binomial")
summary(lm_train)

# Predictions on the training set
Tech_training$predict_probs <- predict(lm_train, Tech_training, type = "response")
Tech_training$predict <- ifelse(Tech_training$predict_probs < 0.5, "No", "Yes")

# Predictions on the test set
Tech_testing$predict_probs <- predict(lm_train, Tech_testing, type = "response")
Tech_testing$predict <- ifelse(Tech_testing$predict_probs < 0.5, "No", "Yes")

# Confusion matrix for training data 
cm_train <- table(Tech_training$treatment, Tech_training$predict, dnn = c("real", "predict"))
cm_train

paste('Accuracy:', round(( cm_train['Yes','Yes'] + cm_train['No','No'] ) / sum(cm_train),2))
paste('Precision:', round(cm_train['Yes','Yes'] / sum(cm_train['Yes',]),2))
paste('Recall:', round(cm_train['Yes','Yes'] / sum(cm_train[,'Yes']),2))

# Confusion matrix for testing data
cm_test <- table(Tech_testing$treatment, Tech_testing$predict, dnn = c("real", "predict"))
cm_test

paste('Accuracy:', round(( cm_test['Yes','Yes'] + cm_test['No','No'] ) / sum(cm_test),2))

paste('Precision:', round(cm_test['Yes','Yes'] / sum(cm_test['Yes',]),2))

paste('Recall:', round(cm_test['Yes','Yes'] / sum(cm_test[,'Yes']),2))


#OPTIMIZATION
#STEP AIC
# Optimizing the logistic regression model using Step AIC
library(MASS)

step.model <- lm_train %>% stepAIC(trace = FALSE)
coef(step.model)

#Predictions
probabilities <- predict(step.model, Tech_testing, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
cm_1 <- table(Tech_testing$treatment, predicted.classes, dnn = c("real", "predict"))
cm_1
paste('Accuracy:', round(( cm_1['Yes','Yes'] + cm_1['No','No'] ) / sum(cm_1),2))


# BUILDING THE KNN MODEL
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 10)
set.seed(333)
fit <- train(treatment ~.,
             data = Tech_training,
             tuneGrid = expand.grid(k=2),
             method = 'knn',
             trControl = trControl)

predict_knn <- predict(fit,Tech_testing)
cm_knn <- with(Tech_testing,table(predict_knn,treatment))
cm_knn
paste('Accuracy:', sum(diag(cm_knn)) / sum(cm_knn) * 100 )



# RANDOM FOREST MODEL

library(randomForest)
library(scales)

control <- trainControl(method="repeatedcv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rf <-  train(treatment ~., Tech_training, 
                   method = "rf", 
                   ntree = 500,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

ggplot(train_rf)

predict <- predict(train_rf,Tech_testing)
cm <- with(Tech_testing,table(predict,treatment))
cm
paste('Accuracy:', sum(diag(cm)) / sum(cm) * 100 )




############# XgBoost#########

parameterGrid <-  expand.grid(eta = 0.1, # shrinkage (learning rate)
                              colsample_bytree = c(0.5,0.7), # subsample ration of columns
                              max_depth = c(4,7), # max tree depth. model complexity
                              nrounds = 10, # boosting iterations
                              gamma = 1.5, # minimum loss reduction
                              subsample = 0.8, # ratio of the training instances
                              min_child_weight = 2) # minimum sum of instance weight

model_xgb <- train(treatment ~ .,
                   data = Tech_training,
                   method = "xgbTree",
                   trControl = trainControl(),
                   tuneGrid=parameterGrid)
model_xgb
# Prediction. Creating a dataframe with the probabilities
predict_xgb <- predict(model_xgb, new_data = Tech_testing)
# Confussion matrix
confusionMatrix(Tech_training$treatment, predict_xgb)



#################### building support vector machine model #########

require(e1071) 
model_svm<-svm(treatment~.,data=Tech_training,kernel='linear',gamma= 1,cost=100)
model_svm
test_pred <- predict(model_svm, newdata = Tech_testing)
test_pred
confusionMatrix(test_pred, Tech_testing$treatment )








