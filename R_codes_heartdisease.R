#Harvard Capstone 2 - Heard disease prediction

#check for necessary libraries

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(latticeExtra)) install.packages("latticeExtra", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")

## Loading the data ##
# alternate google drive path to data. Data is downloaded from kaggle and uploaded to
# google drive and given public access

heart_data <-read.csv(sprintf("https://drive.google.com/uc?id=1voGOsP6Hg3q4Xw3MBm9d_632dGvxVzro&export=download"))

#relative path to the data
#heart_data <- read.csv(file = "heart.csv") 

#rename columns of the dataset 
colnames(heart_data) <- c("age", "sex", "chest_pain", "rest_BP", "cholesterol", "fast_blood_sugar", "rest_ecg", "maxHR",
                          "exercise_angina", "oldpeak", "slope", "ca", "thal", "disease_indicator")


head(heart_data)
summary(heart_data)
dim(heart_data)

colnames(heart_data)
sum(is.na(heart_data))


# converting col names to factors, renaming values
heart_data <- heart_data %>%filter(thal != 0) %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fast_blood_sugar = if_else(fast_blood_sugar == 1, ">120", "<=120"),
         exercise_angina = if_else(exercise_angina == 1, "Yes" ,"No"),
         chest_pain = if_else(chest_pain == 1, "Atypical angina",
                              if_else(chest_pain == 2, "Non-typical pain",
                                      if_else(chest_pain == 0, "Asymptomatic","Typical angina"))),
         rest_ecg = if_else(rest_ecg == 0, "Normal",
                            if_else(rest_ecg == 1, "Wave abnormality", "Hypertrophy")),
        
         
         ca = as.numeric(ca),
         disease_indicator = if_else(disease_indicator == 1, "No", "Yes")
  ) %>% 
  mutate_if(is.character, as.factor) 

heart_data <- heart_data %>%
  mutate(slope = if_else(slope == 2, "ascending", if_else(slope == 1, "flat","descending")),
         
         thal = case_when(
           thal == 1 ~ "fixed defect",
           thal == 2 ~ "normal",
           thal == 3 ~ "reversible defect"
         )
         )%>%
           mutate_if(is.character, as.factor) 


glimpse(heart_data)

class(heart_data$ca)
heart_data$ca
class(heart_data$ca)
heart_data$chest_pain
class(heart_data$oldpeak)
heart_data$fast_blood_sugar
heart_data$rest_ecg
heart_data$exercise_angina
heart_data$disease_indicator
heart_data$slope
heart_data$thal

head(heart_data)%>%kable%>%kable_styling()
#plot missing data
plot_missing(heart_data)

#look at numbers
plot_bar(heart_data)

#density plot function

#Density plot function
density_plot <- function(col,P){
  ggplot(heart_data, aes(x = col, fill = disease_indicator))+
    geom_density(alpha = 0.5)+
    theme(legend.position = "bottom")+
    scale_fill_manual(values=c("gold", "blue", "#56B4E9"))+
    scale_x_continuous(name = P)
  
}


#age density plot
age_dplot <- density_plot(heart_data$age, "Age")

#age barplot
heart_data %>% ggplot(aes(age, fill = disease_indicator))+
  geom_bar() +scale_fill_manual(values = c("green4", "khaki3"))
 
#rest_bp density plot
plot_bp <- density_plot(heart_data$rest_BP, "rest_bp")

#oldpeak density plot
plot_old_peak <- density_plot(heart_data$oldpeak, "oldpeak")

#maxHR density plot
plot_maxHR <- density_plot(heart_data$maxHR, "maxHR")

#cholesterol density plot
plot_cholesterol <- density_plot(heart_data$cholesterol, "cholesterol")

#ca density plot
plot_ca <- density_plot(heart_data$ca, "ca")

#arranging plots in a grid
grid.arrange(age_dplot, plot_bp, plot_maxHR, plot_old_peak, plot_cholesterol,plot_ca, ncol = 3, nrow = 2)

#sex barplot
heart_data %>% ggplot(aes(sex, fill = disease_indicator))+
  geom_bar(width = 0.5)+
  geom_label(stat = "count",aes(label = ..count..),show.legend = FALSE, vjust = 1)+
  scale_fill_manual(values = c("lightpink1","powderblue"))+ 
  labs(x="sex",y="patients")


# chest pain

heart_data %>% ggplot(aes(chest_pain,fill = disease_indicator))+
  geom_bar(width = 0.3)+

  scale_fill_manual(values = c("sienna1","slateblue"))+
  labs(x="Chest_pain",y="patients")

#cholesterol with age

heart_data %>% ggplot(aes(age,cholesterol, color = sex, size= cholesterol))+
  geom_point(shape = 20)+ scale_color_manual(values = c("blue","orange"))+
  facet_grid(~disease_indicator)

# Age vs maxHR

heart_data %>% ggplot(aes(age, maxHR, color = disease_indicator)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess",size = 1)+
  scale_color_manual(values = c("brown3","slateblue"))+ 
  theme(panel.background = element_rect(fill = "white"),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  facet_grid(~sex)

#bp vs sex
heart_data %>% ggplot(aes(sex,rest_BP))+
  geom_boxplot(color = "black", fill = "green1")+
  labs(x = "Sex", y ="Blood pressure")+
  facet_grid(~chest_pain)

### Slope and Thallium

plot_thal <- heart_data %>% ggplot(aes(thal, fill = disease_indicator))+
  geom_bar(stat = "count", width = 0.4)+scale_fill_manual(values = c("gold3","purple4"))+ theme(legend.position="bottom")

plot_slope <- heart_data %>% ggplot(aes(slope, fill = disease_indicator))+
  geom_bar(width = 0.4) + scale_fill_manual(values = c("wheat3","darkolivegreen3"))+ theme(legend.position="bottom")

grid.arrange(plot_thal,plot_slope, ncol = 2)


#correlation plot

heart_c <- round(cor(heart_data[c(1,4,5,8,10,12)]),3)
heart_c

ggcorrplot(heart_c, hc.order = TRUE,
           outline.color = "white", ggtheme = ggplot2::theme_light,
           colors =  c("#6D9EC1", "white", "#E46726")
)


#PCA

pca <- prcomp(heart_data[c(1,4,5,8,10,12)]) #showing only intergers and factors

summary(pca) 

#PCA boxplot
data.frame(type = heart_data$disease_indicator, pca$x[,1:6]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()


# Calculate variance scores per principal component
# pca.var <- pca$sdev^2
# variance <- cumsum(pca$sdev^2/sum(pca$sdev^2))


#variable importance to choose
set.seed(1, sample.kind = "Rounding")
var_imp_heart <- randomForest(disease_indicator~., data = heart_data,
                              importance = TRUE)
#varimpplot
varImpPlot(var_imp_heart) 


#*********************************************************************************
# split into training and test sets
#*********************************************************************************

set.seed(1, sample.kind = "Rounding")
train_index <- createDataPartition(y = heart_data$disease_indicator, 
                                   times = 1, p = 0.7, list = FALSE)
train_heart <- heart_data[train_index,]
test_heart <- heart_data[-train_index,]

train_heart<-subset(train_heart,select =  -c(cholesterol,fast_blood_sugar,
                                             rest_BP,rest_ecg))
test_heart<-subset(test_heart,select =  -c(cholesterol,fast_blood_sugar,
                                           rest_BP,rest_ecg))

#crossvalidation of 10 fold
fitControl <- trainControl(method="cv", number=10)

# dim(train_heart)
# dim(test_heart)

#logistic regression analysis

set.seed(127, sample.kind = "Rounding")
train_glm <- train(disease_indicator ~.,
                  data = train_heart,
                  method = "glm",
                  trControl = fitControl, tuneGrid = NULL)

#prediction
glm_predict <- predict(train_glm, test_heart)
mean(glm_predict == test_heart$disease_indicator)

#confusion matrix
glm_c <- confusionMatrix(glm_predict,test_heart$disease_indicator, 
                         positive = "Yes")

accuracy_glm <- glm_c$overall["Accuracy"]
sensitivity_glm <- glm_c$byClass["Sensitivity"]
specificity_glm <- glm_c$byClass["Specificity"]
pos_pred_glm <- glm_c$byClass["Pos Pred Value"]
neg_pred_glm <- glm_c$byClass["Neg Pred Value"]

#table of accuracy

accuracy_results <- data_frame(Method = "Logistic Regression", Accuracy = accuracy_glm)
accuracy_results%>%kable%>%kable_styling(position="center")

# KNN method

set.seed(2020, sample.kind = "Rounding")
                   
train_kn <- train(disease_indicator ~. ,
                  data = train_heart,
                  method = "knn",
                  trControl = fitControl,
                  tuneGrid = data.frame(k = seq(2, 30, 2)))

train_kn$bestTune #besttune parameter

#prediction
kn_predict <- predict(train_kn, test_heart)
mean(kn_predict == test_heart$disease_indicator)

#plot prediction
plot_kn <- ggplot(train_kn,highlight = TRUE)

#confusion matrix
kn_c <- confusionMatrix(kn_predict,test_heart$disease_indicator, positive = "Yes")

accuracy_kn <- kn_c$overall["Accuracy"]
sensitivity_kn <- kn_c$byClass["Sensitivity"]
specificity_kn <- kn_c$byClass["Specificity"]
pos_pred_kn <- kn_c$byClass["Pos Pred Value"]
neg_pred_kn <- kn_c$byClass["Neg Pred Value"]

#table of accuracies 
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method = "KNN", Accuracy = accuracy_kn))
accuracy_results%>%kable%>%kable_styling(position="center")


# Regression tree model

set.seed(13, sample.kind = "Rounding")
train_rpart <- train(disease_indicator ~.,
                     data = train_heart,
                     method = "rpart")

#prediction
rpart_predict <- predict(train_rpart, test_heart)
mean(rpart_predict == test_heart$disease_indicator)

#confusion matrix
rpart_c <- confusionMatrix(rpart_predict,test_heart$disease_indicator, positive = "Yes")

accuracy_rpart <- rpart_c$overall["Accuracy"]
sensitivity_rpart <- rpart_c$byClass["Sensitivity"]
specificity_rpart <- rpart_c$byClass["Specificity"]
pos_pred_rpart<- rpart_c$byClass["Pos Pred Value"]
neg_pred_rpart <- rpart_c$byClass["Neg Pred Value"]

#table of accuracies
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method = "Regression Trees", Accuracy = accuracy_rpart))
                                         
accuracy_results%>%kable%>%kable_styling(position="center")



# Random forest model

set.seed(13, sample.kind = "Rounding")
tuning <- data.frame(mtry = c(1,20,1))

train_rf <- train(disease_indicator ~.,
                     data = train_heart,
                     method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE)
train_rf$bestTune  #besttune parameter 

#prediction
rf_predict <- predict(train_rf, test_heart)
mean(rf_predict == test_heart$disease_indicator)

#variable importance
vi <- varImp(train_rf)
plot(vi)

#confusion matrix
rf_c <- confusionMatrix(rf_predict,test_heart$disease_indicator, positive = "Yes")

accuracy_rf <- rf_c$overall["Accuracy"]
sensitivity_rf <- rf_c$byClass["Sensitivity"]
specificity_rf <- rf_c$byClass["Specificity"]
pos_pred_rf <- rf_c$byClass["Pos Pred Value"]
neg_pred_rf <- rf_c$byClass["Neg Pred Value"]


#table of accuracies
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method = "Random Forest", Accuracy = accuracy_rf)) 
accuracy_results%>%kable%>%kable_styling(position="center")


#adaptive boosting model
set.seed(13, sample.kind = "Rounding")

train_ada <- train(disease_indicator ~.,
                   data = train_heart,
                   method = "adaboost")

#prediction
ada_predict <- predict(train_ada, test_heart)
mean(ada_predict == test_heart$disease_indicator)

#confusion matrix
ada_c <- confusionMatrix(ada_predict,test_heart$disease_indicator, positive = "Yes")

accuracy_ada <- ada_c$overall["Accuracy"]
sensitivity_ada <- ada_c$byClass["Sensitivity"]
specificity_ada <- ada_c$byClass["Specificity"]
pos_pred_ada <- ada_c$byClass["Pos Pred Value"]
neg_pred_ada <- ada_c$byClass["Neg Pred Value"]


#table of accuracies
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method = "Ada Boost", Accuracy = accuracy_ada)) 
accuracy_results%>%kable%>%kable_styling(position="center")

#QDA model
train_qda <- train(disease_indicator ~.,
                     data = train_heart,
                     method = "qda")
#prediction
qda_predict <- predict(train_ada, test_heart)
mean(qda_predict == test_heart$disease_indicator)

#confusion matrix
qda_c <- confusionMatrix(qda_predict,test_heart$disease_indicator, positive = "Yes")

accuracy_qda <- qda_c$overall["Accuracy"]
sensitivity_qda <- qda_c$byClass["Sensitivity"]
specificity_qda <- qda_c$byClass["Specificity"]
pos_pred_qda <- qda_c$byClass["Pos Pred Value"]
neg_pred_qda <- qda_c$byClass["Neg Pred Value"]

#table of accuracies
accuracy_results <- bind_rows(accuracy_results,
                              data_frame(Method = "QDA", Accuracy = accuracy_qda)) 
accuracy_results%>%kable%>%kable_styling(position="center")


#table for results

table1<- matrix(c(A,B,C,D),ncol = 2, byrow = TRUE)
colnames(table1) <- c("Event","No Event")
rownames(table1) <- c("Event","No Event")

table1  

#creating a table

# library(tibble)
# results_model <- tibble::tribble(
#   ~Method, ~Accuracy, ~Sensitivity, ~Specificity, ~Pos, ~Neg,
#   "Logistic Regression", accuracy_glm, sensitivity_glm,specificity_glm, 
#   pos_pred_glm,neg_pred_glm),"KNN",accuracy_kn,sensitivity_kn, 
#   specificity_kn,pos_pred_kn,neg_pred_kn, "Regression Trees", 
#   accuracy_rpart, sensitivity_rpart,specificity_rpart,pos_pred_rpart,
#   neg_pred_rpart,"Random Forest", accuracy_rf,sensitivity_rf,
#   specificity_rf,pos_pred_rf,neg_pred_rf, "Ada Boost", accuracy_ada, 
#   sensitivity_ada,specificity_ada, pos_pred_ada, neg_pred_ada, "QDA", 
#   accuracy_qda,sensitivity_qda,specificity_qda, pos_pred_qda,neg_pred_qda) 
# 
# class(results_model)


#list of confusion matrix
cm_list <- list(
  Logistic_Regression =glm_c,
  Knn = kn_c,
  Regression_Trees = rpart_c,
  Random_Forest= rf_c,
  Ada_boost = ada_c,
  QDA = qda_c)
     
cm_results <- sapply(cm_list, function(a) a$byClass)#representation with sapply
cm_results %>% knitr::kable()
```


