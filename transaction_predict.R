# remove all the objects stored
rm(list = ls())

# Importing require Libraries
library(ggplot2)
# Importing library for spliting train and test data
library(caTools)
# Importing Library to evaluate PR and ROC
library(PRROC)
# Importing Library for KNN
library(class)
# Importing Library for Naiye Bayes
library(e1071)
# Importing Library for Decision Tree
library(C50)
# Importing Library for Random Forest
library(randomForest)
# Importing library for confusion matrix and correlation
library(caret)
# Importing some useful functions
library(dplyr)

# set current working directory
setwd("E:/Project_2")

# Importing the train and test data set.
# here train set will be used to bulid and test the Model
# The Model will be used to predict the value of target column in the test set
train = read.csv('train.csv', header=T)
test = read.csv('test.csv', header=T)

# Getting the column names of both datasets
colnames(train)
colnames(test)

# Getting the structure of the datasets
str(train)
# the train data have 1 String Variable(id_code), 1 target variable and 200 numerical variables.
str(test)
# the test data have 1 String Variable(id_code) and 200 numerical variables.

# Getting the number of variables and obervation in the datasets
dim(train)
# 200000 X 202
dim(test)
# 200000 X 201

# Getting summary for each columns in data set
summary(train)
summary(test)

# checking and removing duplicate rows
train = distinct(train)
test = distinct(test)
# No duplicate row present in data

# saving test id_code in different data frame
# so that we can save the predicted value to a new file with reference
test_output = data.frame(test$ID_code)

# removing ID_column from both data set
# as it does not contain any information for model building
train = train[, 2:202]
test = test[, 2:201]

# Encoding the target feature as factor
train$target = factor(train$target, levels = c(0, 1))

# looking at the target variable
table(train$target)
# we have 1,79902 '0' values and 20,098 '1' values
# we can see that the data is unbalanced

# Plotting target column for visualization
ggplot(train, aes(x=train$target))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  xlab("target") + ylab('Count') +
  theme_minimal()

# function to plot graph together
grid_plot = function() 
{
  gridExtra::grid.arrange(denvar_0, denvar_1, denvar_2, denvar_3, denvar_4,
                          denvar_5, denvar_6, denvar_7, denvar_8, denvar_9,
                          denvar_10, denvar_11, denvar_12, denvar_13, denvar_14,
                          denvar_15, denvar_16, denvar_17, denvar_18, denvar_19,
                          denvar_20, denvar_21, denvar_22, denvar_23, denvar_24, ncol=5)
  
  gridExtra::grid.arrange(denvar_25, denvar_26, denvar_27, denvar_28, denvar_29,
                          denvar_30, denvar_31, denvar_32, denvar_33, denvar_34,
                          denvar_35, denvar_36, denvar_37, denvar_38, denvar_39,
                          denvar_40, denvar_41, denvar_42, denvar_43, denvar_44,
                          denvar_45, denvar_46, denvar_47, denvar_48, denvar_49, ncol=5)
  
  gridExtra::grid.arrange(denvar_50, denvar_51, denvar_52, denvar_53, denvar_54,
                          denvar_55, denvar_56, denvar_57, denvar_58, denvar_59,
                          denvar_60, denvar_61, denvar_62, denvar_63, denvar_64,
                          denvar_65, denvar_66, denvar_67, denvar_68, denvar_69,
                          denvar_70, denvar_71, denvar_72, denvar_73, denvar_74, ncol=5)
  
  gridExtra::grid.arrange(denvar_75, denvar_76, denvar_77, denvar_78, denvar_79,
                          denvar_80, denvar_81, denvar_82, denvar_83, denvar_84,
                          denvar_85, denvar_86, denvar_87, denvar_88, denvar_89,
                          denvar_90, denvar_91, denvar_92, denvar_93, denvar_94,
                          denvar_95, denvar_96, denvar_97, denvar_98, denvar_99, ncol=5)
  
  gridExtra::grid.arrange(denvar_100, denvar_101, denvar_102, denvar_103, denvar_104,
                          denvar_105, denvar_106, denvar_107, denvar_108, denvar_109,
                          denvar_100, denvar_111, denvar_112, denvar_113, denvar_114,
                          denvar_115, denvar_116, denvar_117, denvar_118, denvar_119,
                          denvar_120, denvar_121, denvar_122, denvar_123, denvar_124, ncol=5)
  
  gridExtra::grid.arrange(denvar_125, denvar_126, denvar_127, denvar_128, denvar_129,
                          denvar_130, denvar_131, denvar_132, denvar_133, denvar_134,
                          denvar_135, denvar_136, denvar_137, denvar_138, denvar_139,
                          denvar_140, denvar_141, denvar_142, denvar_143, denvar_144,
                          denvar_145, denvar_146, denvar_147, denvar_148, denvar_149, ncol=5)
  
  gridExtra::grid.arrange(denvar_150, denvar_151, denvar_152, denvar_153, denvar_154,
                          denvar_155, denvar_156, denvar_157, denvar_158, denvar_159,
                          denvar_160, denvar_161, denvar_162, denvar_163, denvar_164,
                          denvar_165, denvar_166, denvar_167, denvar_168, denvar_169,
                          denvar_170, denvar_171, denvar_172, denvar_173, denvar_174, ncol=5)
  
  gridExtra::grid.arrange(denvar_175, denvar_176, denvar_177, denvar_178, denvar_179,
                          denvar_180, denvar_181, denvar_182, denvar_183, denvar_184,
                          denvar_185, denvar_186, denvar_187, denvar_188, denvar_189,
                          denvar_190, denvar_191, denvar_192, denvar_193, denvar_194,
                          denvar_195, denvar_196, denvar_197, denvar_198, denvar_199, ncol=5)
  
}

######################### Uncomment to plot Graphs ########################

# # plotting distribution of numerical variables of train Dataset
# for(i in colnames(train[,2:201])){
#   assign(paste0("den",i), ggplot(data = train, aes_string(x=i)) +
#            geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8))
# }
# grid_plot()
# rm(list = ls(pattern = "denvar"))
# 
# # plotting distribution of numerical variables of test Dataset
# for(i in colnames(test[,1:200])){
#   assign(paste0("den",i), ggplot(data = test, aes_string(x=i)) +
#            geom_density(fill="#f2b646", color="#e9ecef", alpha=0.8))
# }
# grid_plot()
# rm(list = ls(pattern = "denvar"))
# 
# # Plotting Density Distribution of train per class
# for (i in colnames(train[,2:201])){
#   assign(paste0("den",i), ggplot(train, aes_string(x= i,fill=train$target)) +
#            geom_density(kernel='gaussian', alpha=0.4) + ggtitle(i)+theme_classic())
# }
# grid_plot()
# rm(list = ls(pattern = "denvar"))

#################Missing Value Analysis####################

# checking null values in Train and Test data set
missing_value_train = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_value_train$Columns = row.names(missing_value_train)
row.names(missing_value_train) = NULL
missing_value_train = missing_value_train[,c(2,1)]
names(missing_value_train)[2] =  "count"
missing_value_train$missing_percent = (missing_value_train$count/nrow(train)) * 100

#There is no missing value in our train data set.

missing_value_test = data.frame(apply(test,2,function(x){sum(is.na(x))}))
missing_value_test$Columns = row.names(missing_value_test)
row.names(missing_value_test) = NULL
missing_value_test = missing_value_test[,c(2,1)]
names(missing_value_test)[2] =  "count"
missing_value_test$missing_percent = (missing_value_test$count/nrow(test)) * 100

# There is no missing value in our test data set.

#######################Outlier Analysis################################

# Replace all outliers in Train and Test with NA
for(i in colnames(train[,2:201])){
  val1 = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  val2 = test[,i][test[,i] %in% boxplot.stats(test[,i])$out]
  train[,i][train[,i] %in% val1] = NA
  test[,i][test[,i] %in% val2] = NA
}

# Imputing missing values with Mean
for(i in colnames(train[,2:201])){
  train[,i][is.na(train[,i])] = median(train[,i], na.rm = T)
  test[,i][is.na(test[,i])] = median(test[,i], na.rm = T)
}

#########################Feature Selection##################################

correlation_matrix = cor(train[,-1])
print(correlation_matrix)
highly_correlated = findCorrelation(correlation_matrix, cutoff = 0.5)
print(highly_correlated)
# correlation between every variableis less than 0.5 therefore we are selecting all variables

############################Feature Scaling##########################

# except target column all other column has been scaled using standardization
train[,-1] = scale(train[,-1])
test[,] = scale(test[,])

#################################Sampling################################

# Splitting the train data set into the Training set and Test set
set.seed(1234)
split = sample.split(train$target, SplitRatio = 0.70)
training_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)
rm(split, i, val1, val2)

###################################Model Development#################################

##############Logistic Regression###############

# Fitting Logistic Regression to the Training set
logit_classifier = glm(formula = target ~ .,
                       family = binomial,
                       data = training_set)

# Predicting the Test set results
prob_pred = predict(logit_classifier, type = 'response', newdata = test_set[,-1])
logit_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm_logit = table(test_set[, 1], logit_pred)
confusionMatrix(cm_logit, positive = '1', mode = "everything")

# saving predicted and actual values to different data frame to find ROC and PR.
scores <- data.frame(logit_pred, test_set$target)

# evaluating PR and ROC for Logistic classifier

pr <- pr.curve(scores.class0= scores[scores$test_set.target=="1",]$logit_pred,
               scores.class1= scores[scores$test_set.target=="0",]$logit_pred,
               curve=T)
plot(pr)

roc <- roc.curve(scores.class0= scores[scores$test_set.target=="1",]$logit_pred,
                 scores.class1= scores[scores$test_set.target=="0",]$logit_pred,
                 curve=T)
plot(roc)

################Naive Bayes#####################

# Fitting Naive Bayes to the Training set
nb_classifier = naiveBayes(x = training_set[,-1],
                           y = training_set$target)

# Predicting the Test set results
nb_pred = predict(nb_classifier, newdata = test_set[,-1])

# Making the Confusion Matrix
cm_nb = table(test_set[, 1], nb_pred)
confusionMatrix(cm_nb, positive = '1', mode = "everything")

# saving predicted and actual values to different data frame to find ROC and PR.
scores <- data.frame(nb_pred, test_set$target)

# evaluating PR and ROC for Naive Bayes classifier

pr <- pr.curve(scores.class0= scores[scores$test_set.target=="1",]$nb_pred,
               scores.class1= scores[scores$test_set.target=="0",]$nb_pred,
               curve=T)
plot(pr)

roc <- roc.curve(scores.class0= scores[scores$test_set.target=="1",]$nb_pred,
                 scores.class1= scores[scores$test_set.target=="0",]$nb_pred,
                 curve=T)
plot(roc)

#####################Random Forest##################

################# uncomment to use Random Forest ###############################

# # Fitting Random Forest Classification to the Training set
# set.seed(1234)
# rf_classifier = randomForest(target ~ ., training_set, importance = TRUE, ntree = 10)
# 
# # Predicting the Test set results
# rf_pred = predict(rf_classifier, newdata = test_set[-1])
# 
# # Making the Confusion Matrix
# cm_rf = table(test_set[, 1], rf_pred)
# confusionMatrix(cm_rf, positive = '1', mode = "everything")
# 
# # saving predicted and actual values to different data frame to find ROC and PR.
# scores <- data.frame(rf_pred, test_set$target)
# 
# # evaluating PR and ROC for Random forest classifier
# 
# pr <- pr.curve(scores.class0= scores[scores$test_set.target=="1",]$rf_pred,
#                scores.class1= scores[scores$test_set.target=="0",]$rf_pred,
#                curve=T)
# plot(pr)
# 
# roc <- roc.curve(scores.class0= scores[scores$test_set.target=="1",]$rf_pred,
#                  scores.class1= scores[scores$test_set.target=="0",]$rf_pred,
#                  curve=T)
# plot(roc)

# predicting test value and saving back to file
nb_test_pred = predict(nb_classifier, newdata = test[,])
test_output$target = nb_test_pred
# Writing a csv (output)
write.csv(test_output, "test_output_R.csv", row.names = F)
rm(pr, roc, scores, test_set, training_set)

# Plotting target column of Test Dataset for visualization
ggplot(test_output, aes(x=test_output$target))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  xlab("target") + ylab('Count') +
  theme_minimal()

table(test_output$target)
# We have predicted that out of 200000 customers only 7667 will make transaction.