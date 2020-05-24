m_2 <- read.csv(file.choose(), header = T)
# View(mission_2)
m_2 <- m_2[,-c(1,4,5,7,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,26,28,
                           31,33,34,36,37,38,39,40,41,47,48,49,50,51,52,53,54,57,61,
                           63,64)]
colnames(m_2) <- c("agegr18","agels65","male","unmarried","cad.tvd",
                         "bmi","pulseN","bph","bpl","rrN",
                         "diabetes1","hbAd","hbChild","ur","cret","bcr","billed",
                         "concession","stay","icu","ward","implant","pol")
summary(m_2)
str(m_2)
m_2$pol <- as.factor(m_2$pol)
str(m_2)
table(m_2$pol)
summary(m_2)

#install.packages("caTools")
library(caTools)
set.seed(009)
s_2 <- sample.split(m_2, SplitRatio = 0.7)
training_2 <- subset(m_2, s_2 == T)
testing_2 <- subset(m_2, s_2 == F)
length(testing_2)
table(training_2$pol)
summary(training_2)

#Scaling
#train_2[-23] <- scale(train_2[-23])
#test_2[-23] <- scale(test_2[-23])

# Model
logit_model2 <- glm(pol ~., data = training_2, family = "binomial")
logit_model2
summary(logit_model2)

fitted_results2 <- predict(logit_model2, testing_2, type = "response")
fitted_results2

fitted_results20 <- ifelse(fitted_results2 > 0.5, 1, 0)
fitted_results20

table(testing_2$pol, fitted_results20)

mis_ClassError_2 <- mean(fitted_results20 != testing_2$pol)
mis_ClassError_2
print(paste("Accuracy = ", 1 - mis_ClassError_2))

# install.packages("caret")
library(caret)
confusion_matrix_2 <- confusionMatrix(table(testing_2$pol,fitted_results20))
confusion_matrix_2

# ROC-AUC Curve
# install.packages("pROC")
library(pROC)
par(pty = "s")

# ROC-AUC Curve
# Without Threshold
roc_info2 <- roc(testing_2$pol, fitted_results2, plot = T, legacy.axes = T, percent = T,
                 xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                 col = "purple", lwd = 4, print.auc = T, main = "WS ROC model 2")

#With Threshold
roc_info20 <- roc(testing_2$pol, fitted_results20, plot = T, legacy.axes = T, percent = T,
                  xlab = "False Positive Percentage", ylab = "True Positive Percentage",
                  col = "purple", lwd = 4, print.auc = T, main = "ROC model 20")
