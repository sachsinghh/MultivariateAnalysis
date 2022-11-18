#CLASSIFICATION ANALYSIS - Decision Tree

#setup
library(rpart)
library(rattle)
library(caret)
library(party)
head(readingSkills)
summary(readingSkills)
str(readingSkills)
#Pembagian Data Training dan testing 80:20
n <- round(nrow(readingSkills)*0.80)
set.seed(123)
samp = sample(1:nrow(readingSkills),n)
data.train = readingSkills[samp,]
data.test = readingSkills[-samp,]

# bentuk model klasifikasi DT menggunakan TRAINING DATA
  # naive speaker digunakan sbg var dependen 
  # var age, shoesize, dan skor var independen
fit <- rpart(nativeSpeaker~., data = data.train, method = 'class')
summary(fit)
fit$variable.importance  #importance of independant variable
barplot(fit$variable.importance)

# PLOT DT
fancyRpartPlot(fit)

# Prediksi Testing
prediksi = predict(fit, newdata = data.test, type = "class")
# Confusion Matrix
table(prediksi, data.test$nativeSpeaker)

confusionMatrix(data=prediksi,reference=data.test$nativeSpeaker) #terlihat accuracy

#variabel penting buat dimasukin ke DT
fit$variable.importance
barplot(fit$variable.importance)

