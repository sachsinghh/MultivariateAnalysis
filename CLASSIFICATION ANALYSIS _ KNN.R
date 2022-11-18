#Classification Analysis

#Install Packages and apply library
packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)


# 1. set data
wine = winequality.red
wine = as.data.frame(wine)
wine
str(wine)

# 2. uji asumsi multivariat normal
mshapiro.test(t(wine))

# 3. definisikan kategori u/ kualitas wine
wine$type = as.factor(ifelse(wine$quality <= 5, 'kurang baik', 'cukup baik'))
wine
str(wine)

# KNN

# Count the number of signs of each type
table(wine$type)
# Use kNN to identify the test road signs
data_types <- wine$type
data_pred <- knn(train = wine[-13], test = wine[-13], cl = data_types)
# Create a confusion matrix of the predicted versus actual values
data_actual <- wine$type
table(data_pred, data_actual)
# Compute the accuracy'
mean(data_pred == data_actual)
# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = wine[-13], test = wine[-13], cl = data_types)
mean(k_1 == data_actual)
# Modify the above to set k = 7
k_7 <- knn(train = wine[-13], test = wine[-13], cl = data_types, k = 7)
mean(k_7 == data_actual)
# Use the prob parameter to get the proportion of votes for the winning class
data_pred <- knn(train = wine[-13], test = wine[-13], cl = data_types, k = 7, prob = TRUE)
# Get the "prob" attribute from the predicted classes
data_prob <- attr(data_pred, "prob")
# Examine the first several predictions
head(data_pred)
# Examine the proportion of votes for the winning class
head(data_prob)     #INI YG DIPAKE
cbind(data_prob,data_pred)

str(wine)

