packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class", "car")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

data <- AirQualityUCI
data <- as.data.frame(data)
head(data)

sum(is.na(data))

databaru=data[-1:-2]
head(databaru)

colnames(databaru)
str(databaru)
databaru <- rename(databaru,
                   x1 = CO.GT.,
                   x2 = PT08.S1.CO.,
                   x3 = NMHC.GT.,
                   x4 = C6H6.GT.,
                   x5 = PT08.S2.NMHC.,
                   x6 = NOx.GT.,
                   x7 = PT08.S3.NOx.,
                   x8 = NO2.GT.,
                   x9 = PT08.S4.NO2.,
                   x10 = PT08.S5.O3.)
head(databaru)

mmr <- lm(cbind(T, RH, AH) ~ x1 + x2 + x3 + x4 
          + x5 + x6 + x7 + x8 + x9 + x10	, data = databaru)
summary(mmr)

#residual
head(resid(mmr))

head(fitted(mmr))

coef(mmr)

sigma(mmr)

vcov(mmr)

library(car)
Anova(mmr)

mmr2 <- update(mmr, . ~ . - x1 - x7)
anova(mmr, mmr2)

lh.out <- linearHypothesis(mmr, hypothesis.matrix = c("x1 = 0", "x7 = 0"))
lh.out

#uji hipotesis scr manual

E <- lh.out$SSPE
E

H <- lh.out$SSPH
H

Lambda <- det(E)/det(E + H); Lambda

EinvH.eigen <- eigen(inv(E) %*% H)
roy.stat <- EinvH.eigen$values[1]; roy.stat
theta <- roy.stat/(1+roy.stat); theta

pillai.stat <- sum(diag(inv(E + H) %*% H)); pillai.stat
sum(EinvH.eigen$values / (1 + EinvH.eigen$values))

lawhot.stat <- sum(diag(inv(E) %*% H)); lawhot.stat
sum(EinvH.eigen$values)
(28/4)*sum(EinvH.eigen$values)

