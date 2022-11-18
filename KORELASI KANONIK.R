#Canonical Correlation Analysis

packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class","CCA","vegan","candisc")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

#prep data
library(CCA)
air = fuel_cons
head(air)
databaru=air[-1]
airfinal = na.omit(databaru)
head(airfinal)
airfinal %>% View()

# mendefinisi variabel dependen dan independen
x = airfinal[1:11]
y = airfinal[12:14]
head(x)
head(y)
str(fuel_cons)
#korelasi kanonik
correl = matcor(x,y)
correl
img.matcor(correl, type = 1)
img.matcor(correl, type =2)

cc1 = cancor(x,y)
cc2 = cc(x,y)
cc1 %>% View()
cc2

par(mfrow =c(1,2))
barplot(cc1$cancor, main = "Canonical Correlations for 'cancor()'", col = "grey")
barplot(cc1$cancor, main = "Canonical Correlations for 'cancor()'", col = "grey")
cc1$coef
plt.cc(cc2, var.label=TRUE)

#analisis korelasi kanonik
ccan = candisc::cancor(x,y)
summary(ccan)

#korelasi kanonik
res.cc = cc(x,y)
res.cc

#correlation plot
plot(res.cc$cor, type = "b")

