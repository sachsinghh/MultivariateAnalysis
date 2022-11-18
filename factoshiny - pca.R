haireyecolor = array(c(36,66,16,4,9,34,7,64,5,29,7,5,2,14,7,8),
                     dim = c(4,4),
                     dimnames = list(Hair=c('black','brown','red','blond'),
                                     Eye = c('brown','blue','hazel','green')))
haireyecolor
summary(haireyecolor)

install.packages("Factoshiny")
library(Factoshiny)
library(FactoMineR)

res = Factoshiny(haireyecolor)

res.ca = CA(haireyecolor, ncp = 6, col.sup = 4, row.sup = 3, graph = FALSE)
ellipseCA(res.ca, title = "CA of HAIREYECOLOR")


res.PCA = PCA(haireyecolor, graph = FALSE)
res.PCA
