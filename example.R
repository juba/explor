## FactoMineR examples --------------------------------------------------------------

## MCA 1

library(questionr)
library(FactoMineR)
library(dplyr)
library(explor)

data(hdv2003)

d <- hdv2003 %>% 
  select(sexe, qualif, relig, cuisine, bricol, cinema, sport, age, freres.soeurs)
acm <- MCA(d, quali.sup = 6:7, ind.sup = 1:50, quanti.sup = 8:9, graph = FALSE)
explor(acm)

d <- hdv2003 %>% 
  select(sexe, nivetud, qualif, clso, relig, cuisine, bricol)
acm <- MCA(d, graph = FALSE)
detach(package:explor, unload=TRUE); library(explor)
explor(acm)

## MCA 2

library(FactoMineR)
library(explor)

data(hobbies)
mca <- MCA(hobbies[1:1000,c(1:8,21:23)],quali.sup = 9:10, quanti.sup = 11, ind.sup = 1:100, graph = FALSE)
detach(package:explor, unload=TRUE); library(explor)
explor(mca)

## PCA

library(FactoMineR)
library(explor)

data(decathlon)
d <- decathlon[,1:12]
pca <- PCA(d, quanti.sup = 11:12,  ind.sup = 1:4, graph = FALSE, scale.unit = TRUE)
detach(package:explor, unload=TRUE); library(explor)
explor(pca)


## PCA with quali.sup

library(FactoMineR)
library(explor)

data(decathlon)
d <- decathlon
d$sexe <- sample(c("Homme", "Femme"), 41, replace = TRUE)
pca <- PCA(d, quanti.sup = 11:12,  quali.sup = 13:14, ind.sup = 1:4, graph = FALSE, scale.unit = TRUE)
detach(package:explor, unload=TRUE); library(explor)
explor(pca)


## CA

library(FactoMineR)
library(explor)
library(questionr)

data(children)
res.ca <- CA(children, row.sup = 15:18, col.sup = 6:8, graph = FALSE)
detach(package:explor, unload=TRUE); library(explor)
explor(res.ca)



## Ade4 examples --------------------------------------------------------------

## PCA

library(ade4)
data(deug)
d <- deug$tab
sup_var <- d[-(1:10), 8:9]
sup_ind <- d[1:10, -(8:9)]
pca <- dudi.pca(d[-(1:10), -(8:9)], scale = TRUE, scannf = FALSE, nf = 5)
supi <- suprow(pca, sup_ind)
pca$supi <- supi$lisup
supv <- supcol(pca, dudi.pca(sup_var, scale = TRUE, scannf = FALSE)$tab)
pca$supv <- supv$cosup
detach(package:explor, unload=TRUE); library(explor)
explor(pca)

library(ade4)
data(deug)
pca <- dudi.pca(deug$tab, scale = TRUE, scannf = FALSE, nf = 5)
detach(package:explor, unload=TRUE); library(explor)
explor(pca)

## MCA

library(explor)
library(ade4)
data(banque)
d <- banque[-(1:100),-(19:21)]
ind_sup <- banque[1:100, -(19:21)]
var_sup <- banque[-(1:100),19:21]
acm <- dudi.acm(d, scannf = FALSE, nf = 5)
## Supplementary variables
acm$supv <- supcol(acm, dudi.acm(var_sup, scannf = FALSE, nf = 5)$tab)$cosup
colw <- acm$cw*ncol(d)
X <- acm.disjonctif(ind_sup)
X <- t(t(X)/colw) - 1
X <- data.frame(X)
## Supplementary individuals
acm$supi <- suprow(acm, X)$lisup
detach(package:explor, unload=TRUE); library(explor)
explor(acm)

## CA

library(ade4)
library(explor)

data(bordeaux)
tab <- bordeaux
row_sup <- tab[5,-4]
col_sup <- tab[-5,4]
coa <- dudi.coa(tab[-5,-4], nf = 5, scannf = FALSE)
coa$supr <- suprow(coa, row_sup)$lisup
coa$supc <- supcol(coa, col_sup)$cosup
detach(package:explor, unload=TRUE); library(explor)
explor(coa)

data(bordeaux)
coa <- dudi.coa(bordeaux, nf = 5, scannf = FALSE)
detach(package:explor, unload=TRUE); library(explor)
explor(coa)


