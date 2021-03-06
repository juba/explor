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
explor(acm)

## MCA 2

library(FactoMineR)
library(explor)

data(hobbies)
mca <- MCA(hobbies[1:1000, c(1:8, 21:23)], quali.sup = 9:10, quanti.sup = 11, ind.sup = 1:100, graph = FALSE)
# mca <- MCA(hobbies[1:1000,c(1:8,21:22)],quali.sup = 9:10, ind.sup = 1:100, graph = FALSE)
explor(mca)

## PCA

library(FactoMineR)
library(explor)

data(decathlon)
d <- decathlon[, 1:12]
pca <- PCA(d, quanti.sup = 11:12, ind.sup = 1:4, graph = FALSE, scale.unit = TRUE)
explor(pca)


## PCA with quali.sup

library(FactoMineR)
library(explor)

data(decathlon)
d <- decathlon
d$sexe <- sample(c("Homme", "Femme"), 41, replace = TRUE)
pca <- PCA(d, quanti.sup = 11:12, quali.sup = 13:14, ind.sup = 1:4, graph = FALSE, scale.unit = TRUE)
explor(pca)


## CA

library(FactoMineR)
library(explor)
library(questionr)

data(children)
res.ca <- CA(children[1:14, 1:5], graph = FALSE)
explor(res.ca)


data(children)
res.ca <- CA(children, row.sup = 15:18, col.sup = 6:8, graph = FALSE)
explor(res.ca)

data(children)
tmp <- children
tmp[, 9] <- factor(sample(c("red", "blue", "green"), 18, replace = TRUE))
res.ca <- CA(tmp, row.sup = 15:18, col.sup = 6:8, quali.sup = 9, graph = FALSE)
explor(res.ca)




## Ade4 examples --------------------------------------------------------------

## PCA

library(ade4)
data(deug)
d <- deug$tab
sup_var <- d[-(1:10), 8:9]
sup_ind <- d[1:10, -(8:9)]
pca <- dudi.pca(d[-(1:10), -(8:9)], scale = TRUE, scannf = FALSE, nf = 5)
pca$supi <- suprow(pca, sup_ind)
pca$supv <- supcol(pca, dudi.pca(sup_var, scale = TRUE, scannf = FALSE)$tab)
explor(pca)

library(ade4)
data(deug)
pca <- dudi.pca(deug$tab, scale = TRUE, scannf = FALSE, nf = 5)
explor(pca)

## MCA

library(explor)
library(ade4)
data(banque)
d <- banque[-(1:100), -(19:21)]
ind_sup <- banque[1:100, -(19:21)]
var_sup <- banque[-(1:100), 19:21]
acm <- dudi.acm(d, scannf = FALSE, nf = 5)
## Supplementary variables
acm$supv <- supcol(acm, dudi.acm(var_sup, scannf = FALSE, nf = 5)$tab)
## Supplementary individuals
acm$supi <- suprow(acm, ind_sup)
explor(acm)

## CA

library(ade4)
library(explor)

data(bordeaux)
tab <- bordeaux
row_sup <- tab[5, -4]
col_sup <- tab[-5, 4]
coa <- dudi.coa(tab[-5, -4], nf = 5, scannf = FALSE)
coa$supr <- suprow(coa, row_sup)
coa$supc <- supcol(coa, col_sup)
explor(coa)

data(bordeaux)
coa <- dudi.coa(bordeaux, nf = 5, scannf = FALSE)
explor(coa)


## GDAtools examples ----------------------------------------

## speMCA

library(explor)
library(GDAtools)
data(Music)
mca <- speMCA(Music[, 1:5], excl = c(3, 6, 9, 12, 15))
explor(mca)


## speMCA with indsup and varsup
library(explor)
library(GDAtools)
data(Music)
getindexcat(Music[, 1:4])
mca <- speMCA(Music[3:nrow(Music), 1:4], excl = c(3, 6, 9, 12))
mca$supi <- indsup(mca, Music[1:2, 1:4])
mca$supv <- speMCA_varsup(mca, Music[3:nrow(Music), 5, drop = FALSE])
explor(mca)

## speMCA with varsup with missing values
library(questionr)
library(GDAtools)
library(explor)
data(hdv2003)
acm <- speMCA(hdv2003[, c("sexe", "relig", "occup", "hard.rock")])
GDAtools::varsup(acm, hdv2003$qualif)
speMCA_varsup(acm, hdv2003[,c("qualif", "bricol")])


## MASS examples ---------------------------------------------

## mca

library(MASS)
library(explor)
tmp <- farms[4:20, 2:4]
mca <- MASS::mca(tmp, nf = 11)
supi_df <- farms[1:3, 2:4]
supi <- predict(mca, supi_df, type = "row")
rownames(supi) <- rownames(supi_df)
mca$supi <- supi
mca$supv <- predict(mca, farms[4:20, 1, drop = FALSE], type = "factor")
explor(mca)


## Base examples ---------------------------------------------

# princomp

tmp <- USArrests
pca <- princomp(tmp, cor = FALSE)
explor(pca)

tmp <- USArrests[6:50, ]
pca <- princomp(tmp, cor = TRUE)
pca$supi <- predict(pca, USArrests[1:5, ])
explor(pca)

# prcomp

tmp <- USArrests
pca <- prcomp(tmp, scale. = FALSE)
explor(pca)

tmp <- USArrests[6:50, ]
pca <- prcomp(tmp, scale. = TRUE)
pca$supi <- predict(pca, USArrests[1:5, ])
explor(pca)


## textmodel_ca --------------------------------------------

library(quanteda.textmodels)
dfmat <- quanteda::dfm(data_corpus_irishbudget2010)
tmod <- textmodel_ca(dfmat, nd = 7)
explor(tmod)