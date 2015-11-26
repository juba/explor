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


library(FactoMineR)
library(explor)

data(decathlon)
d <- decathlon[,1:12]
pca <- PCA(d, quanti.sup = 11:12, graph = FALSE)
explor(pca)

data(decathlon)
d <- decathlon[,1:12]
pca <- PCA(d, quanti.sup = 11:12, graph = FALSE, scale.unit = FALSE)
explor(pca)


data(hobbies)
mca <- MCA(hobbies[1:1000,c(1:8,21:23)],quali.sup = 9:10, quanti.sup = 11, ind.sup = 1:100)
explor(mca)
