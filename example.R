library(questionr)
library(FactoMineR)
library(dplyr)
library(explor)

data(hdv2003)

d <- hdv2003 %>% 
  select(sexe, nivetud, qualif, clso, relig, cuisine, bricol, cinema, sport, age, freres.soeurs)
acm <- MCA(d, quali.sup = 8:9, ind.sup = 1:50, quanti.sup = 10:11, graph = FALSE)
explor(acm)

d <- hdv2003 %>% 
  select(sexe, nivetud, qualif, clso, relig, cuisine, bricol)
acm <- MCA(d, graph = FALSE)
explor(acm)


library(questionr)
library(FactoMineR)
library(dplyr)
library(explor)

data(rp99)
d <- rp99
rownames(d) <- rp99$nom
d$popcl <- cut(d$pop.tot, breaks = c(50,500,1000,5000,10000,100000,125000))
d <- d %>% select(agric, artis, cadres, interm, empl, ouvr, retr, pop15)
acp <- PCA(d, scale.unit = TRUE, quanti.sup = 8, ind.sup = 1:20)
explor(acp)
