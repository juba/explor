library(questionr)
library(FactoMineR)
library(dplyr)

data(hdv2003)

d <- hdv2003 %>% select(sexe, nivetud, qualif, clso, relig, cuisine, bricol, cinema, sport)
acm <- MCA(d, quali.sup = 8:9, ind.sup = 1:50)
imca(acm)

acm <- MCA(d, ind.sup = 1:50)
imca(acm)
