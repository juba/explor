library(FactoMineR)
context("prepare_results.PCA")

data(decathlon)
pca <- PCA(decathlon, ind.sup = 1:5, quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
res <- prepare_results(pca)

test_that("Eigenvalues are equals", {
  expect_equal(pca$eig$`percentage of variance`, res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(as.vector(round(pca$var$coord[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(pca$var$cos2[,2],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "2", "Cos2"])
  expect_equal(as.vector(round(pca$var$cor[,3],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "3", "Cor"])
  expect_equal(as.vector(round(pca$var$contrib[,5],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "5", "Contrib"])
})

test_that("Quantitative supplementary variables results are equal", {
  expect_equal(as.vector(round(pca$quanti.sup$coord[,1],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Quantitative" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(pca$quanti.sup$cor[,3],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Quantitative" & res$vars$Axis == "3", "Cor"])
  expect_equal(as.vector(round(pca$quanti.sup$cos2[,5],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Quantitative" & res$vars$Axis == "5", "Cos2"])
})

test_that("Individuals results are equal", {
  expect_equal(as.vector(round(pca$ind$coord[,1],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord"])
  expect_equal(as.vector(round(pca$ind$contrib[,3],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "3", "Contrib"])
  expect_equal(as.vector(round(pca$ind$cos2[,5],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "5", "Cos2"])
})  
  
test_that("Supplementary individuals results are equal", {
  expect_equal(as.vector(round(pca$ind.sup$coord[,4],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord"])
  expect_equal(as.vector(round(pca$ind.sup$cos2[,2],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "2", "Cos2"])
})  

