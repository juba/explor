library(ade4)
context("prepare_results.pca")

data(deug)
d <- deug$tab
sup_var <- d[-(1:10), 8:9]
sup_ind <- d[1:10, -(8:9)]
pca <- dudi.pca(d[-(1:10), -(8:9)], scale = TRUE, scannf = FALSE, nf = 5)
## Supplementary individuals
supi <- suprow(pca, sup_ind)
pca$supi <- supi$lisup
## Supplementary variables
supv <- supcol(pca, dudi.pca(sup_var, scale = TRUE, scannf = FALSE)$tab)
pca$supv <- supv$cosup

iner <- inertia.dudi(pca, row.inertia = TRUE, col.inertia = TRUE)

res <- prepare_results(pca)

test_that("Eigenvalues are equals", {
  expect_equal(pca$eig / sum(pca$eig) * 100, res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(as.vector(round(pca$co[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(iner$col.rel[,2]/10000,3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "2", "Cos2"])
  expect_equal(as.vector(round(iner$col.abs[,3]/100,3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "3", "Contrib"])
})

test_that("Qualitative supplementary variables results are equal", {
  expect_equal(as.vector(round(pca$supv[,1],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Quantitative" & res$vars$Axis == "1", "Coord"])
})


test_that("Individuals results are equal", {
  expect_equal(as.vector(round(pca$li[,1],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord"])
  expect_equal(as.vector(round(iner$row.abs[,3]/100,3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "3", "Contrib"])
  expect_equal(as.vector(round(iner$row.rel[,5]/10000,3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "5", "Cos2"])
})  
  
test_that("Supplementary individuals results are equal", {
  expect_equal(as.vector(round(pca$supi[,4],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord"])
})  

