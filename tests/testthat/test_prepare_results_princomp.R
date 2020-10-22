context("prepare_results.princomp")

tmp <- USArrests[6:50,]
pca <- princomp(tmp, cor = TRUE)
pca$supi <- predict(pca, USArrests[1:5,])

res <- prepare_results(pca)

test_that("Eigenvalues are equals", {
  expect_equal(unname(round(pca$sdev^2 / sum(pca$sdev^2) * 100,2)), res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(as.vector(round(pca$loadings[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord", drop = TRUE])
})

test_that("Individuals results are equal", {
  expect_equal(as.vector(round(pca$scores[,1],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord", drop = TRUE])
})  
  
test_that("Supplementary individuals results are equal", {
  expect_equal(as.vector(round(pca$supi[,4],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord", drop = TRUE])
})  

