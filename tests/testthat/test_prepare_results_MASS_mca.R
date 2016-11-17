library(MASS)
context("prepare_results.mca")

data(farms)
mca <- MASS::mca(farms[4:20, 2:4], nf = 5)
supi_df <- farms[1:3, 2:4]
supi <- predict(mca, supi_df, type="row")
rownames(supi) <- rownames(supi_df)
mca$supi <- supi
mca$supv <- predict(mca, farms[4:20, 1, drop=FALSE], type="factor")

res <- prepare_results(mca)

test_that("Eigenvalues are equals", {
  expect_equal(100* mca$d/(mca$p - 1), res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(as.vector(round(mca$cs[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord"])
})

test_that("Supplementary variables results are equal", {
  expect_equal(as.vector(round(mca$supv[,4],3)),
               data.frame(res$var)[res$var$Type == "Supplementary" & res$var$Axis == "4", "Coord"])
})  


test_that("Individuals results are equal", {
  expect_equal(as.vector(round(mca$rs[,1],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord"])
})  
  
test_that("Supplementary individuals results are equal", {
  expect_equal(as.vector(round(mca$supi[,4],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord"])
})  

test_that("Qualitative data are equal", {
    ids <- c("5", "11", "14", "16", "20")
    data <- eval(as.list(mca$call)$df)
    data$Name <- rownames(data)
    expect_equal(as.character(res$quali_data$Use[res$quali_data$Name %in% ids]),
                 as.character(data[ids, "Use"]))
})

