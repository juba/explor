library(FactoMineR)
context("prepare_results.MCA")

data(hobbies)
mca <- MCA(hobbies[1:1000,c(1:8,21:23)],quali.sup = 9:10, 
           quanti.sup = 11, ind.sup = 1:100, graph = FALSE)
res <- prepare_results(mca)

test_that("Eigenvalues are equals", {
  expect_equal(mca$eig$`percentage of variance`, res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(as.vector(round(mca$var$coord[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(mca$var$cos2[,2],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "2", "Cos2"])
  expect_equal(as.vector(round(mca$var$contrib[,3],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Axis == "3", "Contrib"])
  expect_equal(as.vector(format(mca$var$eta2[,5], scientific = FALSE, nsmall = 3, digits = 0)),
               data.frame(res$vareta2)[res$vareta2$Type == "Active" & 
                                         res$vareta2$Axis == "5", "eta2", drop = TRUE])
})

test_that("Qualitative supplementary variables results are equal", {
  expect_equal(as.vector(round(mca$quali.sup$coord[,1],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Qualitative" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(mca$quali.sup$cos2[,2],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Qualitative" & res$vars$Axis == "2", "Cos2"])
  expect_equal(as.vector(round(mca$quali.sup$v.test[,3],2)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Qualitative" & res$vars$Axis == "3", "V.test"])
  expect_equal(as.vector(format(mca$quali.sup$eta2[,2], scientific = FALSE, nsmall = 3, digits = 0)),
               data.frame(res$vareta2)[res$vareta2$Type == "Supplementary" & 
                                         res$vareta2$Class == "Qualitative" &
                                         res$vareta2$Axis == "2", "eta2", drop = TRUE])
})

test_that("Quantitative supplementary variables results are equal", {
  expect_equal(as.vector(round(mca$quanti.sup$coord[,1],3)),
               res$vars[res$vars$Type == "Supplementary" & 
                          res$vars$Class == "Quantitative" & res$vars$Axis == "1", "Coord"])
})

test_that("Individuals results are equal", {
  expect_equal(as.vector(round(mca$ind$coord[,1],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord"])
  expect_equal(as.vector(round(mca$ind$contrib[,3],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "3", "Contrib"])
  expect_equal(as.vector(round(mca$ind$cos2[,5],3)),
               data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "5", "Cos2"])
})  
  
test_that("Supplementary individuals results are equal", {
  expect_equal(as.vector(round(mca$ind.sup$coord[,4],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord"])
  expect_equal(as.vector(round(mca$ind.sup$cos2[,2],3)),
               data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "2", "Cos2"])
})  

test_that("Qualitative data are equal", {
  ids <- c("11000210", "11009110", "21052910", "21063810", "22007510")
  expect_equal(as.character(res$quali_data$`Marital status`[res$quali_data$Name %in% ids]),
               as.character(hobbies[ids, "Marital status"]))
})

