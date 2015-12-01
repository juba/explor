library(FactoMineR)
context("prepare_results.CA")

data(children)
ca <- CA(children[,1:5], row.sup = 1:3, 
           col.sup = 5, graph = FALSE)
res <- prepare_results(ca)

test_that("Eigenvalues are equals", {
  expect_equal(ca$eig$`percentage of variance`, res$eig$percent)
})

test_that("Levels results are equal", {
  expect_equal(as.vector(round(ca$col$coord[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(ca$row$coord[,2],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "2", "Coord"])
  expect_equal(as.vector(round(ca$col$cos2[,3],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "3", "Cos2"])
  expect_equal(as.vector(round(ca$row$cos2[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "1", "Cos2"])
  expect_equal(as.vector(round(ca$col$contrib[,2],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "2", "Contrib"])
  expect_equal(as.vector(round(ca$row$contrib[,3],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "3", "Contrib"])
})

test_that("Supplementary levels results are equal", {
  expect_equal(as.vector(round(ca$col.sup$coord[,1],3)),
               res$vars[res$vars$Type == "Supplementary" & res$vars$Position == "Column" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(ca$row.sup$coord[,2],3)),
               res$vars[res$vars$Type == "Supplementary" & res$vars$Position == "Row" & res$vars$Axis == "2", "Coord"])
  expect_equal(as.vector(round(ca$col.sup$cos2[,3],3)),
               res$vars[res$vars$Type == "Supplementary" & res$vars$Position == "Column" & res$vars$Axis == "3", "Cos2"])
  expect_equal(as.vector(round(ca$row.sup$cos2[,1],3)),
               res$vars[res$vars$Type == "Supplementary" & res$vars$Position == "Row" & res$vars$Axis == "1", "Cos2"])
})
