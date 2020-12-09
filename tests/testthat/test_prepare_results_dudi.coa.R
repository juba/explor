skip_if_not(require("ade4"))
context("prepare_results.coa")

data(bordeaux)
tab <- bordeaux
row_sup <- tab[5,-4]
col_sup <- tab[-5,4]
coa <- ade4::dudi.coa(tab[-5,-4], nf = 5, scannf = FALSE)
coa$supr <- ade4::suprow(coa, row_sup)
coa$supc <- ade4::supcol(coa, col_sup)
iner <- ade4::inertia.dudi(coa, row.inertia = TRUE, col.inertia = TRUE)
res <- prepare_results(coa)

test_that("Eigenvalues are equals", {
  expect_equal(coa$eig / sum(coa$eig) * 100, res$eig$percent)
})

test_that("Levels results are equal", {
  expect_equal(as.vector(round(coa$co[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(coa$li[,2],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "2", "Coord"])
  expect_equal(as.vector(round(abs(iner$col.rel[,2])/100,3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "2", "Cos2"])
  expect_equal(as.vector(round(abs(iner$row.rel[,1])/100,3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "1", "Cos2"])
  expect_equal(as.vector(round(iner$col.abs[,2],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "2", "Contrib"])
  expect_equal(as.vector(round(iner$row.abs[,1],3)),
               res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "1", "Contrib"])
})

test_that("Supplementary levels results are equal", {
  expect_equal(as.vector(round(coa$supc$cosup[,1],3)),
               res$vars[res$vars$Type == "Supplementary level" & res$vars$Position == "Column" & res$vars$Axis == "1", "Coord"])
  expect_equal(as.vector(round(coa$supr$lisup[,2],3)),
               res$vars[res$vars$Type == "Supplementary level" & res$vars$Position == "Row" & res$vars$Axis == "2", "Coord"])
})

