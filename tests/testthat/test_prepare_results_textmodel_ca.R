skip_if_not(require("quanteda.textmodels"))
skip_if_not(require("quanteda"))
context("prepare_results.textmodel_ca")

tok <- quanteda::tokens(data_corpus_irishbudget2010)
dfmat <- quanteda::dfm(tok)
dfmat <- quanteda::dfm_trim(dfmat, min_termfreq = 30)
ca <- quanteda.textmodels::textmodel_ca(dfmat, nd = 7)
res <- prepare_results(ca)

test_that("Eigenvalues are equals", {
  percent <- ca$sv / sum(ca$sv) * 100
  expect_equal(percent, res$eig$percent)
})

test_that("Levels results are equal", {
  expect_equal(
    as.vector(round(ca$colcoord[, 1], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Position == "Column" & res$vars$Axis == "1", "Coord", drop = TRUE]
  )
  expect_equal(
    as.vector(round(ca$rowcoord[, 2], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Position == "Row" & res$vars$Axis == "2", "Coord", drop = TRUE]
  )
})