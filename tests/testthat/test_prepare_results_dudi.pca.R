skip_if_not(require("ade4"))
context("prepare_results.pca")

data(deug)
d <- deug$tab
sup_var <- d[-(1:10), 8:9]
sup_ind <- d[1:10, -(8:9)]
pca <- ade4::dudi.pca(d[-(1:10), -(8:9)], scale = TRUE, scannf = FALSE, nf = 5)
## Supplementary individuals
pca$supi <- ade4::suprow(pca, sup_ind)
## Supplementary variables
pca$supv <- ade4::supcol(pca, dudi.pca(sup_var, scale = TRUE, scannf = FALSE)$tab)

iner <- ade4::inertia.dudi(pca, row.inertia = TRUE, col.inertia = TRUE)

res <- prepare_results(pca)

test_that("Eigenvalues are equals", {
  expect_equal(pca$eig / sum(pca$eig) * 100, res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(
    as.vector(round(pca$co[, 1], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord", drop = TRUE]
  )
  expect_equal(
    as.vector(round(abs(iner$col.rel[, 2]) / 100, 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Axis == "2", "Cos2", drop = TRUE]
  )
  expect_equal(
    as.vector(round(iner$col.abs[, 3], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Axis == "3", "Contrib", drop = TRUE]
  )
})

test_that("Qualitative supplementary variables results are equal", {
  expect_equal(
    as.vector(round(pca$supv$cosup[, 1], 3)),
    res$vars[res$vars$Type == "Supplementary" &
      res$vars$Class == "Quantitative" & res$vars$Axis == "1", "Coord", drop = TRUE]
  )
})


test_that("Individuals results are equal", {
  expect_equal(
    as.vector(round(pca$li[, 1], 3)),
    data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord", drop = TRUE]
  )
  expect_equal(
    as.vector(round(iner$row.abs[, 3], 3)),
    data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "3", "Contrib", drop = TRUE]
  )
  expect_equal(
    as.vector(round(abs(iner$row.rel[, 5]) / 100, 3)),
    data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "5", "Cos2", drop = TRUE]
  )
})

test_that("Supplementary individuals results are equal", {
  expect_equal(
    as.vector(round(pca$supi$lisup[, 4], 3)),
    data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord", drop = TRUE]
  )
})