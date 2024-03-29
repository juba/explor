skip_if_not(require("ade4"))
context("prepare_results.acm")

data(banque)
banque <- banque[1:100, 1:10]
d <- banque[-(1:10), -(9:10)]
ind_sup <- banque[1:10, -(9:10)]
var_sup <- banque[-(1:10), 9:10]
mca <- ade4::dudi.acm(d, scannf = FALSE, nf = 5)
## Supplementary variables
mca$supv <- ade4::supcol(mca, dudi.acm(var_sup, scannf = FALSE, nf = 5)$tab)
## Supplementary individuals
colw <- mca$cw * ncol(d)
mca$supi <- ade4::suprow(mca, ind_sup)
iner <- ade4::inertia.dudi(mca, row.inertia = TRUE, col.inertia = TRUE)

res <- prepare_results(mca)

test_that("Eigenvalues are equals", {
  expect_equal(mca$eig / sum(mca$eig) * 100, res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(
    as.vector(round(mca$co[, 1], 3)),
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
  expect_equal(
    as.vector(mca$cr[, 5]),
    data.frame(res$vareta2)[res$vareta2$Type == "Active" &
      res$vareta2$Axis == "5", "eta2", drop = TRUE]
  )
})

test_that("Qualitative supplementary variables results are equal", {
  expect_equal(
    as.vector(round(mca$supv$cosup[, 1], 3)),
    res$vars[res$vars$Type == "Supplementary" &
      res$vars$Class == "Qualitative" & res$vars$Axis == "1", "Coord", drop = TRUE]
  )
})


test_that("Individuals results are equal", {
  expect_equal(
    as.vector(round(mca$li[, 1], 3)),
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
    as.vector(round(mca$supi$lisup[, 4], 3)),
    data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord", drop = TRUE]
  )
})

test_that("Qualitative data are equal", {
  ids <- c("11", "20", "45", "87", "89", "99", "100")
  tmp <- res$quali_data
  rownames(tmp) <- tmp$Name
  expect_equal(
    as.character(tmp[ids, "duree"]),
    as.character(banque[ids, "duree"])
  )
})