skip_if_not(require("GDAtools"))
context("prepare_results.speMCA")

data(Music)
mca <- GDAtools::speMCA(Music[3:nrow(Music), 1:4], excl = c(3, 6, 9, 12))
mca$supi <- indsup(mca, Music[1:2, 1:4])
mca$supv <- speMCA_varsup(mca, Music[3:nrow(Music), 5:6])

res <- prepare_results(mca)

test_that("Eigenvalues are equals", {
  expect_equal(mca$eig$rate, res$eig$percent)
})

test_that("Variables results are equal", {
  expect_equal(
    as.vector(round(mca$var$coord[, 1], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Axis == "1", "Coord"]
  )
  expect_equal(
    as.vector(round(mca$var$cos2[, 2], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Axis == "2", "Cos2"]
  )
  expect_equal(
    as.vector(round(mca$var$contrib[, 3], 3)),
    res$vars[res$vars$Type == "Active" & res$vars$Axis == "3", "Contrib"]
  )
  expect_equal(
    as.vector(mca$var$eta2[, 5]),
    data.frame(res$vareta2)[res$vareta2$Type == "Active" &
      res$vareta2$Axis == "5", "eta2", drop = TRUE]
  )
})

test_that("Individuals results are equal", {
  expect_equal(
    as.vector(round(mca$ind$coord[, 1], 3)),
    data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "1", "Coord"]
  )
  expect_equal(
    as.vector(round(mca$ind$contrib[, 3], 3)),
    data.frame(res$ind)[res$ind$Type == "Active" & res$ind$Axis == "3", "Contrib"]
  )
})

test_that("Supplementary individuals results are equal", {
  expect_equal(
    as.vector(round(mca$supi$coord[, 4], 3)),
    data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "4", "Coord"]
  )
  expect_equal(
    as.vector(round(mca$supi$cos2[, 2], 3)),
    data.frame(res$ind)[res$ind$Type == "Supplementary" & res$ind$Axis == "2", "Cos2"]
  )
})

test_that("Supplementary variables results are equal", {
  expect_equal(
    round(GDAtools::varsup(mca, Music[3:nrow(Music), 5])$coord[, 1], 3),
    data.frame(res$vars)[res$vars$Type == "Supplementary" & res$vars$Variable == "Classical" & res$vars$Axis == "1", "Coord"]
  )
  expect_equal(
    round(GDAtools::varsup(mca, Music[3:nrow(Music), 6])$cos2[, 2], 3),
    data.frame(res$vars)[res$vars$Type == "Supplementary" & res$vars$Variable == "Gender" & res$vars$Axis == "2", "Cos2"]
  )
})

test_that("Qualitative data are equal", {
  ids <- c("4198", "3704", "2557", "104", "1206")
  expect_equal(
    as.character(res$quali_data$FrenchPop[rownames(res$quali_data) %in% ids]),
    as.character(Music[ids, "FrenchPop"])
  )
})