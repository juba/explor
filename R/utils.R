#' Compute supplementary variables data for a GDAtools::speMCA result
#'
#' @param mca result object from speMCA.
#' @param df data frame with the supplementary variables data. Must have the
#' same number of rows than the data used with speMCA.
#'
#' @return
#' A list of results suitable to be added as a `supv` element to the `mca`
#' object.
#'
#' @seealso
#' \code{\link[GDAtools]{speMCA}}, \code{\link[GDAtools]{varsup}}
#' @export

speMCA_varsup <- function(mca, df) {
    if (!is.data.frame(df)) stop("df must be a data frame")
    res <- lapply(names(df), function(name) {
        l <- GDAtools::varsup(mca, df[, name])
        l <- lapply(l, function(x) {
            if (is.data.frame(x)) {
                rownames(x) <- paste(name, rownames(x), sep = "____")
            }
            x
        })
        l
    })
    Reduce(function(acc, cur) {
        for (name in names(acc)) {
            if (name == "weight") next
            acc[[name]] <- dplyr::bind_rows(
                data.frame(acc[[name]]),
                data.frame(cur[[name]])
            )
        }
        acc
    }, res)
}
