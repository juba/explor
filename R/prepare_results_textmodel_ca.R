##' @rdname prepare_results
##' @aliases prepare_results.textmodel_ca
##' @seealso \code{\link[quanteda.textmodels]{textmodel_ca}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @export

prepare_results.textmodel_ca <- function(obj) {
    
    if (!inherits(obj, "textmodel_ca")) stop("obj must be of class textmodel_ca")

    ## Axes names and inertia
    axes <- seq_len(length(obj$sv))
    percent <- obj$sv / sum(obj$sv) * 100
    names(axes) <- paste("Axis", axes, paste0("(", head(round(percent, 2), length(axes)),"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = axes, percent = percent)
    
    ## Variables coordinates

    ## Columns
    vars <- data.frame(obj$colcoord)
    vars$name <- rownames(vars)
    vars$pos <- "Column"

    ## Rows
    tmp <- data.frame(obj$rowcoord)
    tmp$name <- rownames(tmp)
    tmp$pos <- "Row"
    

    vars <- rbind(vars, tmp)
    vars$Type <- "Active"
    vars$Class <- "Qualitative"
    vars$Contrib <- NA
    vars$Cos2 <- NA
    vars$Count <- NA

    vars <- vars %>% gather(Axis, Coord, starts_with("Dim")) %>%
        mutate(Axis = gsub("Dim", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3)) %>%
        rename(Level = name, Position = pos)

    return(list(vars = vars, eig = eig, axes = axes))
    
}
