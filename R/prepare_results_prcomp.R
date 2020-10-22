##' @rdname prepare_results
##' @aliases prepare_results.prcomp
##' @seealso \code{\link{prcomp}}
##' @import dplyr
##' @importFrom tidyr pivot_longer
##' @importFrom utils head
##' @export

prepare_results.prcomp <- function(obj) {

    if (!inherits(obj, "prcomp")) stop("obj must be of class prcomp")
    
    vars <- obj$rotation
    vars <- data.frame(vars)
    ## Axes names and inertia
    axes <- seq_len(length(obj$sdev))
    percent <- round(obj$sdev^2 / sum(obj$sdev^2) *100, 2)
    names(axes) <- paste("Axis", axes, paste0("(", percent,"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = 1:length(obj$sdev), percent = percent)
    
    ## Variables data coordinates
    vars$varname <- rownames(vars)
    vars$modname <- NA_character_
    vars$Type <- "Active"
    vars$Class <- "Quantitative"
    
    vars <- vars %>% pivot_longer(names_to = "Axis", values_to = "Coord", starts_with("PC")) %>%
        mutate(Axis = gsub("PC", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    vars <- vars %>% rename(Variable = varname, Level = modname)
    vars$Contrib <- NA
    vars$Cos2 <- NA
    
    ## Individuals coordinates
    ind <- data.frame(obj$x)
    ind$Name <- rownames(ind)
    ind$Type <- "Active"
    if (!is.null(obj$supi)) {
        tmp_sup <- data.frame(obj$supi)
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        ind <- ind %>% bind_rows(tmp_sup)
    }
    ind <- ind %>% pivot_longer(names_to = "Axis", values_to = "Coord", starts_with("PC")) %>%
        mutate(Axis = gsub("PC", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))
    ind$Contrib <- NA
    ind$Cos2 <- NA

    return(list(vars = vars, ind = ind, eig = eig, axes = axes))
    
}
