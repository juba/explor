##' @rdname prepare_results
##' @aliases prepare_results.princomp
##' 
##' @seealso \code{\link{princomp}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @export

prepare_results.princomp <- function(obj) {

    if (!inherits(obj, "princomp")) stop("obj must be of class princomp")
    
    vars <- obj$loadings
    class(vars) <- "matrix"
    vars <- data.frame(vars)
    ## Axes names and inertia
    axes <- seq_len(length(obj$sdev))
    percent <- round(obj$sdev / sum(obj$sdev) *100, 2)
    names(axes) <- paste("Axis", axes, paste0("(", percent,"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = 1:length(obj$sdev), percent = percent)
    
    ## Variables data coordinates
    vars$varname <- rownames(vars)
    vars$modname <- NA_character_
    vars$Type <- "Active"
    vars$Class <- "Quantitative"
    
    vars <- vars %>% gather(Axis, Coord, starts_with("Comp.")) %>%
        mutate(Axis = gsub("Comp.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    vars <- vars %>% rename(Variable = varname, Level = modname)
    vars$Contrib <- NA
    vars$Cos2 <- NA
    
    ## Individuals coordinates
    ind <- data.frame(obj$scores)
    ind$Name <- rownames(ind)
    ind$Type <- "Active"
    if (!is.null(obj$supi)) {
        tmp_sup <- data.frame(obj$supi)
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        ind <- ind %>% bind_rows(tmp_sup)
    }
    ind <- ind %>% gather(Axis, Coord, starts_with("Comp.")) %>%
        mutate(Axis = gsub("Comp.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))
    ind$Contrib <- NA
    ind$Cos2 <- NA

    return(list(vars = vars, ind = ind, eig = eig, axes = axes))
    
}
