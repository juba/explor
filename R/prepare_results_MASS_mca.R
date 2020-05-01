##' @rdname prepare_results
##' @aliases prepare_results.mca
##' 
##' @seealso \code{\link[MASS]{mca}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @importFrom stats pnorm
##' @export

prepare_results.mca <- function(obj) {

    if (!inherits(obj, "mca")) stop("obj must be of class mca")

    vars <- data.frame(obj$cs)
    names(vars) <- paste0("Dim", names(vars))
    ## Axes names and inertia, and eigenvalues
    axes <- seq_len(ncol(obj$cs))
    eig <- data.frame(dim = axes, percent = 100 * obj$d/(obj$p - 1))
    names(axes) <- paste("Axis", axes, paste0("(", round(eig$percent, 2)),"%)")

    
    ## Variables coordinates
    vars$varname <- gsub("\\..*$", "", rownames(vars))
    vars$modname <- gsub("^.*?\\.", "", rownames(vars))
    vars$Type <- "Active"
    vars$Class <- "Qualitative"

    ## Supplementary variables coordinates
    if (!is.null(obj$supv)) {
        vars.quali.sup <- data.frame(obj$supv)
        names(vars.quali.sup) <- paste0("Dim", names(vars.quali.sup))
        vars.quali.sup$varname <- gsub("\\..*$", "", rownames(vars.quali.sup))
        vars.quali.sup$modname <- gsub("^.*?\\.", "", rownames(vars.quali.sup))
        vars.quali.sup$Type <- "Supplementary"
        vars.quali.sup$Class <- "Qualitative"
        vars <- rbind(vars, vars.quali.sup)
    }
 
    vars <- vars %>% gather(Axis, Coord, starts_with("DimX")) %>%
        mutate(Axis = gsub("DimX", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Missing data
    vars$Count <- NA
    vars$Contrib <- NA
    vars$Cos2 <- NA

    vars <- vars %>%
        rename(Variable = varname, Level = modname) %>%
        as.data.frame()
    
    ## Individuals coordinates
    ind <- data.frame(obj$rs)
    names(ind) <- paste0("Dim", names(ind))
    ind$Name <- rownames(ind)
    ind$Type <- "Active"
    if (!is.null(obj$supi)) {
        tmp_sup <- data.frame(obj$supi)
        names(tmp_sup) <- paste0("Dim", names(tmp_sup))
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        ind <- ind %>% bind_rows(tmp_sup)
    }
    ind <- ind %>% gather(Axis, Coord, starts_with("DimX")) %>%
        mutate(Axis = gsub("DimX", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ind$Contrib <- NA
    ind$Cos2 <- NA
    
    ## Qualitative data for individuals plot color mapping
    quali_data <- eval(as.list(obj$call)$df)
    quali_data$Name <- rownames(quali_data)
    
    return(list(vars = vars, ind = ind, eig = eig, axes = axes, quali_data = quali_data))
    
}
