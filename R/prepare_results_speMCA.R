##' @rdname prepare_results
##' @aliases prepare_results.speMCA
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[GDAtools]{speMCA}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @importFrom stats pnorm
##' @export

prepare_results.speMCA <- function(obj) {

    if (!inherits(obj, "speMCA")) stop("obj must be of class speMCA")

    vars <- data.frame(obj$var$coord)
    ## Axes names and inertia
    axes <- seq_len(ncol(obj$var$coord))
    names(axes) <- paste("Axis", axes, paste0("(", head(round(obj$eig$rate, 2), length(axes)),"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = 1:length(obj$eig$rate), percent = obj$eig$rate)
    
    ## Variables coordinates
    varnames <- sapply(obj$call$X[,obj$call$quali, drop = FALSE], nlevels)
    varnames <- rep(names(varnames),varnames)
    if (!is.null(obj$call$excl)) varnames <- varnames[-obj$call$excl]
    vars$varname <- varnames
    vars$modname <- rownames(vars)
    vars$Type <- "Active"
    vars$Class <- "Qualitative"

    vars <- vars %>% gather(Axis, Coord, starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Contributions
    tmp <- data.frame(obj$var$contrib)
    tmp <- tmp %>% mutate(modname = rownames(tmp), Type = "Active", Class = "Qualitative") %>%
        gather(Axis, Contrib, starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))
    
    vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Class", "Axis"))
    
    ## Cos2
    tmp <- data.frame(obj$var$cos2)
    tmp$modname <- rownames(tmp)
    tmp$Type <- "Active"
    tmp$Class <- "Qualitative"
    tmp <- tmp %>% gather(Axis, Cos2, starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Cos2 = round(Cos2, 3))
    vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Class", "Axis"))

    vars$modname <- mapply(vars$modname, vars$varname, FUN = function(mod, var) {
        sub(paste0("^",var,"\\."), "", mod)
    }, USE.NAMES = FALSE)
    vars <- vars %>%
        rename(Variable = varname, Level = modname) %>%
        mutate(Count = NA) %>%
        as.data.frame()
    
    ## Variables eta2
    vareta2 <- data.frame(obj$var$eta2)
    vareta2$Variable <- rownames(vareta2)
    vareta2$Type <- "Active"
    vareta2$Class <- "Qualitative"
    vareta2 <- vareta2 %>% gather(Axis, eta2, starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE))
    vareta2$eta2 <- format(vareta2$eta2, scientific = FALSE, nsmall = 3, digits = 0)

    ## Individuals coordinates
    ind <- data.frame(obj$ind$coord)
    ind$Name <- rownames(ind)
    ind$Type <- "Active"
    if (!is.null(obj$supi)) {
        tmp_sup <- data.frame(obj$supi$coord)
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        ind <- ind %>% bind_rows(tmp_sup)
    }
    ind <- ind %>% gather(Axis, Coord, starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Individuals contrib
    tmp <- data.frame(obj$ind$contrib)
    tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Active") %>%
        gather(Axis, Contrib, starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))
    
    ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))

    ## Individuals Cos2
    if (!is.null(obj$supi)) {
        tmp <- data.frame(obj$supi$cos2)
        tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Supplementary") %>%
            gather(Axis, Cos2, starts_with("dim.")) %>%
            mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
                   Cos2 = round(Cos2, 3))
        ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
    } else {
        ind$Cos2 <- NA
    }
    
    ## Qualitative data for individuals plot color mapping
    quali_data <- obj$call$X[,obj$call$quali]
    if (!is.null(obj$quali.sup)) {
        quali_data <- quali_data %>% bind_cols(obj$call$X[,obj$call$quali.sup, drop = FALSE])
    }
    quali_data$Name <- rownames(obj$call$X)
    
    
    return(list(vars = vars, ind = ind, eig = eig, axes = axes, vareta2 = vareta2, quali_data = quali_data))
    
}
