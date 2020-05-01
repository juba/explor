##' @rdname prepare_results
##' @aliases prepare_results.PCA
##'
##' @seealso \code{\link[FactoMineR]{PCA}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @export

prepare_results.PCA <- function(obj) {

    if (!inherits(obj, "PCA")) stop("obj must be of class PCA")
    
    vars <- data.frame(obj$var$coord)
    ## Axes names and inertia
    axes <- seq_len(ncol(obj$var$coord))
    names(axes) <- paste("Axis", axes, paste0("(", head(round(obj$eig[, 2], 2), length(axes)),"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = 1:nrow(obj$eig), percent = obj$eig[,2])
    
    ## Variables data coordinates
    vars$varname <- rownames(vars)
    vars$modname <- ""
    vars$Type <- "Active"
    vars$Class <- "Quantitative"
    
    ## Quantitative supplementary variables coordinates
    if (!is.null(obj$quanti.sup)) {
        vars.quanti.sup <- data.frame(obj$quanti.sup$coord)
        vars.quanti.sup$varname <- rownames(obj$quanti.sup$coord)
        vars.quanti.sup$Type <- "Supplementary"
        vars.quanti.sup$Class <- "Quantitative"
        vars.quanti.sup$modname <- ""
        vars <- rbind(vars, vars.quanti.sup)
    }

    ## Qualitative supplementary variables coordinates
    if (!is.null(obj$quali.sup)) {
        vars.quali.sup <- data.frame(obj$quali.sup$coord)
        quali_varnames <- names(obj$call$quali.sup$quali.sup)
        ## Get the number of levels in quali sup results
        ## For factor : number of levels in original data
        ## Else : number of unique values when ind sup removed
        quali_data <- obj$call$X[, obj$call$quali.sup$numero, drop = FALSE]
        if (!is.null(obj$call$ind.sup)) quali_data <- quali_data[-(obj$call$ind.sup), , drop = FALSE]
        quali_nlevels <- sapply(quali_data, function(v) {
          if (!is.factor(v)) v <- factor(v)
          nlevels(v)
        })
        vars.quali.sup$varname <- rep(quali_varnames, quali_nlevels)
        vars.quali.sup$modname <- rownames(vars.quali.sup)
        vars.quali.sup$Type <- "Supplementary"
        vars.quali.sup$Class <- "Qualitative"
        vars <- rbind(vars, vars.quali.sup)
    }
    
    vars <- vars %>% gather(Axis, Coord, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Contributions
    tmp <- data.frame(obj$var$contrib)
    tmp <- tmp %>% mutate(varname = rownames(tmp),
                          modname = "",
                          Type = "Active",
                          Class = "Quantitative") %>%
        gather(Axis, Contrib, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))
    
    vars <- vars %>% left_join(tmp, by = c("varname", "modname", "Type", "Class", "Axis"), na_matches = "na")
    
    ## Cos2
    tmp <- data.frame(obj$var$cos2)
    tmp$varname <- rownames(tmp)
    tmp$modname <- ""
    tmp$Type <- "Active"
    tmp$Class <- "Quantitative"
    if (!is.null(obj$quanti.sup)) {
        tmp_sup <- data.frame(obj$quanti.sup$cos2)
        tmp_sup$varname <- rownames(tmp_sup)
        tmp_sup$modname <- ""
        tmp_sup$Type <- "Supplementary"
        tmp_sup$Class <- "Quantitative"
        tmp <- tmp %>% bind_rows(tmp_sup)
    }
    if (!is.null(obj$quali.sup)) {
        tmp_sup <- data.frame(obj$quali.sup$cos2)
        tmp_sup$modname <- rownames(tmp_sup)
        tmp_sup$varname <- rep(quali_varnames, quali_nlevels)
        tmp_sup$Type <- "Supplementary"
        tmp_sup$Class <- "Qualitative"
        tmp <- tmp %>% bind_rows(tmp_sup)
    }
    tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Cos2 = round(Cos2, 3))
    
    vars <- vars %>% left_join(tmp, by = c("varname", "modname", "Type", "Class", "Axis"), na_matches = "na")

    ## Cor  
    tmp <- data.frame(obj$var$cor)
    tmp$varname <- rownames(tmp)
    tmp$modname <- ""
    tmp$Type <- "Active"
    tmp$Class <- "Quantitative"  
    if (!is.null(obj$quanti.sup)) {
        tmp_sup <- data.frame(obj$quanti.sup$cor)
        tmp_sup$varname <- rownames(tmp_sup)
        tmp_sup$modname <- ""
        tmp_sup$Type <- "Supplementary"
        tmp_sup$Class <- "Quantitative"    
        tmp <- tmp %>% bind_rows(tmp_sup)
    }
    tmp <- tmp %>% gather(Axis, Cor, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Cor = round(Cor, 3))
    
    vars <- vars %>% left_join(tmp, by = c("varname", "modname", "Type", "Class", "Axis"), na_matches = "na")

    ## V.test for qualitative supplementary variables
    if (!is.null(obj$quali.sup)) {
        ## V.test
        tmp_sup <- data.frame(obj$quali.sup$v.test)
        tmp_sup$modname <- rownames(tmp_sup)
        tmp_sup$varname <- rep(quali_varnames, quali_nlevels)
        tmp_sup$Type <- "Supplementary"
        tmp_sup$Class <- "Qualitative"
        tmp_sup <- tmp_sup %>% gather(Axis, V.test, starts_with("Dim.")) %>%
            mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
                   P.value = round(ifelse(V.test >= 0, 2 * (1 - pnorm(V.test)), 2 * pnorm(V.test)), 3),
                   V.test = round(V.test, 2))
        vars <- vars %>% left_join(tmp_sup, by = c("varname", "modname", "Type", "Class", "Axis"), na_matches = "na")
    }

    vars <- vars %>% rename(Variable = varname, Level = modname)
    
    ## Individuals coordinates
    ind <- data.frame(obj$ind$coord)
    ind$Name <- rownames(ind)
    ind$Type <- "Active"
    if (!is.null(obj$ind.sup)) {
        tmp_sup <- data.frame(obj$ind.sup$coord)
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        ind <- ind %>% bind_rows(tmp_sup)
    }
    ind <- ind %>% gather(Axis, Coord, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Individuals contrib
    tmp <- data.frame(obj$ind$contrib)
    tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Active") %>%
        gather(Axis, Contrib, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))
    
    ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"), na_matches = "na")
    
    ## Individuals Cos2
    tmp <- data.frame(obj$ind$cos2)
    tmp$Name <- rownames(tmp)
    tmp$Type <- "Active"
    if (!is.null(obj$ind.sup)) {
        tmp_sup <- data.frame(obj$ind.sup$cos2)
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        tmp <- tmp %>% bind_rows(tmp_sup)
    }
    tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Cos2 = round(Cos2, 3))
    
    ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"), na_matches = "na")

    ## Qualitative data for individuals plot color mapping
    quali_data <- obj$call$X[,obj$call$quali.sup$numero, drop = FALSE]
    quali_data$Name <- rownames(obj$call$X)
    
    return(list(vars = vars, ind = ind, eig = eig, axes = axes, quali_data = quali_data))
    
}
