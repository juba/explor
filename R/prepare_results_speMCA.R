##' @rdname prepare_results
##' @aliases prepare_results.speMCA
##' @seealso \code{\link[GDAtools]{speMCA}}
##' @import dplyr
##' @importFrom tidyr pivot_longer
##' @importFrom utils head
##' @importFrom stats pnorm
##' @export

prepare_results.speMCA <- function(obj) {

    if (!inherits(obj, "speMCA")) stop("obj must be of class speMCA")

    ## Extract variable names from results row names
    extract_var <- function(df) {
        gsub("(.*)____.*?$", "\\1", rownames(df))
    }
    ## Extract level names from results row names
    extract_mod <- function(df) {
        gsub(".*____(.*?)$", "\\1", rownames(df))
    }

    vars <- data.frame(obj$var$coord)
    ## Axes names and inertia
    axes <- seq_len(ncol(obj$var$coord))
    names(axes) <- paste("Axis", axes, paste0("(", head(round(obj$eig$rate, 2), length(axes)),"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = seq_len(length(obj$eig$rate)), percent = obj$eig$rate)

    ## Variables coordinates
    varnames <- sapply(obj$call$X[,obj$call$quali, drop = FALSE], nlevels)
    varnames <- rep(names(varnames),varnames)
    if (!is.null(obj$call$excl)) varnames <- varnames[-obj$call$excl]
    vars$varname <- varnames
    vars$modname <- rownames(vars)
    vars$Type <- "Active"
    vars$Class <- "Qualitative"
    if (!is.null(obj$supv)) {
        tmp_sup <- data.frame(obj$supv$coord)
        tmp_sup$varname <- extract_var(tmp_sup)
        tmp_sup$modname <- extract_mod(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        tmp_sup$Class <- "Qualitative"
        vars <- vars %>% bind_rows(tmp_sup)
    }

    vars <- vars %>% pivot_longer(names_to = "Axis", values_to = "Coord", starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Variables contrib
    tmp <- data.frame(obj$var$contrib)
    tmp <- tmp %>% mutate(modname = rownames(tmp), Type = "Active", Class = "Qualitative") %>%
        pivot_longer(names_to = "Axis", values_to = "Contrib", starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))

    vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Class", "Axis"))

    ## Variables cos2
    tmp <- data.frame(obj$var$cos2)
    tmp$modname <- rownames(tmp)
    tmp$Type <- "Active"
    tmp$Class <- "Qualitative"
    if (!is.null(obj$supv)) {
        tmp_sup <- data.frame(obj$supv$cos2)
        tmp_sup$modname <- extract_mod(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        tmp_sup$Class <- "Qualitative"
        tmp <- tmp %>% bind_rows(tmp_sup)
    }
    tmp <- tmp %>%
        pivot_longer(names_to = "Axis", values_to = "Cos2", starts_with("dim.")) %>%
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
    vareta2 <- vareta2 %>%
        pivot_longer(names_to = "Axis", values_to = "eta2", starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE))

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
    ind <- ind %>%
        pivot_longer(names_to = "Axis", values_to = "Coord", starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Individuals contrib
    tmp <- data.frame(obj$ind$contrib)
    tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Active") %>%
        pivot_longer(names_to = "Axis", values_to = "Contrib", starts_with("dim.")) %>%
        mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))

    ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))

    ## Individuals Cos2
    if (!is.null(obj$supi)) {
        tmp <- data.frame(obj$supi$cos2)
        tmp <- tmp %>%
            mutate(Name = rownames(tmp), Type = "Supplementary") %>%
            pivot_longer(names_to = "Axis", values_to = "Cos2", starts_with("dim.")) %>%
            mutate(Axis = gsub("dim.", "", Axis, fixed = TRUE),
                   Cos2 = round(Cos2, 3))
        ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
    } else {
        ind$Cos2 <- NA
    }

    ## Qualitative data for individuals plot color mapping
    quali_data <- obj$call$X[,obj$call$quali]
    if (!is.null(obj$quali.sup)) {
        quali_data <- quali_data %>%
            bind_cols(obj$call$X[,obj$call$quali.sup, drop = FALSE])
    }
    quali_data$Name <- rownames(obj$call$X)


    return(
        list(vars = vars, ind = ind, eig = eig, axes = axes, vareta2 = vareta2, quali_data = quali_data)
    )

}
