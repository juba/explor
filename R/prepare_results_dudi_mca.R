##' @rdname prepare_results
##' @aliases prepare_results.acm
##' 
##' @seealso \code{\link[ade4]{dudi.acm}}
##' @import dplyr
##' @importFrom tidyr pivot_longer
##' @importFrom tidyr unite
##' @importFrom utils head
##' @export

prepare_results.acm <- function(obj) {

    if (!inherits(obj, "acm") || !inherits(obj, "dudi")) stop("obj must be of class dudi and acm")

    if (!requireNamespace("ade4", quietly = TRUE)) {
        stop("the ade4 package is needed for this function to work.")
    }

    ## Extract variable names from results row names
    extract_var <- function(df) {
        gsub("(.*)\\..*?$","\\1", rownames(df))
    }
    ## Extract level names from results row names  
    extract_mod <- function(df) {
        gsub(".*\\.(.*?)$","\\1", rownames(df))
    }
    
    
    vars <- obj$co
    ## Axes names and inertia
    axes <- seq_len(ncol(vars))
    eig <- obj$eig / sum(obj$eig) * 100
    names(axes) <- paste("Axis", axes, paste0("(", head(round(eig, 2), length(axes)),"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = 1:length(eig), percent = eig)
    ## Inertia
    inertia <- ade4::inertia.dudi(obj, row.inertia = TRUE, col.inertia = TRUE)
    
    ## Variables coordinates
    vars$varname <- extract_var(vars)
    vars$modname <- extract_mod(vars)
    vars$Type <- "Active"
    vars$Class <- "Qualitative"
    vars$Count <- NA
    
    ## Supplementary variables coordinates
    if (!is.null(obj$supv)) {
        vars.quali.sup <- data.frame(obj$supv$cosup)
        vars.quali.sup$varname <- extract_var(vars.quali.sup)
        vars.quali.sup$modname <- extract_mod(vars.quali.sup)
        vars.quali.sup$Type <- "Supplementary"
        vars.quali.sup$Class <- "Qualitative"
        vars.quali.sup$Count <- NA
        vars <- rbind(vars, vars.quali.sup)
    }

    vars <- vars %>% pivot_longer(names_to = "Axis", values_to = "Coord", starts_with("Comp")) %>%
        mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Contributions
    tmp <- inertia$col.abs
    tmp <- tmp %>% mutate(varname = extract_var(tmp),
                          modname = extract_mod(tmp), 
                          Type = "Active", Class = "Qualitative") %>%
        pivot_longer(names_to = "Axis", values_to = "Contrib", starts_with("Axis")) %>%
        mutate(Axis = gsub("^Axis([0-9]+)$", "\\1", Axis),
               Contrib = round(Contrib, 3))
    
    vars <- vars %>% left_join(tmp, by = c("varname", "modname", "Type", "Class", "Axis"))
    
    ## Cos2
    tmp <- abs(inertia$col.rel) / 100
    tmp <- tmp %>% mutate(varname = extract_var(tmp),
                          modname = extract_mod(tmp), 
                          Type = "Active", Class = "Qualitative")

    tmp <- tmp %>% pivot_longer(names_to = "Axis", values_to = "Cos2", starts_with("Axis")) %>%
        mutate(Axis = gsub("Axis", "", Axis, fixed = TRUE),
               Cos2 = round(Cos2, 3))
    
    vars <- vars %>% left_join(tmp, by = c("varname", "modname", "Type", "Class", "Axis"))
    
    vars <- vars %>% 
        rename(Variable = varname, Level = modname)
    
    ## Variables eta2
    vareta2 <- obj$cr
    vareta2$Variable <- rownames(vareta2)
    vareta2$Type <- "Active"
    vareta2$Class <- "Qualitative"

    vareta2 <- vareta2 %>% pivot_longer(names_to = "Axis", values_to = "eta2", starts_with("RS")) %>%
        mutate(Axis = gsub("RS", "", Axis, fixed = TRUE))
    vareta2$eta2 <- format(vareta2$eta2, scientific = FALSE, nsmall = 3, digits = 0)

    ## Individuals coordinates
    ind <- obj$li
    ind$Name <- rownames(ind)
    ind$Type <- "Active"
    if (!is.null(obj$supi)) {
        tmp_sup <- data.frame(obj$supi$lisup)
        tmp_sup$Name <- rownames(tmp_sup)
        tmp_sup$Type <- "Supplementary"
        ind <- ind %>% bind_rows(tmp_sup)
    }
    ind <- ind %>% pivot_longer(names_to = "Axis", values_to = "Coord", starts_with("Axis")) %>%
        mutate(Axis = gsub("Axis", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Individuals contrib
    tmp <- inertia$row.abs
    tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Active") %>%
      pivot_longer(names_to = "Axis", values_to = "Contrib", starts_with("Axis")) %>%
      mutate(Axis = gsub("^Axis([0-9]+)$", "\\1", Axis),
             Contrib = round(Contrib, 3))
    
    ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
    
    ## Individuals Cos2
    tmp <- abs(inertia$row.rel) / 100
    tmp$Name <- rownames(tmp)
    tmp$Type <- "Active"
    tmp <- tmp %>% 
        pivot_longer(names_to = "Axis", values_to = "Cos2", starts_with("Axis")) %>%
        mutate(Axis = gsub("Axis", "", Axis, fixed = TRUE),
               Cos2 = round(Cos2, 3))

    ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
    
    ## Qualitative data for individuals plot color mapping
    tmp <- obj$tab
    row_names <- rownames(tmp)
    if (!is.null(obj$supv)) {
      tmp <- tmp %>% bind_cols(obj$supv$tab)
    }
    # Rebuild original data from `tab` slot
    tmp <- as.data.frame(vapply(names(tmp), function(name) {
        value <- sub("^.*?\\.", "", name)
        v <- rep("", nrow(tmp))
        v[tmp[,name] >= 0] <- value
        return(v)
    }, character(nrow(tmp))))
    names <- sub("\\..*$", "", names(tmp))
    for (name in unique(names)) {
        cols <- grep(paste0("^", name, "\\."), names(tmp), value = TRUE)
        tmp <- tmp %>% 
            tidyr::unite_(name, cols, sep = "")
    }
    tmp$Name <- row_names
    quali_data <- tmp
    
    return(list(vars = vars, ind = ind, eig = eig, axes = axes, vareta2 = vareta2, quali_data = quali_data))
    
}
