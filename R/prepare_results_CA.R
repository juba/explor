##' @rdname prepare_results
##' @aliases prepare_results.CA
##' 
##' @seealso \code{\link[FactoMineR]{CA}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @export

prepare_results.CA <- function(obj) {
    
    if (!inherits(obj, "CA")) stop("obj must be of class CA")

    ## Axes names and inertia
    axes <- seq_len(ncol(obj$col$coord))
    names(axes) <- paste("Axis", axes, paste0("(", head(round(obj$eig[, 2], 2), length(axes)),"%)"))
    ## Eigenvalues
    eig <- data.frame(dim = 1:nrow(obj$eig), percent = obj$eig[,2])
    
    ## Variables coordinates

    ## Columns
    vars <- data.frame(obj$col$coord)
    vars$name <- rownames(vars)
    vars$pos <- "Column"
    # Counts
    col.mods <- rownames(obj$col$coord)
    counts.cols <- sapply(obj$call$Xtot[, col.mods, drop = FALSE], sum, na.rm = TRUE)
    vars$Count <- counts.cols

    ## Rows
    tmp <- data.frame(obj$row$coord)
    tmp$name <- rownames(tmp)
    tmp$pos <- "Row"
    # Counts
    row.mods <- rownames(obj$row$coord)
    if (!is.null(obj$call$quali.sup) || !is.null(obj$call$quanti.sup)) {
        tmp_call <- obj$call$Xtot[, -c(obj$call$quali.sup, obj$call$quanti.sup)]
    } else {
        tmp_call <- obj$call$Xtot
    }
    counts.rows <- sapply(data.frame(t(tmp_call))[, make.names(row.mods), drop = FALSE], sum, na.rm = TRUE)
    tmp$Count <- counts.rows

    vars <- rbind(vars, tmp)
    vars$Type <- "Active"
    vars$Class <- "Qualitative"

    
    ## Supplementary rows coordinates
    if (!is.null(obj$row.sup)) {
        tmp <- data.frame(obj$row.sup$coord)
        tmp$name <- rownames(tmp)
        ## Counts
        row.mods <- rownames(obj$row.sup$coord)
        counts.rows <- sapply(data.frame(t(tmp_call), check.names = FALSE)[, row.mods, drop = FALSE], sum, na.rm = TRUE)
        tmp$Count <- counts.rows
        tmp$pos <- "Row"
        tmp$Type <- "Supplementary level"
        tmp$Class <- "Qualitative"  
        vars <- rbind(vars, tmp)
    }

    ## Supplementary columns coordinates
    if (!is.null(obj$col.sup)) {
        tmp <-  tmp <- data.frame(obj$col.sup$coord)
        tmp$name <- rownames(tmp)
        ## Counts
        col.mods <- rownames(obj$col.sup$coord)
        counts.cols <- sapply(obj$call$Xtot[, col.mods, drop = FALSE], sum, na.rm = TRUE)
        tmp$Count <- counts.cols
        tmp$pos <- "Column"
        tmp$Type <- "Supplementary level"
        tmp$Class <- "Qualitative"    
        vars <- rbind(vars, tmp)
    }
    
    ## Supplementary variables coordinates
    if (!is.null(obj$quali.sup)) {
        vars.quali.sup <- data.frame(obj$quali.sup$coord)
        vars.quali.sup$name <- rownames(vars.quali.sup)
        vars.quali.sup$Type <- "Supplementary variable"
        vars.quali.sup$Class <- "Qualitative"
        # quali.sup.mods <- rownames(obj$quali.sup$coord)
        # counts <- sapply(counts.tab[,quali.sup.mods, drop = FALSE], sum)
        # vars.quali.sup$Count <- counts
        vars.quali.sup$Count <- NA
        vars.quali.sup$pos <- "Supplementary variable"
        vars <- rbind(vars, vars.quali.sup)
    }

    vars <- vars %>% gather(Axis, Coord, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Coord = round(Coord, 3))

    ## Contributions
    tmp_row <- data.frame(obj$row$contrib)
    tmp_row <- tmp_row %>% mutate(name = rownames(tmp_row),
                                  pos = "Row",
                                  Type = "Active",
                                  Class = "Qualitative")
    tmp_col <- data.frame(obj$col$contrib)
    tmp_col <- tmp_col %>% mutate(name = rownames(tmp_col),
                                  pos = "Column",
                                  Type = "Active",
                                  Class = "Qualitative")
    tmp <- tmp_col %>% bind_rows(tmp_row) %>%
        gather(Axis, Contrib, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Contrib = round(Contrib, 3))
    
    vars <- vars %>% left_join(tmp, by = c("name", "pos", "Type", "Class", "Axis"))
    
    ## Cos2
    tmp_col <- data.frame(obj$col$cos2) %>%
        mutate(name = rownames(obj$col$cos2),
               pos = "Column")
    tmp_row <- data.frame(obj$row$cos2) %>%
        mutate(name = rownames(obj$row$cos2),
               pos = "Row")
    tmp <- tmp_col %>% bind_rows(tmp_row) %>%
        mutate(Type = "Active",
               Class = "Qualitative")
    
    ## Supplementary rows cos2
    if (!is.null(obj$row.sup)) {
        tmp_row_sup <- data.frame(obj$row.sup$cos2) %>%
            mutate(name = rownames(obj$row.sup$cos2),
                   pos = "Row",
                   Type = "Supplementary level",
                   Class = "Qualitative")
        tmp <- tmp %>% bind_rows(tmp_row_sup)
    }

    ## Supplementary columns cos2
    if (!is.null(obj$col.sup)) {
        tmp_col_sup <- data.frame(obj$col.sup$cos2) %>%
            mutate(name = rownames(obj$col.sup$cos2),
                   pos = "Column",
                   Type = "Supplementary level",
                   Class = "Qualitative")
        tmp <- tmp %>% bind_rows(tmp_col_sup)
    }
    
    ## Supplementary variables cos2
    if (!is.null(obj$quali.sup)) {
        tmp_sup <- data.frame(obj$quali.sup$cos2)
        tmp_sup$name <- rownames(tmp_sup)
        tmp_sup$pos <- "Supplementary variable"
        tmp_sup$Type <- "Supplementary variable"
        tmp_sup$Class <- "Qualitative"
        tmp <- tmp %>% bind_rows(tmp_sup)
    }

    tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
        mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
               Cos2 = round(Cos2, 3))
    
    vars <- vars %>% left_join(tmp, by = c("name", "pos", "Type", "Class", "Axis")) %>%
        rename(Level = name, Position = pos)


    return(list(vars = vars, eig = eig, axes = axes))
    
}
