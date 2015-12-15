##' @rdname prepare_results
##' @aliases prepare_results.CA
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
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
  vars <- data.frame(obj$col$coord)
  vars$name <- rownames(vars)
  vars$pos <- "Column"
  tmp <- data.frame(obj$row$coord)
  tmp$name <- rownames(tmp)
  tmp$pos <- "Row"
  vars <- rbind(vars, tmp)
  vars$Type <- "Active"
  vars$Class <- "Qualitative"
  
  ## Supplementary rows coordinates
  if (!is.null(obj$row.sup)) {
    tmp <- data.frame(obj$row.sup$coord)
    tmp$name <- rownames(tmp)
    tmp$pos <- "Row"
    tmp$Type <- "Supplementary"
    tmp$Class <- "Qualitative"    
    vars <- rbind(vars, tmp)
  }

  ## Supplementary columns coordinates
  if (!is.null(obj$col.sup)) {
    tmp <-  tmp <- data.frame(obj$col.sup$coord)
    tmp$name <- rownames(tmp)
    tmp$pos <- "Column"
    tmp$Type <- "Supplementary"
    tmp$Class <- "Qualitative"    
    vars <- rbind(vars, tmp)
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
             Type = "Supplementary",
             Class = "Qualitative")
    tmp <- tmp %>% bind_rows(tmp_row_sup)
  }

  ## Supplementary columns cos2
  if (!is.null(obj$col.sup)) {
    tmp_col_sup <- data.frame(obj$col.sup$cos2) %>%
      mutate(name = rownames(obj$col.sup$cos2),
             pos = "Column",
             Type = "Supplementary",
             Class = "Qualitative")
    tmp <- tmp %>% bind_rows(tmp_col_sup)
  }
  
  tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Cos2 = round(Cos2, 3))
  
  vars <- vars %>% left_join(tmp, by = c("name", "pos", "Type", "Class", "Axis")) %>%
    rename(Level = name, Position = pos)


  return(list(vars = vars, eig = eig, axes = axes))
  
}