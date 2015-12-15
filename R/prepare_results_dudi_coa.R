##' @rdname prepare_results
##' @aliases prepare_results.CA
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[FactoMineR]{CA}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @export

prepare_results.coa <- function(obj) {
  
  if (!inherits(obj, "coa")) stop("obj must be of class coa")

  if (!requireNamespace("ade4", quietly = TRUE)) {
    stop("the ade4 package is needed for this function to work.")
  }  

  ## Axes names and inertia
  axes <- seq_len(ncol(obj$co))
  eig <- obj$eig / sum(obj$eig) * 100
  names(axes) <- paste("Axis", axes, paste0("(", head(round(eig, 2), length(axes)),"%)"))
  ## Eigenvalues
  eig <- data.frame(dim = 1:length(eig), percent = eig)
  ## Inertia
  inertia <- ade4::inertia.dudi(obj, row.inertia = TRUE, col.inertia = TRUE)
    
  ## Variables coordinates
  vars <- data.frame(obj$co)
  vars$name <- rownames(vars)
  vars$pos <- "Column"
  tmp <- data.frame(obj$li)
  tmp$name <- rownames(tmp)
  tmp$pos <- "Row"
  names(tmp) <- gsub("Axis", "Comp", names(tmp), fixed = TRUE)
  vars <- vars %>% bind_rows(tmp)
  vars$Type <- "Active"
  vars$Class <- "Qualitative"
  
  ## Supplementary rows coordinates
  if (!is.null(obj$supr)) {
    tmp <- data.frame(obj$supr)
    tmp$name <- rownames(tmp)
    tmp$pos <- "Row"
    tmp$Type <- "Supplementary"
    tmp$Class <- "Qualitative"
    names(tmp) <- gsub("Axis", "Comp", names(tmp), fixed = TRUE)    
    vars <- rbind(vars, tmp)
  }

  ## Supplementary columns coordinates
  if (!is.null(obj$supc)) {
    tmp <-  tmp <- data.frame(obj$supc)
    tmp$name <- rownames(tmp)
    tmp$pos <- "Column"
    tmp$Type <- "Supplementary"
    tmp$Class <- "Qualitative"    
    vars <- rbind(vars, tmp)
  }
  
  vars <- vars %>% gather(Axis, Coord, starts_with("Comp")) %>%
    mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
           Coord = round(Coord, 3))

  ## Contributions
  tmp_row <- data.frame(inertia$row.abs / 100)
  tmp_row <- tmp_row %>% mutate(name = rownames(tmp_row),
                                pos = "Row",
                                Type = "Active",
                                Class = "Qualitative")
  names(tmp_row) <- gsub("Axis", "Comp", names(tmp_row), fixed = TRUE)      
  tmp_col <- data.frame(inertia$col.abs / 100)
  tmp_col <- tmp_col %>% mutate(name = rownames(tmp_col),
                                pos = "Column",
                                Type = "Active",
                                Class = "Qualitative")
  tmp <- tmp_col %>% bind_rows(tmp_row) %>%
    gather(Axis, Contrib, starts_with("Comp")) %>%
    mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
           Contrib = round(Contrib, 3))
    
  vars <- vars %>% left_join(tmp, by = c("name", "pos", "Type", "Class", "Axis"))
  
  ## Cos2
  tmp_row <- data.frame(inertia$row.rel / 10000)
  tmp_row <- tmp_row %>% mutate(name = rownames(tmp_row),
                                pos = "Row",
                                Type = "Active",
                                Class = "Qualitative")
  names(tmp_row) <- gsub("Axis", "Comp", names(tmp_row), fixed = TRUE)      
  tmp_col <- data.frame(inertia$col.rel / 10000)
  tmp_col <- tmp_col %>% mutate(name = rownames(tmp_col),
                                pos = "Column",
                                Type = "Active",
                                Class = "Qualitative")
  tmp <- tmp_col %>% bind_rows(tmp_row) %>%
    gather(Axis, Cos2, starts_with("Comp")) %>%
    mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
           Cos2 = round(Cos2, 3))
  
  vars <- vars %>% left_join(tmp, by = c("name", "pos", "Type", "Class", "Axis")) %>%
    select(-con.tra) %>%
    rename(Level = name, Position = pos) %>%
    as.data.frame()

  return(list(vars = vars, eig = eig, axes = axes))
  
}