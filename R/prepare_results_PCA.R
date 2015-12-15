##' @rdname prepare_results
##' @aliases prepare_results.PCA
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
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
  vars$Type <- "Active"
  vars$Class <- "Quantitative"
  
  ## Supplementary variables coordinates
  # if (!is.null(obj$quali.sup)) {
  #   vars.quali.sup <- data.frame(obj$quali.sup$coord)
  #   varnames <- sapply(obj$call$X[, obj$call$quali.sup, drop = FALSE], nlevels)
  #   vars.quali.sup$varname <- rep(names(varnames),varnames)
  #   vars.quali.sup$modname <- rownames(vars.quali.sup)
  #   vars.quali.sup$Type <- "Supplementary"
  #   vars.quali.sup$Class <- "Qualitative"    
  #   vars <- rbind(vars, vars.quali.sup)
  # }

  ## Quantitative supplementary variables coordinates
  if (!is.null(obj$quanti.sup)) {
    vars.quanti.sup <- data.frame(obj$quanti.sup$coord)
    vars.quanti.sup$varname <- rownames(obj$quanti.sup$coord)
    vars.quanti.sup$Type <- "Supplementary"
    vars.quanti.sup$Class <- "Quantitative"
    vars <- rbind(vars, vars.quanti.sup)
  }

  vars <- vars %>% gather(Axis, Coord, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Coord = round(Coord, 3))

  ## Contributions
  tmp <- data.frame(obj$var$contrib)
  tmp <- tmp %>% mutate(varname = rownames(tmp), Type = "Active", Class = "Quantitative") %>%
    gather(Axis, Contrib, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Contrib = round(Contrib, 3))
    
  vars <- vars %>% left_join(tmp, by = c("varname", "Type", "Class", "Axis"))
  
  ## Cos2
  tmp <- data.frame(obj$var$cos2)
  tmp$varname <- rownames(tmp)
  tmp$Type <- "Active"
  tmp$Class <- "Quantitative"
  if (!is.null(obj$quanti.sup)) {
    tmp_sup <- data.frame(obj$quanti.sup$cos2)
    tmp_sup$varname <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    tmp_sup$Class <- "Quantitative"
    tmp <- tmp %>% bind_rows(tmp_sup)
  }
  tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Cos2 = round(Cos2, 3))
  
  vars <- vars %>% left_join(tmp, by = c("varname", "Type", "Class", "Axis"))

  ## Cor  
  tmp <- data.frame(obj$var$cor)
  tmp$varname <- rownames(tmp)
  tmp$Type <- "Active"
  tmp$Class <- "Quantitative"  
  if (!is.null(obj$quanti.sup)) {
    tmp_sup <- data.frame(obj$quanti.sup$cor)
    tmp_sup$varname <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    tmp_sup$Class <- "Quantitative"    
    tmp <- tmp %>% bind_rows(tmp_sup)
  }
  tmp <- tmp %>% gather(Axis, Cor, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Cor = round(Cor, 3))
  
  vars <- vars %>% left_join(tmp, by = c("varname", "Type", "Class", "Axis")) %>% 
    rename(Variable = varname)

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
  
  ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
  
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
  
  ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
  
  return(list(vars = vars, ind = ind, eig = eig, axes = axes))
  
}