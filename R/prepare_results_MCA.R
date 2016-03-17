##' @rdname prepare_results
##' @aliases prepare_results.MCA
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[FactoMineR]{MCA}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @importFrom stats pnorm
##' @export

prepare_results.MCA <- function(obj) {

  if (!inherits(obj, "MCA")) stop("obj must be of class MCA")
  
  vars <- data.frame(obj$var$coord)
  ## Axes names and inertia
  axes <- seq_len(ncol(obj$var$coord))
  names(axes) <- paste("Axis", axes, paste0("(", head(round(obj$eig[, 2], 2), length(axes)),"%)"))
  ## Eigenvalues
  eig <- data.frame(dim = 1:nrow(obj$eig), percent = obj$eig[,2])
  
  ## Variables coordinates
  varnames <- sapply(obj$call$X[,obj$call$quali, drop = FALSE], nlevels)
  vars$varname <- rep(names(varnames),varnames)
  vars$modname <- rownames(vars)
  vars$Type <- "Active"
  vars$Class <- "Qualitative"
  
  ## Supplementary variables coordinates
  if (!is.null(obj$quali.sup)) {
    vars.quali.sup <- data.frame(obj$quali.sup$coord)
    varnames <- sapply(obj$call$X[, obj$call$quali.sup, drop = FALSE], nlevels)
    vars.quali.sup$varname <- rep(names(varnames),varnames)
    vars.quali.sup$modname <- rownames(vars.quali.sup)
    vars.quali.sup$Type <- "Supplementary"
    vars.quali.sup$Class <- "Qualitative"    
    vars <- rbind(vars, vars.quali.sup)
  }

  ## Quantitative supplementary variables coordinates
  if (!is.null(obj$quanti.sup)) {
    vars.quanti.sup <- data.frame(obj$quanti.sup$coord)
    vars.quanti.sup$varname <- rownames(obj$quanti.sup$coord)
    vars.quanti.sup$modname <- rownames(obj$quanti.sup$coord)
    vars.quanti.sup$Type <- "Supplementary"
    vars.quanti.sup$Class <- "Quantitative"
    vars <- rbind(vars, vars.quanti.sup)
  }

  vars <- vars %>% gather(Axis, Coord, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Coord = round(Coord, 3))

  ## Contributions
  tmp <- data.frame(obj$var$contrib)
  tmp <- tmp %>% mutate(modname = rownames(tmp), Type = "Active", Class = "Qualitative") %>%
    gather(Axis, Contrib, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Contrib = round(Contrib, 3))
    
  vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Class", "Axis"))
  
  ## Cos2
  tmp <- data.frame(obj$var$cos2)
  tmp$modname <- rownames(tmp)
  tmp$Type <- "Active"
  tmp$Class <- "Qualitative"
  if (!is.null(obj$quali.sup)) {
    tmp_sup <- data.frame(obj$quali.sup$cos2)
    tmp_sup$modname <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    tmp_sup$Class <- "Qualitative"
    tmp <- tmp %>% bind_rows(tmp_sup)
  }
  tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Cos2 = round(Cos2, 3))
  
  vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Class", "Axis"))

  ## V.test for supplementary qualitative variables
  if (!is.null(obj$quali.sup)) {  
    tmp <- data.frame(obj$quali.sup$v.test)
    tmp$modname <- rownames(tmp)
    tmp$Type <- "Supplementary"
    tmp$Class <- "Qualitative"    
    tmp <- tmp %>% gather(Axis, V.test, starts_with("Dim.")) %>%
      mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
             P.value = round(ifelse(V.test >= 0, 2 * (1 - pnorm(V.test)), 2 * pnorm(V.test)), 3),
             V.test = round(V.test, 2))
  
    vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Class", "Axis"))
  }
  
  vars <- vars %>%
    rename(Variable = varname, Level = modname) %>%
    as.data.frame()
  
  ## Variables eta2
  vareta2 <- data.frame(obj$var$eta2)
  vareta2$Variable <- rownames(vareta2)
  vareta2$Type <- "Active"
  vareta2$Class <- "Qualitative"
  if (!is.null(obj$quali.sup)) {
    vareta2_sup <- data.frame(obj$quali.sup$eta2)
    vareta2_sup$Variable <- rownames(vareta2_sup)
    vareta2_sup$Type <- "Supplementary"
    vareta2_sup$Class <- "Qualitative"
    vareta2 <- vareta2 %>% bind_rows(vareta2_sup)
  }
  vareta2 <- vareta2 %>% gather(Axis, eta2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE))
  vareta2$eta2 <- format(vareta2$eta2, scientific = FALSE, nsmall = 3, digits = 0)

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
  
  ## Qualitative data for individuals plot color mapping
  quali_data <- obj$call$X[,obj$call$quali]
  if (!is.null(obj$quali.sup)) {
    quali_data <- quali_data %>% bind_cols(obj$call$X[,obj$call$quali.sup, drop = FALSE])
  }
  quali_data$Name <- rownames(obj$call$X)
    
  
  return(list(vars = vars, ind = ind, eig = eig, axes = axes, vareta2 = vareta2, quali_data = quali_data))
  
}