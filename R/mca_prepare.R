##' Results preparation for a FactoMineR MCA analysis
##'
##' This function prepares MCA results to be used by \code{imca}. Not to be used directly.
##'
##' @param acm an object of class MCA, result of the \code{MCA()} function from the \code{FactoMineR} package.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[FactoMineR]{MCA}}
##' @import dplyr
##' @import tidyr

mca_prepare.MCA <- function(mca) {

  vars <- data.frame(mca$var$coord)
  ## Axes names and inertia
  axes <- seq_len(ncol(acm$var$coord))
  names(axes) <- paste("Axis", axes, paste0("(", head(round(mca$eig[, 2], 2), length(axes)),"%)"))
  ## Eigenvalues
  eig <- data.frame(dim = 1:nrow(acm$eig), percent = acm$eig[,2])
  
  ## Variables data coordinates
  varnames <- sapply(mca$call$X[,mca$call$quali, drop = FALSE], nlevels)
  vars$varname <- rep(names(varnames),varnames)
  vars$modname <- rownames(vars)
  vars$Type <- "Primary"
  
  ## Supplementary variables coordinates
  if (!is.null(mca$quali.sup)) {
    vars.quali.sup <- data.frame(mca$quali.sup$coord)
    varnames <- sapply(mca$call$X[, mca$call$quali.sup, drop = FALSE], nlevels)
    vars.quali.sup$varname <- rep(names(varnames),varnames)
    vars.quali.sup$modname <- rownames(vars.quali.sup)
    vars.quali.sup$Type <- "Supplementary"
    vars <- rbind(vars, vars.quali.sup)
  }

  vars <- vars %>% gather(Axis, Coord, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Coord = signif(Coord, 3))

  ## Contributions
  tmp <- data.frame(mca$var$contrib)
  tmp <- tmp %>% mutate(modname = rownames(tmp), Type = "Primary") %>%
    gather(Axis, Contrib, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Contrib = signif(Contrib, 3))
    
  vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Axis"))
  
  ## Cos2
  tmp <- data.frame(mca$var$cos2)
  tmp$modname <- rownames(tmp)
  tmp$Type <- "Primary"
  if (!is.null(mca$quali.sup)) {
    tmp_sup <- data.frame(mca$quali.sup$cos2)
    tmp_sup$modname <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    tmp <- tmp %>% bind_rows(tmp_sup)
  }
  tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Cos2 = signif(Cos2, 2))
  
  vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Axis"))

  ## V.test  
  tmp <- data.frame(mca$var$v.test)
  tmp$modname <- rownames(tmp)
  tmp$Type <- "Primary"
  if (!is.null(mca$quali.sup)) {
    tmp_sup <- data.frame(mca$quali.sup$v.test)
    tmp_sup$modname <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    tmp <- tmp %>% bind_rows(tmp_sup)
  }
  tmp <- tmp %>% gather(Axis, V.test, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           P.value = signif(ifelse(V.test >= 0, 2 * (1 - pnorm(V.test)), 2 * pnorm(V.test)), 3),
           V.test = signif(V.test, 2))
  
  vars <- vars %>% left_join(tmp, by = c("modname", "Type", "Axis")) %>%
    rename(Variable = varname, Level = modname)

  ## Variables eta2
  vareta2 <- data.frame(mca$var$eta2)
  vareta2$Variable <- rownames(vareta2)
  vareta2$Type <- "Primary"
  if (!is.null(mca$quali.sup)) {
    vareta2_sup <- data.frame(mca$quali.sup$eta2)
    vareta2_sup$Variable <- rownames(vareta2_sup)
    vareta2_sup$Type <- "Supplementary"
    vareta2 <- vareta2 %>% bind_rows(vareta2_sup)
  }
  vareta2 <- vareta2 %>% gather(Axis, eta2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE))
  vareta2$eta2 <- format(vareta2$eta2, scientific = FALSE, nsmall = 3, digits = 0)

  ## Individuals coordinates
  ind <- data.frame(mca$ind$coord)
  ind$Name <- rownames(ind)
  ind$Type <- "Primary"
  if (!is.null(mca$ind.sup)) {
    tmp_sup <- data.frame(mca$ind.sup$coord)
    tmp_sup$Name <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    ind <- ind %>% bind_rows(tmp_sup)
  }
  ind <- ind %>% gather(Axis, Coord, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Coord = signif(Coord, 3))

  ## Individuals contrib
  tmp <- data.frame(mca$ind$contrib)
  tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Primary") %>%
    gather(Axis, Contrib, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Contrib = signif(Contrib, 3))
  
  ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
  
  ## Individuals Cos2
  tmp <- data.frame(mca$ind$cos2)
  tmp$Name <- rownames(tmp)
  tmp$Type <- "Primary"
  if (!is.null(mca$ind.sup)) {
    tmp_sup <- data.frame(mca$ind.sup$cos2)
    tmp_sup$Name <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    tmp <- tmp %>% bind_rows(tmp_sup)
  }
  tmp <- tmp %>% gather(Axis, Cos2, starts_with("Dim.")) %>%
    mutate(Axis = gsub("Dim.", "", Axis, fixed = TRUE),
           Cos2 = signif(Cos2, 2))
  
  ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
  
  return(list(vars = vars, ind = ind, eig = eig, axes = axes, vareta2 = vareta2))
  
}