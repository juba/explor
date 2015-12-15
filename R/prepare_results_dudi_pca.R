##' @rdname prepare_results
##' @aliases prepare_results.pca
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[ade4]{dudi.pca}}
##' @import dplyr
##' @importFrom tidyr gather
##' @importFrom utils head
##' @export

prepare_results.pca <- function(obj) {

  if (!inherits(obj, "pca") || !inherits(obj, "dudi")) stop("obj must be of class dudi and pca")

  if (!requireNamespace("ade4", quietly = TRUE)) {
    stop("the ade4 package is needed for this function to work.")
  }

  vars <- data.frame(obj$co)
  ## Axes names and inertia
  axes <- seq_len(ncol(vars))
  eig <- obj$eig / sum(obj$eig) * 100
  names(axes) <- paste("Axis", axes, paste0("(", head(round(eig, 2), length(axes)),"%)"))
  ## Eigenvalues
  eig <- data.frame(dim = 1:length(eig), percent = eig)
  ## Inertia
  inertia <- ade4::inertia.dudi(obj, row.inertia = TRUE, col.inertia = TRUE)
  
  ## Variables coordinates
  vars$varname <- rownames(vars)
  vars$Type <- "Active"
  vars$Class <- "Quantitative"
  
  ## Supplementary variables coordinates
  if (!is.null(obj$supv)) {
    vars.quanti.sup <- data.frame(obj$supv)
    vars.quanti.sup$varname <- rownames(vars.quanti.sup)
    vars.quanti.sup$Type <- "Supplementary"
    vars.quanti.sup$Class <- "Quantitative"
    vars <- rbind(vars, vars.quanti.sup)
  }

  vars <- vars %>% gather(Axis, Coord, starts_with("Comp")) %>%
    mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
           Coord = round(Coord, 3))

  ## Contributions
  tmp <- data.frame(inertia$col.abs / 100)
  tmp <- tmp %>% mutate(varname = rownames(tmp),
                        Type = "Active", Class = "Quantitative") %>%
    gather(Axis, Contrib, starts_with("Comp")) %>%
    mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
           Contrib = round(Contrib, 3))
    
  vars <- vars %>% left_join(tmp, by = c("varname", "Type", "Class", "Axis"))
  
  ## Cos2
  tmp <- data.frame(inertia$col.rel / 10000)
  tmp <- tmp %>% mutate(varname = rownames(tmp),
                        Type = "Active", Class = "Quantitative") %>%
    select(-con.tra)
  tmp <- tmp %>% gather(Axis, Cos2, starts_with("Comp")) %>%
    mutate(Axis = gsub("Comp", "", Axis, fixed = TRUE),
           Cos2 = round(Cos2, 3))
  
  vars <- vars %>% left_join(tmp, by = c("varname", "Type", "Class", "Axis"))
  
  vars <- vars %>% rename(Variable = varname)


  ## Individuals coordinates
  ind <- data.frame(obj$li)
  ind$Name <- rownames(ind)
  ind$Type <- "Active"
  if (!is.null(obj$supi)) {
    tmp_sup <- data.frame(obj$supi)
    tmp_sup$Name <- rownames(tmp_sup)
    tmp_sup$Type <- "Supplementary"
    ind <- ind %>% bind_rows(tmp_sup)
  }
  ind <- ind %>% gather(Axis, Coord, starts_with("Axis")) %>%
    mutate(Axis = gsub("Axis", "", Axis, fixed = TRUE),
           Coord = round(Coord, 3))

  ## Individuals contrib
  tmp <- data.frame(inertia$row.abs / 100)
  tmp <- tmp %>% mutate(Name = rownames(tmp), Type = "Active") %>%
    gather(Axis, Contrib, starts_with("Axis")) %>%
    mutate(Axis = gsub("Axis", "", Axis, fixed = TRUE),
           Contrib = round(Contrib, 3))
  
  ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
  
  ## Individuals Cos2
  tmp <- data.frame(inertia$row.rel / 10000)
  tmp$Name <- rownames(tmp)
  tmp$Type <- "Active"
  tmp <- tmp %>% select(-con.tra) %>%
    gather(Axis, Cos2, starts_with("Axis")) %>%
    mutate(Axis = gsub("Axis", "", Axis, fixed = TRUE),
           Cos2 = round(Cos2, 3))
  
  ind <- ind %>% left_join(tmp, by = c("Name", "Type", "Axis"))
  
  return(list(vars = vars, ind = ind, eig = eig, axes = axes))
  
}