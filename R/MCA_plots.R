## Functions to generate plots in explor_MCA

## Variables plot reactive data
## Not exported
MCA_var_data <- function(res, xax = 1, yax = 2, var_sup = TRUE, var_lab_min_contrib = 0) {
  tmp_x <- res$vars %>%
    arrange(Axis, Type, Variable) %>%
    filter(Axis == xax) %>%
    select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2")
  tmp_y <- res$vars %>% 
    filter(Axis == yax) %>%
    select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2")
  if (!(var_sup)) {
    tmp_x <- tmp_x %>% filter(Type == 'Active')
    tmp_y <- tmp_y %>% filter(Type == 'Active')
  }
  tmp <- tmp_x %>%
    left_join(tmp_y, by = c("Variable", "Level", "Type", "Class")) %>%
    mutate(Contrib = Contrib.x + Contrib.y,
           Cos2 = Cos2.x + Cos2.y,
           tooltip = paste(paste0("<strong>", Level, "</strong>"),
                           paste0("<strong>",
                                  gettext("Variable", domain = "R-explor"),
                                  ":</strong> ", Variable),
                           paste0("<strong>x:</strong> ", Coord.x),
                           paste0("<strong>y:</strong> ", Coord.y),
                           paste0("<strong>",
                                  gettext("Cos2", domain = "R-explor"),
                                  ":</strong> ", Cos2),
                           paste0("<strong>",
                                  gettext("Contribution:", domain = "R-explor"),
                                  "</strong> ", Contrib),
                           sep = "<br />"),
           Lab = ifelse(Contrib >= as.numeric(var_lab_min_contrib) | 
                          (is.na(Contrib) & as.numeric(var_lab_min_contrib) == 0), Level, ""))
  data.frame(tmp)
}


##' Interactive MCA variables plot
##'
##' This function generates an HTML widget displaying the variables plot of an MCA result.
##'
##' @param res Result of prepare_results() call
##' @param xax Horizontal axis number
##' @param yax Vertical axis number
##' @param var_sup TRUE to display supplementary variables
##' @param var_lab_min_contrib Contribution threshold to display points labels
##' @param col_var name of the variable for points color
##' @param symbol_var name of the variable for points symbol
##' @param size_var name of the variable for points size
##' @param size_range points size range with format c(minimum, maximum)
##' @param var_lab_size points label size
##' @param var_point_size fixed points size
##' @param transitions wether to display animated transitions (only when used in a shiny app)
##'
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export
MCA_var_plot <- function(res, xax = 1, yax = 2, var_sup = TRUE, var_lab_min_contrib = 0,
                         col_var = "Variable",
                         symbol_var = NULL,
                         size_var = NULL,
                         size_range = c(10,400),
                         var_lab_size = 10,  
                         var_point_size = 30,
                         transitions = FALSE) {
  var_data <- MCA_var_data(res, xax, yax, var_sup, var_lab_min_contrib)
  scatterD3::scatterD3(
    x = var_data[, "Coord.x"],
    y = var_data[, "Coord.y"],
    xlab = names(res$axes)[res$axes == xax],
    ylab = names(res$axes)[res$axes == yax],
    lab = var_data[, "Lab"],
    labels_size = var_lab_size,
    point_opacity = 1,
    point_size = var_point_size,
    col_var = if (is.null(col_var)) NULL else var_data[,col_var],
    col_lab = col_var,
    symbol_var = if (is.null(symbol_var)) NULL else var_data[,symbol_var],
    symbol_lab = symbol_var,
    size_var = if (is.null(size_var)) NULL else var_data[,size_var],
    size_lab = size_var,
    size_range = if (is.null(size_var)) c(10,300) else c(30,400) * var_point_size / 32,
    tooltip_text = var_data[, "tooltip"],
    type_var = ifelse(var_data[,"Class"] == "Quantitative", "arrow", "point"),
    unit_circle = var_sup && "Quantitative" %in% var_data[,"Class"],
    key_var = paste(var_data[, "Variable"], var_data[, "Level"], sep = "-"),
    fixed = TRUE,
    transitions = transitions,
    html_id = "explor_var",
    dom_id_reset_zoom = "explor-var-reset-zoom",
    dom_id_svg_export = "explor-var-svg-export",
    dom_id_lasso_toggle = "explor-var-lasso-toggle",
    lasso = TRUE,
    lasso_callback = explor_multi_lasso_callback()
  )  
}

## MCA individuals plot data
MCA_ind_data <- function(res, xax = 1, yax = 2, ind_sup = TRUE, ind_col = NULL) {
  tmp_x <- res$ind %>% 
    filter(Axis == xax) %>%
    select(Name, Type, Coord, Contrib, Cos2)
  if (!ind_sup)
    tmp_x <- tmp_x %>% filter(Type == "Active")
  tmp_y <- res$ind %>% 
    filter(Axis == yax) %>%
    select(Name, Type, Coord, Contrib, Cos2)
  if (!ind_sup)
    tmp_y <- tmp_y %>% filter(Type == "Active")
  tmp <- tmp_x %>%
    left_join(tmp_y, by = c("Name", "Type")) %>%
    mutate(Contrib = Contrib.x + Contrib.y,
           Cos2 = Cos2.x + Cos2.y,
           tooltip = paste(paste0("<strong>", Name, "</strong>"),
                           paste0("<strong>x:</strong> ", Coord.x),
                           paste0("<strong>y:</strong> ", Coord.y),
                           paste0("<strong>",
                                  gettext("Squared cosinus", domain = "R-explor"),
                                  ":</strong> ", Cos2),
                           paste0("<strong>",
                                  gettext("Contribution:", domain = "R-explor"),
                                  "</strong> ", Contrib),
                           sep = "<br />"))
  if (!(is.null(ind_col) || ind_col %in% c("None", "Type"))) {
    tmp_data <- res$quali_data %>% select_("Name", ind_col)
    tmp <- tmp %>%
      left_join(tmp_data, by = "Name")
  }
  data.frame(tmp)
}

##' Interactive MCA indivuals plot
##'
##' This function generates an HTML widget displaying the individuals plot of an MCA result.
##'
##' @param res Result of prepare_results() call
##' @param xax Horizontal axis number
##' @param yax Vertical axis number
##' @param symbol_var name of the variable for points symbol
##' @param size_var name of the variable for points size
##' @param size_range points size range with format c(minimum, maximum)
##' @param transitions wether to display animated transitions (only when used in a shiny app)
##' @param ind_sup TRUE to display supplementary individuals
##' @param ind_col variable to be used for points color
##' @param lab_var variable to be used for points names
##' @param ellipses wether to display confidence ellipses for points color
##' @param ind_point_size fixed points size
##' @param ind_labels_size points labels size
##' @param ind_opacity points opacity
##'
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @export
MCA_ind_plot <- function(res, xax = 1, yax = 2, ind_sup = TRUE, ind_col = NULL,
                         symbol_var = "Type",
                         size_var = NULL,
                         size_range = c(10,400),
                         lab_var = "Name",
                         ellipses = FALSE,
                         ind_point_size = 64,
                         ind_labels_size = 9,
                         ind_opacity = 0.5,
                         transitions = FALSE) {
  ind_data <- MCA_ind_data(res, xax, yax, ind_sup, ind_col)
  scatterD3::scatterD3(
    x = ind_data[, "Coord.x"],
    y = ind_data[, "Coord.y"],
    xlab = names(res$axes)[res$axes == xax],
    ylab = names(res$axes)[res$axes == yax],
    point_size = ind_point_size,
    point_opacity = ind_opacity,
    lab = if (is.null(lab_var)) NULL else ind_data[,lab_var],
    labels_size = ind_labels_size,
    col_var = if (is.null(ind_col)) NULL else ind_data[,ind_col],
    col_lab = ind_col,
    ellipses = ellipses,
    tooltip_text = ind_data[, "tooltip"],
    key_var = ind_data[, "Name"],
    fixed = TRUE,
    transitions = transitions,
    html_id = "explor_ind",
    dom_id_reset_zoom = "explor-ind-reset-zoom",
    dom_id_svg_export = "explor-ind-svg-export",
    dom_id_lasso_toggle = "explor-ind-lasso-toggle",
    lasso = TRUE,
    lasso_callback = explor_multi_lasso_callback()
  )
}

