## Functions to generate plots in explor_PCA

## Variables plot reactive data
## Not exported
PCA_var_data <- function(res, xax = 1, yax = 2, var_sup = TRUE, 
    var_sup_choice = NULL, var_lab_min_contrib = 0) {
    tmp_x <- res$vars %>% 
        filter(Axis == xax) %>%
        select("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2")
    tmp_y <- res$vars %>% 
        filter(Axis == yax) %>%
        select("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2")
    if (!(var_sup) || is.null(var_sup_choice)) {
        tmp_x <- tmp_x %>% filter(Type == 'Active')
        tmp_y <- tmp_y %>% filter(Type == 'Active')
    }
    if (var_sup && !is.null(var_sup_choice)) {
        tmp_x <- tmp_x %>% filter(Type == 'Active' | Variable %in% var_sup_choice)
        tmp_y <- tmp_y %>% filter(Type == 'Active' | Variable %in% var_sup_choice)
    }

    tmp <- tmp_x %>%
        left_join(tmp_y, by = c("Variable", "Level", "Type", "Class")) %>%
        mutate(Contrib = Contrib.x + Contrib.y,
               Cos2 = Cos2.x + Cos2.y,
               tooltip = paste(ifelse(is.na(Level), "",
                                      paste0("<strong>",
                                             gettext("Level", domain = "R-explor"),
                                             ":</strong> ", Level, "<br />")),
                               paste0("<strong>",
                                      gettext("Variable", domain = "R-explor"),
                                      ":</strong> ", Variable, "<br />"),
                               paste0("<strong>Axis ",xax," :</strong> ", Coord.x, "<br />"),
                               paste0("<strong>Axis ", yax," :</strong> ", Coord.y, "<br />"),
                               ifelse(is.na(Cos2), "",
                                      paste0("<strong>",
                                             gettext("Squared cosinus", domain = "R-explor"),
                                             ":</strong> ", Cos2, "<br />")),
                               ifelse(is.na(Contrib), "",
                                      paste0("<strong>",
                                             gettext("Contribution:", domain = "R-explor"),
                                             "</strong> ", Contrib, "<br />"))),
               Level = ifelse(Class == "Qualitative", Level, Variable),
               Variable = if_else(Class == "Qualitative", Variable, "-"),
               Lab = ifelse(Contrib >= as.numeric(var_lab_min_contrib) | 
                            (is.na(Contrib) & as.numeric(var_lab_min_contrib) == 0), Level, ""))
    data.frame(tmp)
}


##' Interactive PCA variables plot
##'
##' This function generates an HTML widget displaying the variables plot of a PCA result.
##'
##' @param res Result of prepare_results() call
##' @param xax Horizontal axis number
##' @param yax Vertical axis number
##' @param var_sup TRUE to display supplementary variables
##' @param var_sup_choice list of supplementary variables to display
##' @param var_lab_min_contrib Contribution threshold to display points labels
##' @param col_var name of the variable for points color
##' @param size_var name of the variable for points size
##' @param scale_unit wether the PCA is scaled
##' @param zoom_callback scatterD3 zoom callback JavaScript body
##' @param xlim custom x axis limits
##' @param ylim custom y axis limits
##' @param in_explor wether the plot is to be displayed in the \code{explor} interface
##' @param ... Other arguments passed to scatterD3
##'
##' @export
PCA_var_plot <- function(res, xax = 1, yax = 2, var_sup = TRUE, 
                         var_sup_choice = NULL,
                         var_lab_min_contrib = 0,
                         scale_unit = FALSE,
                         col_var = NULL,
                         size_var = NULL,
                         zoom_callback = NULL,
                         in_explor = FALSE,
                         xlim = NULL, ylim = NULL, ...) {

    has_quali_sup_vars <- any("Supplementary" %in% res$vars$Type &
                              "Qualitative" %in% res$vars$Class)
    
    ## Settings changed if not run in explor
    html_id <- if(in_explor) "explor_var" else  NULL
    dom_id_svg_export <- if(in_explor) "explor-var-svg-export" else NULL
    dom_id_lasso_toggle <- if(in_explor) "explor-var-lasso-toggle" else NULL    
    lasso <- if(in_explor) TRUE else FALSE 
    lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
    zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "var") else NULL
    if (is.null(xlim) && scale_unit && !has_quali_sup_vars) xlim <- c(-1.1, 1.1)
    if (is.null(ylim) && scale_unit && !has_quali_sup_vars) ylim <- c(-1.1, 1.1)
    
    var_data <- PCA_var_data(res, xax, yax, var_sup, var_sup_choice, var_lab_min_contrib)
    
    scatterD3::scatterD3(
                   x = var_data[, "Coord.x"],
                   y = var_data[, "Coord.y"],
                   xlab = names(res$axes)[res$axes == xax],
                   ylab = names(res$axes)[res$axes == yax],
                   lab = var_data[, "Lab"],
                   point_opacity = 1,
                   col_var = if (is.null(col_var)) NULL else var_data[,col_var],
                   col_lab = col_var,
                   tooltip_text = var_data[, "tooltip"],
                   type_var = ifelse(var_data[,"Class"] == "Quantitative", "arrow", "point"),
                   key_var = var_data[, "Level"],
                   unit_circle = scale_unit,
                   fixed = TRUE,
                   html_id = html_id,
                   dom_id_svg_export = dom_id_svg_export,
                   dom_id_lasso_toggle = dom_id_lasso_toggle,
                   lasso = lasso,
                   lasso_callback = lasso_callback,
                   zoom_callback = zoom_callback,
                   xlim = xlim, ylim = ylim,
                   ...
               )  
}

## PCA individuals plot data
PCA_ind_data <- function(res, xax = 1, yax = 2, ind_sup = TRUE, col_var = NULL, opacity_var = NULL, ind_lab_min_contrib = 0) {
    tmp_x <- res$ind %>% 
        filter(Axis == xax) %>%
        select(Name, Type, Coord, Contrib, Cos2)
    tmp_y <- res$ind %>% 
        filter(Axis == yax) %>%
        select(Name, Type, Coord, Contrib, Cos2)
    if (!ind_sup) {
        tmp_x <- tmp_x %>% filter(Type == "Active")
        tmp_y <- tmp_y %>% filter(Type == "Active")
    }
    tmp <- tmp_x %>%
        left_join(tmp_y, by = c("Name", "Type")) %>%
        mutate(Contrib = Contrib.x + Contrib.y,
               Cos2 = Cos2.x + Cos2.y,
               tooltip = paste(paste0("<strong>", Name, "</strong><br />"),
                               paste0("<strong>Axis ", xax," :</strong> ", Coord.x, "<br />"),
                               paste0("<strong>Axis ", yax," :</strong> ", Coord.y, "<br />"),
                               ifelse(is.na(Cos2), "",
                                   paste0("<strong>",
                                          gettext("Squared cosinus", domain = "R-explor"),
                                          ":</strong> ", Cos2, "<br />")),
                               ifelse(is.na(Contrib), "",
                                  paste0("<strong>",
                                         gettext("Contribution:", domain = "R-explor"),
                                         "</strong> ", Contrib, "<br />"))),
               Lab = ifelse(Contrib >= as.numeric(ind_lab_min_contrib) | 
                              (is.na(Contrib) & as.numeric(ind_lab_min_contrib) == 0), Name, ""))
    if (!(is.null(col_var) || col_var %in% c("None", "Type"))) {
        tmp_data <- res$quali_data %>% select("Name", col_var)
        tmp <- tmp %>%
            left_join(tmp_data, by = "Name")
    }
    data.frame(tmp)
}

##' Interactive PCA indivuals plot
##'
##' This function generates an HTML widget displaying the individuals plot of a PCA result.
##'
##' @param res Result of prepare_results() call
##' @param xax Horizontal axis number
##' @param yax Vertical axis number
##' @param ind_sup TRUE to display supplementary individuals
##' @param col_var variable to be used for points color
##' @param symbol_var name of the variable for points symbol
##' @param opacity_var name of the variable for points opacity
##' @param lab_var variable to be used for points names
##' @param ind_lab_min_contrib Contribution threshold to display points labels
##' @param size_var name of the variable for points size
##' @param size_range points size range with format c(minimum, maximum)
##' @param zoom_callback scatterD3 zoom callback JavaScript body
##' @param in_explor wether the plot is to be displayed in the \code{explor} interface
##' @param ... Other arguments passed to scatterD3
##'
##' @export
PCA_ind_plot <- function(res, xax = 1, yax = 2, ind_sup = TRUE, ind_lab_min_contrib = 0,
                         col_var = NULL,
                         symbol_var = NULL,
                         opacity_var = NULL,
                         size_var = NULL,
                         size_range = c(10,300),
                         lab_var = NULL,
                         zoom_callback = NULL,
                         in_explor = FALSE,
                         ...) {

    html_id <- if(in_explor) "explor_ind" else  NULL
    dom_id_svg_export <- if(in_explor) "explor-ind-svg-export" else NULL
    dom_id_lasso_toggle <- if(in_explor) "explor-ind-lasso-toggle" else NULL
    lasso <- if(in_explor) TRUE else FALSE 
    lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
    zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "ind") else NULL
    
    ind_data <- PCA_ind_data(res, xax, yax, ind_sup, col_var, opacity_var, ind_lab_min_contrib)

    scatterD3::scatterD3(
                   x = ind_data[, "Coord.x"],
                   y = ind_data[, "Coord.y"],
                   xlab = names(res$axes)[res$axes == xax],
                   ylab = names(res$axes)[res$axes == yax],
                   lab = if (is.null(lab_var)) NULL else ind_data[,lab_var],
                   col_var = if (is.null(col_var)) NULL else ind_data[,col_var],
                   col_lab = col_var,
                   opacity_var = if (is.null(opacity_var)) NULL else ind_data[,opacity_var],
                   tooltip_text = ind_data[, "tooltip"],
                   key_var = ind_data[, "Name"],
                   fixed = TRUE,
                   html_id = html_id,
                   dom_id_svg_export = dom_id_svg_export,
                   dom_id_lasso_toggle = dom_id_lasso_toggle,                   
                   lasso = lasso,
                   lasso_callback = lasso_callback,
                   zoom_callback = zoom_callback,
                   ...)

}

