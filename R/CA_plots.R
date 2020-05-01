## Functions to generate plots in explor_CA

## Variables plot reactive data
## Not exported
CA_var_data <- function(res, xax = 1, yax = 2, 
                        lev_sup = TRUE, var_sup = TRUE,  var_sup_choice = NULL, 
                        var_hide = "None",
                        var_lab_min_contrib = 0) {
    tmp_x <- res$vars %>% 
        filter(Axis == xax) %>%
        select("Level", "Position", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
    tmp_y <- res$vars %>% 
        filter(Axis == yax) %>%
        select("Level", "Position", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
    if (!(var_sup) || is.null(var_sup_choice)) {
        tmp_x <- tmp_x %>% filter(Type != 'Supplementary variable')
        tmp_y <- tmp_y %>% filter(Type != 'Supplementary variable')
    }
    if (var_sup && !is.null(var_sup_choice)) {
        tmp_x <- tmp_x %>% filter(Type != 'Supplementary variable' | Level %in% var_sup_choice)
        tmp_y <- tmp_y %>% filter(Type != 'Supplementary variable' | Level %in% var_sup_choice)
    }
    if (!lev_sup) {
        tmp_x <- tmp_x %>% filter(Type != 'Supplementary level')
        tmp_y <- tmp_y %>% filter(Type != 'Supplementary level')
    }
    if (var_hide != "None") {
        tmp_x <- tmp_x %>% filter(Position != var_hide)
        tmp_y <- tmp_y %>% filter(Position != var_hide)
    }
    tmp <- tmp_x %>%
        left_join(tmp_y, by = c("Level", "Position", "Type", "Class", "Count")) %>%
        mutate(Contrib = Contrib.x + Contrib.y,
               Cos2 = Cos2.x + Cos2.y,
               tooltip = paste(paste0("<strong>", Level, "</strong><br />"),
                               paste0("<strong>",
                                      gettext("Position", domain = "R-explor"),
                                      ":</strong> ", Position, "<br />"),
                               paste0("<strong>Axis ",xax," :</strong> ", Coord.x, "<br />"),
                               paste0("<strong>Axis ", yax," :</strong> ", Coord.y, "<br />"),
                               ifelse(is.na(Cos2), "",
                                      paste0("<strong>",
                                             gettext("Squared cosinus", domain = "R-explor"),
                                             ":</strong> ", Cos2, "<br />")),
                               ifelse(is.na(Contrib), "",
                                      paste0("<strong>",
                                             gettext("Contribution:", domain = "R-explor"),
                                             "</strong> ", Contrib, "<br />")),
                               ifelse(is.na(Count), "",
                                      paste0("<strong>",
                                             gettext("Count:", domain = "R-explor"),
                                             "</strong> ", Count))),
               Lab = ifelse(Contrib >= as.numeric(var_lab_min_contrib) | 
                            (is.na(Contrib) & as.numeric(var_lab_min_contrib) == 0), Level, ""))                 
    data.frame(tmp)
}


##' Interactive CA variables plot
##'
##' This function generates an HTML widget displaying the variables plot of a CA result.
##'
##' @param res Result of prepare_results() call
##' @param xax Horizontal axis number
##' @param yax Vertical axis number
##' @param lev_sup TRUE to display supplementary levels
##' @param var_sup TRUE to display supplementary variables
##' @param var_sup_choice list of supplementary variables to display
##' @param var_hide elements to hide (rows or columns)
##' @param var_lab_min_contrib Contribution threshold to display points labels
##' @param point_size base point size
##' @param col_var name of the variable for points color
##' @param symbol_var name of the variable for points symbol
##' @param size_var name of the variable for points size
##' @param size_range points size range with format c(minimum, maximum)
##' @param zoom_callback scatterD3 zoom callback JavaScript body
##' @param in_explor wether the plot is to be displayed in the \code{explor} interface
##' @param ... Other arguments passed to scatterD3
##'
##' @export
CA_var_plot <- function(res, xax = 1, yax = 2, 
                        lev_sup = TRUE,
                        var_sup = TRUE, 
                        var_sup_choice = NULL,
                        var_hide = "None",
                        var_lab_min_contrib = 0,
                        point_size = 64,
                        col_var = NULL,
                        symbol_var = NULL,
                        size_var = NULL,
                        size_range = c(10,300),
                        zoom_callback = NULL,
                        in_explor = FALSE, ...) {

    ## Settings changed if not run in explor
    html_id <- if(in_explor) "explor_var" else  NULL
    dom_id_svg_export <- if(in_explor) "explor-var-svg-export" else NULL
    dom_id_lasso_toggle <- if(in_explor) "explor-var-lasso-toggle" else NULL    
    lasso <- if(in_explor) TRUE else FALSE 
    lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
    zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "var") else NULL

    var_data <- CA_var_data(res, xax, yax, lev_sup, var_sup, var_sup_choice, var_hide, var_lab_min_contrib)

    scatterD3::scatterD3(
                   x = var_data[, "Coord.x"],
                   y = var_data[, "Coord.y"],
                   xlab = names(res$axes)[res$axes == xax],
                   ylab = names(res$axes)[res$axes == yax],
                   lab = var_data[, "Lab"],
                   point_size = point_size,
                   point_opacity = 1,
                   col_var = if (is.null(col_var)) NULL else var_data[,col_var],
                   col_lab = col_var,
                   symbol_var = if (is.null(symbol_var)) NULL else var_data[,symbol_var],
                   symbol_lab = symbol_var,
                   size_var = if (is.null(size_var)) NULL else var_data[,size_var],
                   size_lab = size_var,
                   size_range = if (is.null(size_var)) c(10,300) else c(30,400) * point_size / 32,
                   tooltip_text = var_data[, "tooltip"],
                   type_var = ifelse(var_data[, "Class"] == "Quantitative", "arrow", "point"),
                   unit_circle = var_sup && "Quantitative" %in% var_data[,"Class"],
                   key_var = paste(var_data[, "Position"], var_data[, "Level"], sep = "-"),
                   fixed = TRUE,
                   html_id = html_id,
                   dom_id_svg_export = dom_id_svg_export,
                   dom_id_lasso_toggle = dom_id_lasso_toggle,                   
                   lasso = lasso,
                   lasso_callback = lasso_callback,
                   zoom_callback = zoom_callback,
                   ...
               )  
}
