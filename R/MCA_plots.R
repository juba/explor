## Functions to generate plots in explor_MCA

## Variables plot reactive data
## Not exported
MCA_var_data <- function(res, xax = 1, yax = 2, var_sup = TRUE, var_sup_choice = NULL,
    var_lab_min_contrib = 0, labels_prepend_var = FALSE) {
    
    tmp_x <- res$vars %>%
        arrange(Axis, Type, Variable) %>%
        filter(Axis == xax) %>%
        select("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
    tmp_y <- res$vars %>% 
        filter(Axis == yax) %>%
        select("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2", "Count")
    if (!(var_sup) || is.null(var_sup_choice)) {
        tmp_x <- tmp_x %>% filter(Type == 'Active')
        tmp_y <- tmp_y %>% filter(Type == 'Active')
    }
    if (var_sup && !is.null(var_sup_choice)) {
        tmp_x <- tmp_x %>% filter(Type == 'Active' | Variable %in% var_sup_choice)
        tmp_y <- tmp_y %>% filter(Type == 'Active' | Variable %in% var_sup_choice)
    }
    if (labels_prepend_var) {
        tmp_x$Level <- paste(tmp_x$Variable, "-", tmp_x$Level)
        tmp_y$Level <- paste(tmp_y$Variable, "-", tmp_y$Level)
    }
    tmp <- tmp_x %>%
        left_join(tmp_y, by = c("Variable", "Level", "Type", "Class", "Count")) %>%
        mutate(Contrib = Contrib.x + Contrib.y,
               Cos2 = Cos2.x + Cos2.y,
               tooltip = paste(paste0("<strong>", Level, "</strong><br />"),
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
                                             "</strong> ", Contrib, "<br />")),
                               ifelse(is.na(Count), "",
                                      paste0("<strong>",
                                             gettext("Count:", domain = "R-explor"),
                                             "</strong> ", Count))),
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
##' @param var_sup_choice list of supplementary variables to display
##' @param var_lab_min_contrib Contribution threshold to display points labels
##' @param labels_prepend_var if TRUE, prepend variable names to labels
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
MCA_var_plot <- function(res, xax = 1, yax = 2, 
                         var_sup = TRUE, 
                         var_sup_choice = NULL,
                         var_lab_min_contrib = 0,
                         point_size = 64,
                         labels_prepend_var = FALSE,
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
    
    var_data <- MCA_var_data(res, xax, yax, var_sup, var_sup_choice, var_lab_min_contrib, labels_prepend_var)
    
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
                   type_var = ifelse(var_data[,"Class"] == "Quantitative", "arrow", "point"),
                   unit_circle = var_sup && "Quantitative" %in% var_data[,"Class"],
                   key_var = paste(var_data[, "Variable"], var_data[, "Level"], sep = "-"),
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

## MCA individuals plot data
MCA_ind_data <- function(res, xax = 1, yax = 2, ind_sup, col_var = NULL, opacity_var = NULL, ind_lab_min_contrib = 0) {
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

##' Interactive MCA indivuals plot
##'
##' This function generates an HTML widget displaying the individuals plot of an MCA result.
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
MCA_ind_plot <- function(res, xax = 1, yax = 2, ind_sup = TRUE, ind_lab_min_contrib = 0,
                         lab_var = NULL,
                         col_var = NULL,
                         symbol_var = NULL,
                         opacity_var = NULL,
                         size_var = NULL,
                         size_range = c(10,300),
                         zoom_callback = NULL,
                         in_explor = FALSE,
                         ...) {

    html_id <- if(in_explor) "explor_ind" else  NULL
    dom_id_svg_export <- if(in_explor) "explor-ind-svg-export" else NULL
    dom_id_lasso_toggle <- if(in_explor) "explor-ind-lasso-toggle" else NULL
    lasso <- if(in_explor) TRUE else FALSE 
    lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
    zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "ind") else NULL
    
    ind_data <- MCA_ind_data(res, xax, yax, ind_sup, col_var, opacity_var, ind_lab_min_contrib)
    
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

## Biplot reactive data
## Not exported
MCA_bi_data <- function(res, settings) {

    # Compute ind data
    ind_data <- MCA_ind_data(res, settings$xax, settings$yax, ind_sup = settings$ind_sup, 
        ind_lab_min_contrib = settings$bi_lab_min_contrib) 
    ind_data$source <- "ind"
    ind_data$key <- ind_data$Name
    # Compute var data
    var_sup_choice <- res$vars %>% 
        filter(Type == "Supplementary") %>%
        pull("Variable") %>%
        unique()
    var_data <- MCA_var_data(
        res, settings$xax, settings$yax, 
        var_sup = settings$var_sup, 
        var_sup_choice = var_sup_choice,
        var_lab_min_contrib = settings$bi_lab_min_contrib
    )
    var_data$source <- "var"
    var_data$key <- paste(var_data$Variable, var_data$Level, sep = "-")
    
    # Bind ind and var
    bi_data <- bind_rows(ind_data, var_data)
    bi_data$key <- paste0(bi_data$source, bi_data$key)
    ind <- bi_data$source == "ind"
    
    # Point or arrow
    bi_data$type <- ifelse(bi_data$Class == "Quantitative", "arrow", "point")
    bi_data$type[ind] <- "point"

    bi_data$Variable[ind] <- ""
    bi_data$Nature <- gettext("Variable level")
    bi_data$Nature[ind] <- gettext("Individual")
    if (!settings$ind_labels) bi_data$Lab[ind] <- ""
    bi_data$Lab[is.na(bi_data$Lab)] <- ""
    bi_data$Contrib[!ind] <- NA
    
    # Colors
    if (!is.null(settings$col_var) && settings$col_var == "Variable") {
        bi_data$color <- bi_data$Variable
        bi_data$color[ind] <- ""
    } else {
        if(is.null(settings$col_var)) {
            bi_data$color <- NULL   
        } else {
            bi_data$color <- bi_data[,settings$col_var]
        }
    }

    bi_data
}


##' Interactive MCA biplot
##'
##' This function generates an HTML widget displaying the variables plot of an MCA result.
##'
##' @param res Result of prepare_results() call
##' @param xax Horizontal axis number
##' @param yax Vertical axis number
##' @param ind_sup TRUE to display supplementary individuals
##' @param var_sup TRUE to display supplementary variables
##' @param bi_lab_min_contrib Contribution threshold to display points labels
##' @param ind_point_size base point size for individuals
##' @param var_point_size base point size for variable levels
##' @param ind_opacity individuals point opacity (constant)
##' @param ind_opacity_var individuals point opacity (variable)
##' @param ind_labels TRUE to display individuals labels
##' @param col_var name of the variable for points color
##' @param symbol_var name of the variable for points symbol
##' @param zoom_callback scatterD3 zoom callback JavaScript body
##' @param in_explor wether the plot is to be displayed in the \code{explor} interface
##' @param ... Other arguments passed to scatterD3
##'
##' @export
##' @importFrom RColorBrewer brewer.pal

MCA_biplot <- function(res, xax = 1, yax = 2, 
    col_var, ind_sup = TRUE, var_sup = TRUE, bi_lab_min_contrib = 0,
    symbol_var = NULL,
    ind_point_size = 16,
    var_point_size = 96,
    ind_opacity = 0.5,
    ind_opacity_var = NULL,
    ind_labels = FALSE,
    zoom_callback = NULL,
    in_explor = FALSE, ...) {
    
    ## Settings changed if not run in explor
    html_id <- if(in_explor) "explor_bi" else  NULL
    dom_id_svg_export <- if(in_explor) "explor-bi-svg-export" else NULL
    #dom_id_lasso_toggle <- if(in_explor) "explor-bi-lasso-toggle" else NULL
    #lasso <- if(in_explor) TRUE else FALSE 
    #lasso_callback <- if(in_explor) explor_multi_lasso_callback() else NULL
    zoom_callback <- if(in_explor) explor_multi_zoom_callback(type = "bi") else NULL
    
    settings <- list(xax = xax, yax = yax, ind_sup = ind_sup, var_sup = var_sup,
        col_var = col_var, bi_lab_min_contrib = bi_lab_min_contrib,
        ind_opacity = ind_opacity, ind_opacity_var = ind_opacity_var, ind_labels = ind_labels,
        ind_point_size = ind_point_size, var_point_size = var_point_size)
        
    bi_data <- MCA_bi_data(res, settings)
    
    colors <- NULL
    if (!is.null(col_var) && col_var == "Variable") {
        n_colors <- nlevels(bi_data$color)
        if (n_colors <= 11) {
            colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
        } else {
            colors <- RColorBrewer::brewer.pal(n_colors - 1, "Paired")
        }
        colors <- c("#666666", colors)
    }
    
    if (is.null(ind_opacity_var)) {
        opacity_var <- bi_data$source
        opacities <- c("ind" = ind_opacity, "var" = 1)
    } else {
        opacity_var <- bi_data[,ind_opacity_var]
        opacity_var[bi_data$source == "var"] <- max(opacity_var, na.rm = TRUE)
        opacities <- NULL
    }
    sizes <- c("ind" = ind_point_size, "var" = var_point_size)
    
    scatterD3::scatterD3(
        x = bi_data[, "Coord.x"],
        y = bi_data[, "Coord.y"],
        xlab = names(res$axes)[res$axes == xax],
        ylab = names(res$axes)[res$axes == yax],
        lab = bi_data$Lab,
        col_var = bi_data$color,
        col_lab = col_var,
        colors = colors,
        symbol_var = if (is.null(symbol_var)) NULL else bi_data[,symbol_var],
        symbol_lab = symbol_var,
        size_var = bi_data$source,
        sizes = sizes,
        size_lab = NA,
        opacity_var = opacity_var,
        opacities = opacities,
        tooltip_text = bi_data[, "tooltip"],
        type_var = bi_data$type,
        unit_circle = var_sup && "Quantitative" %in% bi_data[,"Class"],
        key_var = bi_data$key,
        fixed = TRUE,
        html_id = html_id,
        dom_id_svg_export = dom_id_svg_export,
        dom_id_lasso_toggle = NULL,
        lasso = FALSE,
        lasso_callback = NULL,
        zoom_callback = zoom_callback,
        ...
    )  
}


