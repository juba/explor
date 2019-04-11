##' @rdname explor
##' @aliases explor.PCA
##' @export

explor.PCA <- function(obj) {
    
    if (!inherits(obj, "PCA")) stop("obj must be of class PCA")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Coord", "Contrib", "Cos2", "Cor")
    settings$varsup_columns <- c("Variable", "Coord", "Cos2", "Cor")
    settings$varsup_quali_columns <- c("Variable", "Level", "Coord", "Cos2", "V.test", "P.value")
    settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
    settings$indsup_columns <- c("Name", "Coord", "Cos2")
    settings$scale_unit <- obj$call$scale.unit
    settings$obj_name <- deparse(substitute(obj))    
    
    settings$has_count <- FALSE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE
    
    
    ## Launch interface
    explor_multi_pca(res, settings)
    
}

##' @rdname explor
##' @aliases explor.princomp
##' @export

explor.princomp <- function(obj) {
    
    if (!inherits(obj, "princomp")) stop("obj must be of class princomp")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Coord")
    settings$varsup_columns <- c("Variable", "Coord")
    settings$ind_columns <- c("Name", "Coord")
    settings$indsup_columns <- c("Name", "Coord")
    settings$scale_unit <- obj$call$cor
    settings$obj_name <- deparse(substitute(obj))    
    
    settings$has_count <- FALSE
    settings$has_contrib <- FALSE
    settings$has_cos2 <- FALSE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE
    
    
    ## Launch interface
    explor_multi_pca(res, settings)
    
}

##' @rdname explor
##' @aliases explor.prcomp
##' @export

explor.prcomp <- function(obj) {
    
    if (!inherits(obj, "prcomp")) stop("obj must be of class prcomp")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Coord")
    settings$varsup_columns <- c("Variable", "Coord")
    settings$ind_columns <- c("Name", "Coord")
    settings$indsup_columns <- c("Name", "Coord")
    settings$scale_unit <- obj$scale != FALSE
    settings$obj_name <- deparse(substitute(obj))    
    
    settings$has_count <- FALSE
    settings$has_contrib <- FALSE
    settings$has_cos2 <- FALSE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE
    
    
    ## Launch interface
    explor_multi_pca(res, settings)
    
}


##' @rdname explor
##' @aliases explor.pca
##' @details 
##' If you want to display supplementary individuals or variables and you're using
##' the \code{\link[ade4]{dudi.pca}} function, you can add the coordinates of 
##' \code{\link[ade4]{suprow}} and/or \code{\link[ade4]{supcol}} to as \code{supi} and/or 
##' \code{supv} elements added to your \code{\link[ade4]{dudi.pca}} result (See example).
##' @export
##' @examples
##' \dontrun{
##' 
##' library(ade4)
##' data(deug)
##' d <- deug$tab
##' sup_var <- d[-(1:10), 8:9]
##' sup_ind <- d[1:10, -(8:9)]
##' pca <- dudi.pca(d[-(1:10), -(8:9)], scale = TRUE, scannf = FALSE, nf = 5)
##' supi <- suprow(pca, sup_ind)
##' pca$supi <- supi$lisup
##' supv <- supcol(pca, dudi.pca(sup_var, scale = TRUE, scannf = FALSE)$tab)
##' pca$supv <- supv$cosup
##' explor(pca)
##' }


explor.pca <- function(obj) {
    
    if (!inherits(obj, "pca") || !inherits(obj, "dudi")) stop("obj must be of class dudi and pca")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Coord", "Contrib", "Cos2")
    settings$varsup_columns <- c("Variable", "Coord")
    settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
    settings$indsup_columns <- c("Name", "Coord")
    settings$scale_unit <- if (is.null(obj$call$scale)) TRUE else obj$call$scale
    settings$obj_name <- deparse(substitute(obj))
    
    settings$has_count <- FALSE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE
    
    ## Launch interface
    explor_multi_pca(res, settings)
    
}



##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2

explor_multi_pca <- function(res, settings) {
    
    ## Precompute inputs 
    settings$has_sup_vars <- "Supplementary" %in% res$vars$Type
    settings$has_quali_sup_vars <- any("Supplementary" %in% res$vars$Type &
            "Qualitative" %in% res$vars$Class)
    settings$has_sup_ind <- "Supplementary" %in% res$ind$Type
    settings$type <- "PCA"
    
    shiny::shinyApp(
        ui = navbarPage(gettext("PCA"),
            header = tags$head(
                tags$style(explor_multi_css())),
            
            tabPanel(gettext("Eigenvalues"),
                explor_multi_eigenUI("eigen", res$eig)),
            
            ## VARIABLES PLOT UI
            tabPanel(gettext("Variables plot"),
                fluidRow(
                    column(2,
                        wellPanel(
                            explor_multi_axes_input(res, "var"),
                            sliderInput("var_lab_size", 
                                gettext("Labels size"),
                                4, 20, 10),
                            explor_multi_min_contrib_input(res$vars, settings, "var"),
                            if (settings$has_sup_vars) explor_multi_var_col_input(settings),
                            if (settings$has_sup_vars)
                                checkboxInput("var_sup", 
                                    HTML(gettext("Supplementary variables")), 
                                    value = TRUE),
                            explor_multi_sidebar_footer(type = "var"))),
                    column(10,
                        scatterD3Output("varplot", height = "auto"))
                )),
            
            tabPanel(gettext("Variables data"),
                explor_multi_var_dataUI("var_data", settings, res$axes)),
            
            ## INDIVIDUALS PLOT UI
            tabPanel(gettext("Individuals plot"),
                fluidRow(
                    column(2,
                        wellPanel(
                            explor_multi_axes_input(res, "ind"),
                            sliderInput("ind_point_size", 
                                gettext("Points size"),
                                8, 128, 64),
                            explor_multi_ind_opacity_input(settings),
                            conditionalPanel(
                                condition = 'input.ind_opacity_var == "Fixed"',
                                sliderInput("ind_opacity", 
                                    gettext("Fixed points opacity"),
                                    0, 1, 0.5)
                            ),
                            checkboxInput("ind_labels_show", 
                                HTML(gettext("Show labels")),
                                value = FALSE),
                            conditionalPanel(
                                condition = 'input.ind_labels_show == true',
                                sliderInput("ind_labels_size", 
                                    gettext("Labels size"),
                                    5, 20, 9),
                                explor_multi_auto_labels_input(res$ind, "ind"),
                                explor_multi_min_contrib_input(res$ind, settings, "ind")),
                            if (settings$has_sup_ind || settings$has_quali_sup_vars) 
                                explor_multi_ind_col_input(settings, res),
                            if (settings$has_sup_ind || settings$has_quali_sup_vars) 
                                checkboxInput("ind_ellipses", 
                                    HTML(gettext("Ellipses")),
                                    value = FALSE),
                            if (settings$has_sup_ind)
                                checkboxInput("ind_sup", 
                                    HTML(gettext("Supplementary individuals")),
                                    value = TRUE),
                            explor_multi_sidebar_footer(type = "ind"))),
                    column(10,
                        scatterD3Output("indplot")))),
            tabPanel(gettext("Individuals data"),
                explor_multi_ind_dataUI("ind_data", settings, res$axes))
        ),
        
        server = function(input, output) {
            
            ## Eigenvalues
            callModule(explor_multi_eigen,
                "eigen",
                reactive(res$eig))
            
            ## Variables plot code
            varplot_code <- reactive({
                col_var <- if (!is.null(input$var_col) && input$var_col == "None") NULL else input$var_col
                
                paste0("explor::PCA_var_plot(res, ",
                    "xax = ", input$var_x, 
                    ", yax = ", input$var_y,
                    ", var_sup = ", settings$has_sup_vars && input$var_sup,
                    ", var_lab_min_contrib = ", input$var_lab_min_contrib,
                    ", col_var = ", deparse(substitute(col_var)),
                    ", labels_size = ", input$var_lab_size,
                    ", scale_unit = ", settings$scale_unit,
                    ", transitions = ", input$var_transitions,
                    ", labels_positions = NULL")
            })
            
            ## Variables plot
            output$varplot <- scatterD3::renderScatterD3({
                code <- paste0(varplot_code(), ", in_explor = TRUE)")        
                eval(parse(text = code))
            })
            
            ## Variables plot code export modal dialog
            observeEvent(input$explor_var_plot_code, {
                showModal(code_modal(settings$obj_name, 
                    varplot_code(),
                    explor_multi_zoom_code(input$var_zoom_range)
                ))
            })
            
            
            ## Indidivuals plot code
            indplot_code <- reactive({
                col_var <- if (!is.null(input$ind_col) && input$ind_col == "None") NULL else input$ind_col
                lab_var <- if (input$ind_labels_show) "Lab" else NULL
                opacity_var <- if (!is.null(input$ind_opacity_var) && input$ind_opacity_var == "Fixed") NULL else input$ind_opacity_var
                ellipses <- !is.null(input$ind_ellipses) && input$ind_ellipses
                ind_lab_min_contrib <- if (settings$has_contrib) input$ind_lab_min_contrib else 0
                ind_auto_labels <- if (!is.null(input$ind_auto_labels) && input$ind_auto_labels) "\"auto\"" else "NULL"
                
                
                paste0("explor::PCA_ind_plot(res, ",
                    "xax = ", input$ind_x, 
                    ", yax = ", input$ind_y,
                    ", ind_sup = ", settings$has_sup_ind && input$ind_sup,
                    ", lab_var = ", deparse(substitute(lab_var)),
                    ", ind_lab_min_contrib = ", ind_lab_min_contrib,
                    ", col_var = ", deparse(substitute(col_var)),
                    ", labels_size = ", input$ind_labels_size,
                    ", point_opacity = ", input$ind_opacity,
                    ", opacity_var = ", deparse(substitute(opacity_var)),
                    ", point_size = ", input$ind_point_size,
                    ", ellipses = ", ellipses,
                    ", transitions = ", input$ind_transitions,
                    ", labels_positions = ", ind_auto_labels)
            })
            
            ## Indidivuals plot
            output$indplot <- scatterD3::renderScatterD3({
                code <- paste0(indplot_code(), ", in_explor = TRUE)")        
                eval(parse(text = code))
            })
            
            ## Indidivuals plot code export modal dialog
            observeEvent(input$explor_ind_plot_code, {
                showModal(code_modal(settings$obj_name, 
                    indplot_code(),
                    explor_multi_zoom_code(input$ind_zoom_range)
                ))
            })
            
            
            callModule(explor_multi_var_data,
                "var_data",
                reactive(res),
                reactive(settings))
            
            callModule(explor_multi_ind_data,
                "ind_data",
                reactive(res),
                reactive(settings))
            
            ## Lasso modal dialog
            observeEvent(input$show_lasso_modal, {
                showModal(modalDialog(
                    title = gettext("Lasso selection"),
                    HTML(input$show_lasso_modal),
                    easyClose = TRUE
                ))
            })
            
        }
    )
}
