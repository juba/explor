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

    ## Launch interface
    explor_multi_pca(res, settings)
    
}



##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2

explor_multi_pca <- function(res, settings) {
    
    ## Precompute inputs 
    has_sup_vars <- "Supplementary" %in% res$vars$Type
    has_quali_sup_vars <- any("Supplementary" %in% res$vars$Type &
                              "Qualitative" %in% res$vars$Class)
    ## Variable color input
    if (has_sup_vars) {
        ## Qualitative supplementary
        if (has_quali_sup_vars) {
            choices <- c("None", "Type", "Variable")
            names(choices) <- c(gettext("None", domain = "R-explor"),
                                gettext("Variable type", domain = "R-explor"),
                                gettext("Variable name", domain = "R-explor"))
        }
        ## Only quantitative supplementary
        else {
            choices <- c("None", "Type")
            names(choices) <- c(gettext("None", domain = "R-explor"),
                                gettext("Variable type", domain = "R-explor"))
        }
        var_col_input <- selectInput("var_col", 
                                     gettext("Points color :", domain = "R-explor"),
                                     choices = choices,  selected = "Type")
    } else {
        var_col_input <- NULL
    }

    ## Individual color input
    ind_col_choices <- c("None", "Type")
    names(ind_col_choices) <- c(gettext("None", domain = "R-explor"),
                                gettext("Individual type", domain = "R-explor"))
    if (has_quali_sup_vars) {
        ind_col_choices <- c(ind_col_choices, names(res$quali_data))
        ind_col_choices <- setdiff(ind_col_choices, "Name")
    }
    ind_col_input <- selectInput("ind_col", 
                                 gettext("Points color :", domain = "R-explor"),
                                 choices = ind_col_choices,
                                 selected = "Type")
    
    
    
    has_sup_ind <- "Supplementary" %in% res$ind$Type

    shiny::shinyApp(
               ui = navbarPage(gettext("PCA", domain = "R-explor"),
                               header = tags$head(
                                                 tags$style(explor_multi_css())),
                               tabPanel(gettext("Eigenvalues", domain = "R-explor"),
                                        explor_multi_eigenplotUI("eigenplot", res$eig)),
                               
                               tabPanel(gettext("Variables plot", domain = "R-explor"),
                                        fluidRow(
                                            column(2,
                                                   wellPanel(
                                                       selectInput("var_x", 
                                                                   gettext("X axis", domain = "R-explor"), 
                                                                   choices = res$axes, selected = "1"),
                                                       selectInput("var_y", 
                                                                   gettext("Y axis", domain = "R-explor"), 
                                                                   choices = res$axes, selected = "2"),
                                                       sliderInput("var_lab_size", 
                                                                   gettext("Labels size", domain = "R-explor"),
                                                                   4, 20, 10),
                                                       numericInput("var_lab_min_contrib",
                                                                    gettext("Minimum contribution to show label", domain = "R-explor"),
                                                                    min = 0, max = ceiling(2*max(res$vars$Contrib, na.rm = TRUE)), value = 0),
                                                       if (has_sup_vars) var_col_input,
                                                       if (has_sup_vars)
                                                           checkboxInput("var_sup", 
                                                                         HTML(gettext("Supplementary variables", domain = "R-explor")), 
                                                                         value = TRUE),
                                                       explor_multi_sidebar_footer(type = "var"))),
                                            column(10,
                                                   scatterD3Output("varplot", height = "auto"))
                                        )),
                               
                               tabPanel(gettext("Variables data", domain = "R-explor"),
                                        explor_multi_var_dataUI("var_data", has_sup_vars, res$axes, PCA_quali = has_quali_sup_vars)),

                               tabPanel(gettext("Individuals plot", domain = "R-explor"),
                                        fluidRow(
                                            column(2,
                                                   wellPanel(
                                                       selectInput("ind_x", 
                                                                   gettext("X axis", domain = "R-explor"),
                                                                   choices = res$axes, selected = "1"),
                                                       selectInput("ind_y", 
                                                                   gettext("Y axis", domain = "R-explor"),
                                                                   choices = res$axes, selected = "2"),
                                                       sliderInput("ind_point_size", 
                                                                   gettext("Points size", domain = "R-explor"),
                                                                   8, 128, 64),
                                                       sliderInput("ind_opacity", 
                                                                   gettext("Points opacity", domain = "R-explor"),
                                                                   0, 1, 0.5),
                                                       checkboxInput("ind_labels_show", 
                                                                     HTML(gettext("Show labels", domain = "R-explor")),
                                                                     value = FALSE),
                                                       conditionalPanel(
                                                           condition = 'input.ind_labels_show == true',
                                                           sliderInput("ind_labels_size", 
                                                                       gettext("Labels size", domain = "R-explor"),
                                                                       5, 20, 9)
                                                       ),
                                                       if (has_sup_ind) 
                                                           ind_col_input,
                                                       if (has_sup_ind) 
                                                           checkboxInput("ind_ellipses", 
                                                                         HTML(gettext("Ellipses", domain = "R-explor")),
                                                                         value = FALSE),
                                                       if (has_sup_ind)
                                                           checkboxInput("ind_sup", 
                                                                         HTML(gettext("Supplementary individuals", domain = "R-explor")),
                                                                         value = TRUE),
                                                       explor_multi_sidebar_footer(type = "ind"))),
                                            column(10,
                                                   scatterD3Output("indplot")))),
                               tabPanel(gettext("Individuals data", domain = "R-explor"),
                                        explor_multi_ind_dataUI("ind_data", has_sup_ind, res$axes))
                               ),
               
               server = function(input, output) {

                   ## Eigenvalues
                   callModule(explor_multi_eigenplot,
                              "eigenplot",
                              reactive(res$eig))

                   
                   ## Variables plot code
                   varplot_code <- reactive({
                       col_var <- if (!is.null(input$var_col) && input$var_col == "None") NULL else input$var_col
                       
                       paste0("explor::PCA_var_plot(res, ",
                              "xax = ", input$var_x, ", yax = ", input$var_y, ",\n",
                              "    var_sup = ", has_sup_vars && input$var_sup, ", ",
                              "var_lab_min_contrib = ", input$var_lab_min_contrib, ",\n",
                              "    col_var = ", deparse(substitute(col_var)), ", ",
                              "labels_size = ", input$var_lab_size, ", ",
                              "scale_unit = ", settings$scale_unit, ",\n",
                              "    transitions = ", input$var_transitions)
                   })
                   
                   ## Variables plot
                   output$varplot <- scatterD3::renderScatterD3({
                       code <- paste0(varplot_code(), ", in_explor = TRUE)")        
                       eval(parse(text = code))
                   })
                   
                   ## Variables plot code export modal dialog
                   observeEvent(input$explor_var_plot_code, {
                       code <- paste0("res <- explor::prepare_results(", settings$obj_name, ")\n")
                       code <- paste0(code, varplot_code())
                       code <- paste0(code, explor_multi_zoom_code(input$var_zoom_range), ")")

                       showModal(modalDialog(
                           title = "Export R code",
                           HTML(explor_multi_export_code_message(),
                                paste0("<pre><code>",
                                       paste(highr::hi_html(code), collapse="\n"),
                                       "</code></pre>")),
                           easyClose = TRUE))
                   })


                   ## Indidivuals plot code
                   indplot_code <- reactive({
                       col_var <- if (!is.null(input$ind_col) && input$ind_col == "None") NULL else input$ind_col
                       lab_var <- if (input$ind_labels_show) "Name" else NULL
                       ellipses <- !is.null(input$ind_ellipses) && input$ind_ellipses
                       paste0("explor::PCA_ind_plot(res, ",
                              "xax = ", input$ind_x, ", yax = ", input$ind_y, ",",
                              "ind_sup = ", has_sup_ind && input$ind_sup, ",\n",
                              "    col_var = ", deparse(substitute(col_var)), ", ",
                              "lab_var = ", deparse(substitute(lab_var)), ", ",
                              "labels_size = ", input$ind_labels_size, ",\n",
                              "    point_opacity = ", input$ind_opacity, ", ",
                              "point_size = ", input$ind_point_size, ",\n",
                              "    ellipses = ", ellipses, ", ",
                              "transitions = ", input$ind_transitions)
                   })
                   
                   ## Indidivuals plot
                   output$indplot <- scatterD3::renderScatterD3({
                       code <- paste0(indplot_code(), ", in_explor = TRUE)")        
                       eval(parse(text = code))
                   })
                   
                   ## Indidivuals plot code export modal dialog
                   observeEvent(input$explor_ind_plot_code, {
                       code <- paste0("res <- explor::prepare_results(", settings$obj_name, ")\n")
                       code <- paste0(code, indplot_code())
                       code <- paste0(code, explor_multi_zoom_code(input$ind_zoom_range), ")")

                       showModal(modalDialog(
                           title = "Export R code",
                           HTML(paste0(explor_multi_export_code_message(),
                                       "<pre><code>",
                                       paste(highr::hi_html(code), collapse="\n"),
                                       "</code></pre>")),
                           easyClose = TRUE))
                   })

                   
                   callModule(explor_multi_var_data,
                              "var_data",
                              reactive(res),
                              reactive(settings),
                              PCA_quali = has_quali_sup_vars)

                   callModule(explor_multi_ind_data,
                              "ind_data",
                              reactive(res),
                              reactive(settings))
                   
                   ## Lasso modal dialog
                   observeEvent(input$show_lasso_modal, {
                       showModal(modalDialog(
                           title = "Lasso selection",
                           HTML(input$show_lasso_modal),
                           easyClose = TRUE
                       ))
                   })
                   
               }
           )
}
