##' @rdname explor
##' @aliases explor.MCA
##' @export

explor.MCA <- function(obj) {
    
    if (!inherits(obj, "MCA")) stop("obj must be of class MCA")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Level", "Coord", "Contrib", "Cos2", "Count")
    settings$varsup_columns <- c("Variable", "Level", "Class", "Coord", "Cos2", "Count", "V.test", "P.value")
    settings$vareta2_columns <- c("Variable", "eta2")
    settings$varsupeta2_columns <- c("Variable", "eta2")
    settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
    settings$indsup_columns <- c("Name", "Coord")
    settings$obj_name <- deparse(substitute(obj))

    settings$has_count <- TRUE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- TRUE
    settings$has_varsup_eta2 <- TRUE
    
    ## Launch interface
    explor_multi_mca(res, settings)
    
}

##' @rdname explor
##' @aliases explor.speMCA
##' @export

explor.speMCA <- function(obj) {
    
    if (!inherits(obj, "speMCA")) stop("obj must be of class speMCA")

    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Level", "Coord", "Contrib", "Cos2")
    settings$varsup_columns <- c("Variable", "Level", "Class", "Coord", "Cos2", "V.test", "P.value")
    settings$vareta2_columns <- c("Variable", "eta2")
    settings$varsupeta2_columns <- c("Variable", "eta2")
    settings$ind_columns <- c("Name", "Coord", "Contrib")
    settings$indsup_columns <- c("Name", "Coord", "Cos2")
    settings$obj_name <- deparse(substitute(obj))

    settings$has_count <- FALSE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- TRUE
    settings$has_varsup_eta2 <- FALSE
    
    ## Launch interface
    explor_multi_mca(res, settings)
    
}

##' @rdname explor
##' @aliases explor.mca
##' @export

explor.mca <- function(obj) {
    
    if (!inherits(obj, "mca")) stop("obj must be of class mca")

    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Level", "Coord")
    settings$varsup_columns <- c("Variable", "Level", "Class", "Coord")
    settings$ind_columns <- c("Name", "Coord")
    settings$indsup_columns <- c("Name", "Coord")
    settings$obj_name <- deparse(substitute(obj))

    settings$has_count <- FALSE
    settings$has_contrib <- FALSE
    settings$has_cos2 <- FALSE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE
    
    ## Launch interface
    explor_multi_mca(res, settings)
    
}


##' @rdname explor
##' @aliases explor.acm
##' @details 
##' If you want to display supplementary individuals or variables and you're using
##' the \code{\link[ade4]{dudi.acm}} function, you can add the coordinates of 
##' \code{\link[ade4]{suprow}} and/or \code{\link[ade4]{supcol}} to as \code{supi} and/or 
##' \code{supv} elements added to your \code{\link[ade4]{dudi.acm}} result (See example).
##' @export
##' @examples
##' \dontrun{
##' 
##' library(ade4)
##' data(banque)
##' d <- banque[-(1:100),-(19:21)]
##' ind_sup <- banque[1:100, -(19:21)]
##' var_sup <- banque[-(1:100),19:21]
##' acm <- dudi.acm(d, scannf = FALSE, nf = 5)
##' acm$supv <- supcol(acm, dudi.acm(var_sup, scannf = FALSE, nf = 5)$tab)$cosup
##' colw <- acm$cw*ncol(d)
##' X <- acm.disjonctif(ind_sup)
##' X <- data.frame(t(t(X)/colw) - 1)
##' acm$supi <- suprow(acm, X)$lisup
##' explor(acm)
##' }


explor.acm <- function(obj) {
    
    if (!inherits(obj, "acm") || !inherits(obj, "dudi")) stop("obj must be of class dudi and acm")
    
    ## results preparation
    res <- prepare_results(obj)

    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Level", "Coord", "Contrib", "Cos2")
    settings$varsup_columns <- c("Variable", "Level", "Coord")
    settings$vareta2_columns <- c("Variable", "eta2")
    settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
    settings$indsup_columns <- c("Name", "Coord")
    settings$obj_name <- deparse(substitute(obj))

    settings$has_count <- FALSE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- TRUE
    settings$has_varsup_eta2 <- FALSE
    
    ## Launch interface
    explor_multi_mca(res, settings)
    
}




##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2
##' @importFrom highr hi_html

explor_multi_mca <- function(res, settings) { 
    
    settings$has_sup_vars <- "Supplementary" %in% res$vars$Type
    settings$has_sup_ind <- "Supplementary" %in% res$ind$Type
    settings$type <- "MCA"
    



    shiny::shinyApp(
               ui = navbarPage(gettext("MCA", domain = "R-explor"),
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
                                                       sliderInput("var_point_size", 
                                                                   gettext("Points size", domain = "R-explor"),
                                                                   4, 128, 56),
                                                       if (settings$has_contrib) {
                                                           numericInput("var_lab_min_contrib",
                                                                    gettext("Minimum contribution to show label", domain = "R-explor"),
                                                                    min = 0, max = ceiling(2*max(res$vars$Contrib, na.rm = TRUE)), value = 0) },
                                                       explor_multi_var_col_input(settings),
                                                       explor_multi_var_symbol_input(settings),
                                                       explor_multi_var_size_input(settings),
                                                       if (settings$has_sup_vars) checkboxInput("var_sup", 
                                                                                       HTML(gettext("Supplementary variables",
                                                                                                    domain = "R-explor")), 
                                                                                       value = TRUE),
                                                       explor_multi_sidebar_footer(type = "var"))),
                                            column(10,
                                                   scatterD3Output("varplot", height = "auto"))
                                        )),
                               
                               tabPanel(gettext("Variables data", domain = "R-explor"),
                                        explor_multi_var_dataUI("var_data", settings, res$axes)),
                               
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
                                                       explor_multi_ind_col_input(settings, res),
                                                       checkboxInput("ind_ellipses", 
                                                                     HTML(gettext("Ellipses", domain = "R-explor")),
                                                                     value = FALSE),
                                                       if (settings$has_sup_ind)
                                                           checkboxInput("ind_sup", 
                                                                         HTML(gettext("Supplementary individuals", domain = "R-explor")),
                                                                         value = TRUE),
                                                       explor_multi_sidebar_footer(type = "ind"))),
                                            column(10,
                                                   scatterD3Output("indplot")))),
                               tabPanel(gettext("Individuals data", domain = "R-explor"),
                                        explor_multi_ind_dataUI("ind_data", settings, res$axes))
                               ),
               
               server = function(input, output) {

                   ## Eigenvalues
                   callModule(explor_multi_eigenplot,
                              "eigenplot",
                              reactive(res$eig))
                   
                   ## Variables plot code
                   varplot_code <- reactive({
                       col_var <- if (input$var_col == "None") NULL else input$var_col
                       symbol_var <- if (input$var_symbol == "None") NULL else input$var_symbol
                       size_var <- if (!is.null(input$var_size) && input$var_size != "None") input$var_size else NULL
                       size_range <- if (!is.null(input$var_size) && input$var_size != "None") c(30,400) * input$var_point_size / 32 else c(10,300)
                       var_lab_min_contrib <- if(settings$has_contrib) input$var_lab_min_contrib else 0
                       
                       paste0("explor::MCA_var_plot(res",
                              ", xax = ", input$var_x, ", yax = ", input$var_y, ",\n",
                              "    var_sup = ", settings$has_sup_vars && input$var_sup,
                              ", var_lab_min_contrib = ", var_lab_min_contrib, ",\n",
                              "    col_var = ", deparse(substitute(col_var)),
                              ", symbol_var = ", deparse(substitute(symbol_var)), ",\n",
                              "    size_var = ", deparse(substitute(size_var)),
                              ", size_range = ", deparse(size_range), ",\n",
                              "    labels_size = ", input$var_lab_size,
                              ", point_size = ", input$var_point_size, ",\n",
                              "    transitions = ", input$var_transitions,
                              ", labels_positions = NULL")
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
                           title = gettext("Export R code", domain="R-explor"),
                           HTML(paste0(explor_multi_export_code_message(),
                                       "<pre><code>",
                                       paste(highr::hi_html(code), collapse="\n"),
                                       "</code></pre>")),
                           easyClose = TRUE))
                   })


                   ## Indidivuals plot code
                   indplot_code <- reactive({
                       col_var <- if (input$ind_col == "None") NULL else input$ind_col
                       lab_var <- if (input$ind_labels_show) "Name" else NULL
                       
                       paste0("explor::MCA_ind_plot(res, ",
                              "xax = ", input$ind_x, ", yax = ", input$ind_y, ",",
                              "ind_sup = ", settings$has_sup_ind && input$ind_sup, ",\n",
                              "    col_var = ", deparse(substitute(col_var)), ", ",
                              "lab_var = ", deparse(substitute(lab_var)), ", ",
                              "labels_size = ", input$ind_labels_size, ",\n",
                              "    point_opacity = ", input$ind_opacity, ", ",
                              "point_size = ", input$ind_point_size, ",\n",
                              "    ellipses = ", input$ind_ellipses, ", ",
                              "transitions = ", input$ind_transitions,
                              ", labels_positions = NULL")
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
                           title = gettext("Export R code", domain="R-explor"),
                           HTML(paste0(explor_multi_export_code_message(),
                                       "<pre><code>",
                                       paste(highr::hi_html(code), collapse="\n"),
                                       "</code></pre>")),
                           easyClose = TRUE))
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
                         title = gettext("Lasso selection", domain="R-explor"),                         
                         HTML(input$show_lasso_modal),
                         easyClose = TRUE
                       ))
                   })

               })
}
