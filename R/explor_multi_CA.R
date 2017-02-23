##' @rdname explor
##' @aliases explor.CA
##' @export

explor.CA <- function(obj) {
    
    if (!inherits(obj, "CA")) stop("obj must be of class CA")
    
    ## results preparation
    res <- prepare_results(obj)

    ## Settings
    settings <- list()
    settings$var_columns <- c("Level", "Position", "Coord", "Contrib", "Cos2", "Count")
    settings$varsup_columns <- c("Level", "Position", "Coord", "Cos2", "Count")
    settings$obj_name <- deparse(substitute(obj))    

    settings$has_count <- TRUE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE

    
    ## Launch interface
    explor_multi_ca(res, settings)
    
}


##' @rdname explor
##' @aliases explor.coa
##' @details 
##' If you want to display supplementary individuals or variables and you're using
##' the \code{\link[ade4]{dudi.coa}} function, you can add the coordinates of 
##' \code{\link[ade4]{suprow}} and/or \code{\link[ade4]{supcol}} to as \code{supr} and/or 
##' \code{supr} elements added to your \code{\link[ade4]{dudi.coa}} result (See example).
##' @export
##' @examples
##' \dontrun{
##' 
##' library(ade4)
##' 
##' data(bordeaux)
##' tab <- bordeaux
##' row_sup <- tab[5,-4]
##' col_sup <- tab[-5,4]
##' coa <- dudi.coa(tab[-5,-4], nf = 5, scannf = FALSE)
##' coa$supr <- suprow(coa, row_sup)$lisup
##' coa$supc <- supcol(coa, col_sup)$cosup
##' explor(coa)
##' }


explor.coa <- function(obj) {
    
    if (!inherits(obj, "coa") || !inherits(obj, "dudi"))
        stop("obj must be of class dudi and coa")
    
    ## results preparation
    res <- prepare_results(obj)

    ## Settings
    settings <- list()
    settings$var_columns <- c("Level", "Position", "Coord", "Contrib", "Cos2")
    settings$varsup_columns <- c("Level", "Position", "Coord")
    settings$obj_name <- deparse(substitute(obj))

    settings$has_count <- FALSE
    settings$has_contrib <- TRUE
    settings$has_cos2 <- TRUE
    settings$has_var_eta2 <- FALSE
    settings$has_varsup_eta2 <- FALSE

    ## Launch interface
    explor_multi_ca(res, settings)
    
}




##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2

explor_multi_ca <- function(res, settings) { 
    
    ## Precompute inputs 
    settings$has_sup_vars <- "Supplementary" %in% res$vars$Type
    settings$type <- "CA"
    
    shiny::shinyApp(
               ui = navbarPage(gettext("CA", domain = "R-explor"),
                               header = tags$head(
                                                 tags$style(explor_multi_css())),
                               
                               tabPanel(gettext("Eigenvalues", domain = "R-explor"),
                                        explor_multi_eigenUI("eigen", res$eig)),

                               tabPanel(gettext("Plot", domain = "R-explor"),
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
                                                                   0, 20, 10),
                                                       sliderInput("var_point_size", 
                                                                   gettext("Points size", domain = "R-explor"),
                                                                   4, 128, 56),                                       
                                                       numericInput("var_lab_min_contrib",
                                                                    gettext("Minimum contribution to show label", domain = "R-explor"),
                                                                    min = 0, max = ceiling(2*max(res$vars$Contrib, na.rm = TRUE)), value = 0),
                                                       explor_multi_var_col_input(settings),
                                                       explor_multi_var_symbol_input(settings),
                                                       explor_multi_var_size_input(settings),
                                                       selectInput("var_hide", 
                                                                   gettext("Hide :", domain = "R-explor"),
                                                                   choices = explor_multi_hide_choices(),
                                                                   selected = "None"),
                                                       if(settings$has_sup_vars)
                                                           checkboxInput("var_sup", 
                                                                         HTML(gettext("Supplementary levels", domain = "R-explor")),
                                                                         value = TRUE),
                                                       explor_multi_sidebar_footer(type = "var"))),
                                            column(10,
                                                   scatterD3Output("varplot", height = "auto"))
                                        )),
                               
                               tabPanel(gettext("Data", domain = "R-explor"),
                                        explor_multi_var_dataUI("var_data", settings, res$axes))
                               ),
               
               server = function(input, output) {

                   ## Eigenvalues
                   callModule(explor_multi_eigen,
                              "eigen",
                              reactive(res$eig))

                   
                   ## Variables plot code
                   varplot_code <- reactive({
                       col_var <- if (input$var_col == "None") NULL else input$var_col
                       symbol_var <- if (input$var_symbol == "None") NULL else input$var_symbol
                       size_var <- if (input$var_size == "None") NULL else input$var_size
                       size_range <- if (input$var_size == "None") c(10,300) else c(30,400) * input$var_point_size / 32

                       paste0("explor::CA_var_plot(res, ",
                              "xax = ", input$var_x, ", yax = ", input$var_y, ",\n",
                              "    var_sup = ", settings$has_sup_vars && input$var_sup, ", ",
                              "var_hide = '", input$var_hide, "', ",
                              "var_lab_min_contrib = ", input$var_lab_min_contrib, ",\n",
                              "    col_var = ", deparse(substitute(col_var)), ", ", 
                              "symbol_var = ", deparse(substitute(symbol_var)), ", ",
                              "size_var = ", deparse(substitute(size_var)), ",\n",
                              "    size_range = ", deparse(size_range), ", ",
                              "labels_size = ", input$var_lab_size, ", ",
                              "point_size = ", input$var_point_size, ",\n",
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

                   
                   callModule(explor_multi_var_data,
                              "var_data",
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
                   
               }
           )
}
