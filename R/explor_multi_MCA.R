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
    settings$show_varsup_eta2 <- TRUE
    settings$counts_size <- TRUE    
    settings$varsupeta2_columns <- c("Variable", "eta2")
    settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
    settings$indsup_columns <- c("Name", "Coord")
    settings$obj_name <- deparse(substitute(obj))

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
    settings$show_varsup_eta2 <- FALSE
    settings$counts_size <- FALSE   
    settings$varsupeta2_columns <- c("Variable", "eta2")
    settings$ind_columns <- c("Name", "Coord", "Contrib")
    settings$indsup_columns <- c("Name", "Coord")
    settings$obj_name <- deparse(substitute(obj))

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
    settings$show_varsup_eta2 <- FALSE
    settings$counts_size <- FALSE
    settings$ind_columns <- c("Name", "Coord", "Contrib", "Cos2")
    settings$indsup_columns <- c("Name", "Coord")
    settings$obj_name <- deparse(substitute(obj))
    
    ## Launch interface
    explor_multi_mca(res, settings)
    
}




##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2
##' @importFrom highr hi_html

explor_multi_mca <- function(res, settings) { 
    
    ## Precompute inputs 
    has_sup_vars <- "Supplementary" %in% res$vars$Type 
    if (has_sup_vars) {
        choices <- c("None", "Variable", "Type")
        names(choices) <- c(gettext("None", domain = "R-explor"),
                            gettext("Variable name", domain = "R-explor"),
                            gettext("Variable type", domain = "R-explor"))
        symbol_selected <- "Type"
    } else {
        choices <- c("None", "Variable")
        names(choices) <- c(gettext("None", domain = "R-explor"),
                            gettext("Variable name", domain = "R-explor"))
        symbol_selected <- "None"
    }
    ## Variable color input
    var_col_input <- selectInput("var_col", gettext("Points color :", domain = "R-explor"),
                                 choices = choices,  selected = "Variable")
    ## Variable symbol input
    var_symbol_input <- selectInput("var_symbol", gettext("Points symbol :", domain = "R-explor"),
                                    choices = choices, selected = symbol_selected)
    ## Variable size input
    var_size_choices <- c("None", "Contrib", "Cos2")
    names(var_size_choices) <- c(gettext("None", domain = "R-explor"),
                                 gettext("Contribution", domain = "R-explor"),
                                 gettext("Squared cosinus", domain = "R-explor"))
    if (settings$counts_size) {
        var_size_choices <- c(var_size_choices, "Count")
        names(var_size_choices)[var_size_choices == "Count"] <- gettext("Count", domain = "R-explor")
    }  
    var_size_input <- selectInput("var_size", 
                                  gettext("Points size :", domain = "R-explor"),
                                  choices = var_size_choices,
                                  selected = "None")
    ## Individual color input
    ind_col_choices <- c("None", "Type")
    names(ind_col_choices) <- c(gettext("None", domain = "R-explor"),
                                gettext("Individual type", domain = "R-explor"))
    ind_col_choices <- c(ind_col_choices, names(res$quali_data))
    ind_col_choices <- setdiff(ind_col_choices, "Name")
    ind_col_input <- selectInput("ind_col", 
                                 gettext("Points color :", domain = "R-explor"),
                                 choices = ind_col_choices,
                                 selected = "None")

    has_sup_ind <- "Supplementary" %in% res$ind$Type

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
                                                       numericInput("var_lab_min_contrib",
                                                                    gettext("Minimum contribution to show label", domain = "R-explor"),
                                                                    min = 0, max = ceiling(2*max(res$vars$Contrib, na.rm = TRUE)), value = 0),
                                                       var_col_input,
                                                       var_symbol_input,
                                                       var_size_input,
                                                       if (has_sup_vars) checkboxInput("var_sup", 
                                                                                       HTML(gettext("Supplementary variables",
                                                                                                    domain = "R-explor")), 
                                                                                       value = TRUE),
                                                       explor_multi_sidebar_footer(type = "var"))),
                                            column(10,
                                                   scatterD3Output("varplot", height = "auto"))
                                        )),
                               
                               tabPanel(gettext("Variables data", domain = "R-explor"),
                                        explor_multi_var_dataUI("var_data", has_sup_vars, res$axes, is_MCA = TRUE)),
                               
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
                                                       ind_col_input,
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
                       col_var <- if (input$var_col == "None") NULL else input$var_col
                       symbol_var <- if (input$var_symbol == "None") NULL else input$var_symbol
                       size_var <- if (input$var_size == "None") NULL else input$var_size
                       size_range <- if (input$var_size == "None") c(10,300) else c(30,400) * input$var_point_size / 32
                       
                       paste0("explor::MCA_var_plot(res",
                              ", xax = ", input$var_x, ", yax = ", input$var_y, ",\n",
                              "    var_sup = ", has_sup_vars && input$var_sup, ", ",
                              ", var_lab_min_contrib = ", input$var_lab_min_contrib, ",\n",
                              "    col_var = ", deparse(substitute(col_var)),
                              ", symbol_var = ", deparse(substitute(symbol_var)), ",\n",
                              "    size_var = ", deparse(substitute(size_var)),
                              ", size_range = ", deparse(dput(size_range)), ",\n",
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
                           title = "Export R code",
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
                              "ind_sup = ", has_sup_ind && input$ind_sup, ",\n",
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
                              is_MCA = TRUE)
                   
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

               })
}
