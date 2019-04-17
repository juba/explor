##' @rdname explor
##' @aliases explor.MCA
##' @export

explor.MCA <- function(obj) {
    if (!inherits(obj, "MCA"))
        stop("obj must be of class MCA")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <-
        c("Variable", "Level", "Coord", "Contrib", "Cos2", "Count")
    settings$varsup_columns <-
        c("Variable",
            "Level",
            "Class",
            "Coord",
            "Cos2",
            "Count",
            "V.test",
            "P.value")
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
    if (!inherits(obj, "speMCA"))
        stop("obj must be of class speMCA")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <-
        c("Variable", "Level", "Coord", "Contrib", "Cos2")
    settings$varsup_columns <-
        c("Variable",
            "Level",
            "Class",
            "Coord",
            "Cos2",
            "V.test",
            "P.value")
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
    if (!inherits(obj, "mca"))
        stop("obj must be of class mca")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <- c("Variable", "Level", "Coord")
    settings$varsup_columns <-
        c("Variable", "Level", "Class", "Coord")
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
    if (!inherits(obj, "acm") ||
            !inherits(obj, "dudi"))
        stop("obj must be of class dudi and acm")
    
    ## results preparation
    res <- prepare_results(obj)
    
    ## Settings
    settings <- list()
    settings$var_columns <-
        c("Variable", "Level", "Coord", "Contrib", "Cos2")
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
        ui = navbarPage(
            gettext("MCA"),
            header = tags$head(tags$style(explor_multi_css())),
            
            tabPanel(
                gettext("Eigenvalues"),
                explor_multi_eigenUI("eigen", res$eig)
            ),

            ## VARIABLES PLOT UI            
            tabPanel(gettext("Variables plot"),
                fluidRow(
                    column(
                        2,
                        wellPanel(
                            explor_multi_axes_input(res, "var"),
                            sliderInput("var_lab_size",
                                gettext("Labels size"),
                                4, 20, 10),
                            explor_multi_auto_labels_input(res$vars, "var"),
                            sliderInput("var_point_size",
                                gettext("Points size"),
                                4, 128, 56),
                            explor_multi_min_contrib_input(res$vars, settings, "var"),
                            explor_multi_var_col_input(settings),
                            explor_multi_var_symbol_input(settings),
                            explor_multi_var_size_input(settings),
                            if (settings$has_sup_vars)
                                checkboxInput("var_sup",
                                    HTML(gettext(
                                        "Supplementary variables"
                                    )),
                                    value = TRUE),
                            explor_multi_sidebar_footer(type = "var")
                        )
                    ),
                    column(10,
                        scatterD3Output("varplot", height = "auto"))
                )),
            
            tabPanel(
                gettext("Variables data"),
                explor_multi_var_dataUI("var_data", settings, res$axes)
            ),
            
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
                            conditionalPanel(condition = 'input.ind_opacity_var == "Fixed"',
                                sliderInput(
                                    "ind_opacity",
                                    gettext("Fixed points opacity"),
                                    0, 1, 0.5
                                )),
                            checkboxInput("ind_labels_show",
                                HTML(gettext("Show labels")),
                                value = FALSE),
                            conditionalPanel(
                                condition = 'input.ind_labels_show == true',
                                sliderInput("ind_labels_size",
                                    gettext("Labels size"),
                                    5, 20, 9),
                                explor_multi_auto_labels_input(res$ind, "ind"),
                                explor_multi_min_contrib_input(res$ind, settings, "ind")
                            ),
                            explor_multi_ind_col_input(settings, res),
                            checkboxInput("ind_ellipses",
                                HTML(gettext("Ellipses")),
                                value = FALSE),
                            if (settings$has_sup_ind)
                                checkboxInput("ind_sup",
                                    HTML(
                                        gettext("Supplementary individuals")
                                    ),
                                    value = TRUE),
                            explor_multi_sidebar_footer(type = "ind")
                        )
                    ),
                    column(10,
                        scatterD3Output("indplot"))
                )),
            tabPanel(
                gettext("Individuals data"),
                explor_multi_ind_dataUI("ind_data", settings, res$axes)
            ),
            
            ## BIPLOT UI
            tabPanel(gettext("Biplot"),
                fluidRow(
                    column(2,
                        wellPanel(
                            explor_multi_axes_input(res, "bi"),
                            explor_multi_bi_col_input(settings),
                            explor_multi_bi_symbol_input(settings),
                            sliderInput("bi_lab_size",
                                gettext("Labels size"),
                                4, 20, 12),
                            uiOutput("bi_auto_labels"),
                            explor_multi_min_contrib_input(res$vars, settings, "bi"),
                            
                            div(id = "bi_accordion", class = "panel-group",
                                role="tablist", `aria-multiselectable`="true",
                                ## Individuals accordion
                                div(class="panel panel-default",
                                    div(class="panel-heading", role="tab", id="headingInd",
                                        h4(class = "panel-title",
                                            a(role="button", `data-toggle`="collapse", class="collapsed",
                                                `data-parent`="#bi_accordion", href="#collapseInd",
                                                `aria-expanded`="false", `aria-controls`="collapseInd",
                                                gettext("Individuals settings")
                                            )
                                        )
                                    ),
                                    div(id="collapseInd", class="panel-collapse collapse",
                                        role="tabpanel", `aria-labelledby`="headingInd",
                                        div(class="panel-body",
                                            checkboxInput("bi_ind_labels_show",
                                                HTML(gettext("Show individuals labels")),
                                                value = FALSE),
                                            sliderInput("bi_ind_point_size",
                                                gettext("Individuals point size"),
                                                4, 128, 32),
                                            explor_multi_bi_ind_opacity_input(settings),
                                            conditionalPanel(condition = 'input.bi_opacity_var == "Fixed"',
                                                sliderInput(
                                                    "bi_ind_point_opacity",
                                                    gettext("Fixed points opacity"),
                                                    0, 1, 0.5
                                                )),
                                            if (settings$has_sup_ind)
                                                checkboxInput("bi_ind_sup",
                                                    HTML(gettext(
                                                        "Supplementary individuals"
                                                    )),
                                                    value = TRUE)
                                        )
                                    )
                                ),
                                ## Variables accordion
                                div(class="panel panel-default",
                                    div(class="panel-heading", role="tab", id="headingVar",
                                        h4(class = "panel-title",
                                            a(role="button", `data-toggle`="collapse", class="collapsed",
                                                `data-parent`="#bi_accordion", href="#collapseVar",
                                                `aria-expanded`="false", `aria-controls`="collapseVar",
                                                gettext("Variables settings")
                                            )
                                        )
                                    ),
                                    div(id="collapseVar", class="panel-collapse collapse",
                                        role="tabpanel", `aria-labelledby`="headingVar",
                                        div(class="panel-body",
                                            sliderInput("bi_var_point_size",
                                                gettext("Variables point size"),
                                                4, 128, 96),
                                            if (settings$has_sup_vars)
                                                checkboxInput("bi_var_sup",
                                                    HTML(gettext(
                                                        "Supplementary variables"
                                                    )),
                                                    value = TRUE)
                                        )
                                    )
                                )
                            ),
                            explor_multi_sidebar_footer(type = "bi")
                        )
                    ),
                    column(10,
                        scatterD3Output("biplot"))
                ))
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
                size_var <- if (!is.null(input$var_size) && input$var_size != "None") input$var_size else NULL
                size_range <- if (!is.null(input$var_size) && input$var_size != "None") c(30, 400) * input$var_point_size / 32 else c(10, 300)
                var_lab_min_contrib <- if (settings$has_contrib) input$var_lab_min_contrib else 0
                var_auto_labels <- if (!is.null(input$var_auto_labels) && input$var_auto_labels) "\"auto\"" else "NULL"
                
                paste0(
                    "explor::MCA_var_plot(res",
                    ", xax = ", input$var_x,
                    ", yax = ", input$var_y,
                    ", var_sup = ", settings$has_sup_vars && input$var_sup,
                    ", var_lab_min_contrib = ", var_lab_min_contrib,
                    ", col_var = ", deparse(substitute(col_var)),
                    ", symbol_var = ", deparse(substitute(symbol_var)),
                    ", size_var = ", deparse(substitute(size_var)),
                    ", size_range = ", deparse(size_range),
                    ", labels_size = ", input$var_lab_size,
                    ", point_size = ", input$var_point_size,
                    ", transitions = ", input$var_transitions,
                    ", labels_positions = ", var_auto_labels
                )
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
                col_var <- if (input$ind_col == "None") NULL else input$ind_col
                lab_var <- if (input$ind_labels_show) "Lab" else NULL
                opacity_var <- if (!is.null(input$ind_opacity_var) && input$ind_opacity_var == "Fixed") NULL else input$ind_opacity_var
                ind_lab_min_contrib <- if (settings$has_contrib) input$ind_lab_min_contrib else 0
                ind_auto_labels <- if (!is.null(input$ind_auto_labels) && input$ind_auto_labels) "\"auto\"" else "NULL"
                
                paste0(
                    "explor::MCA_ind_plot(res, ",
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
                    ", ellipses = ", input$ind_ellipses,
                    ", transitions = ", input$ind_transitions,
                    ", labels_positions = ", ind_auto_labels
                )
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
            
            
            ## biplot code
            biplot_code <- reactive({
                bi_auto_labels <- if (!is.null(input$bi_auto_labels) && input$bi_auto_labels) "\"auto\"" else "NULL"
                bi_lab_min_contrib <- if (settings$has_contrib) input$bi_lab_min_contrib else 0
                col_var <- if (input$bi_col == "None") NULL else input$bi_col
                symbol_var <- if (input$bi_symbol == "None") NULL else input$bi_symbol
                opacity_var <- if (!is.null(input$bi_opacity_var) && input$bi_opacity_var == "Fixed") NULL else input$bi_opacity_var
                
                paste0(
                    "explor::MCA_biplot(res",
                    ", xax = ", input$bi_x,
                    ", yax = ", input$bi_y,
                    ", col_var = ", deparse(substitute(col_var)),
                    ", ind_point_size = ", input$bi_ind_point_size, 
                    ", ind_opacity = ", input$bi_ind_point_opacity, 
                    ", ind_opacity_var = ", deparse(substitute(opacity_var)), 
                    ", ind_labels = ", input$bi_ind_labels_show,
                    ", var_point_size = ", input$bi_var_point_size, 
                    ", var_sup = ", settings$has_sup_vars && input$bi_var_sup,
                    ", ind_sup = ", settings$has_sup_ind && input$bi_ind_sup,
                    ", labels_size = ", input$bi_lab_size,
                    ", bi_lab_min_contrib = ", bi_lab_min_contrib,
                    ", symbol_var = ", deparse(substitute(symbol_var)),     
                    ", transitions = ", input$bi_transitions,
                    ", labels_positions = ", bi_auto_labels
                )
            })
            
            ## Biplot
            output$biplot <- scatterD3::renderScatterD3({
                code <- paste0(biplot_code(), ", in_explor = TRUE)")
                eval(parse(text = code))
            })
            
            ## Biplot code export modal dialog
            observeEvent(input$explor_bi_plot_code, {
                showModal(code_modal(settings$obj_name, 
                    biplot_code(),
                    explor_multi_zoom_code(input$bi_zoom_range)
                ))
            })
            
            ## Only show automatic label placement checkbox if less than 100 labels
            output$bi_auto_labels <- renderUI({
                n_label <- sum(res$vars$Level[res$vars$Axis == 1] != "")
                if (input$bi_ind_labels_show) {
                    n_label <- n_label + sum(res$ind$Name[res$ind$Axis == 1] != "")
                }
                if (n_label > 100) return(NULL)
                explor_multi_auto_labels_input(res$vars, "bi")
            })

            ## Variable data module
            callModule(explor_multi_var_data,
                "var_data",
                reactive(res),
                reactive(settings))
            
            ## Individual data module            
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
