##' @rdname explor
##' @aliases explor.CA
##' @export

explor.CA <- function(obj) {
  
  if (!inherits(obj, "CA")) stop("obj must be of class CA")
  
  ## results preparation
  res <- prepare_results(obj)

  ## Settings
  settings <- list()
  settings$var_columns <- c("Level", "Position", "Coord", "Contrib", "Cos2")
  settings$varsup_columns <- c("Level", "Position", "Coord", "Cos2")
  
  ## Launch interface
  explor_ca(res, settings)
  
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
  
  if (!inherits(obj, "coa") || !inherits(obj, "dudi")) stop("obj must be of class dudi and coa")
  
  ## results preparation
  res <- prepare_results(obj)

  ## Settings
  settings <- list()
  settings$var_columns <- c("Level", "Position", "Coord", "Contrib", "Cos2")
  settings$varsup_columns <- c("Level", "Position", "Coord")

  ## Launch interface
  explor_ca(res, settings)
  
}




##' @import shiny
##' @import shinyBS
##' @import dplyr
##' @import scatterD3
##' @import ggplot2

explor_ca <- function(res, settings) { 
    
  ## Precompute inputs 
  has_sup_vars <- "Supplementary" %in% res$vars$Type
  
  if (has_sup_vars) {
      choices <- c("None", "Position", "Type")
      names(choices) <- c(gettext("None", domain = "R-explor"),
                          gettext("Variable position", domain = "R-explor"),
                          gettext("Variable type", domain = "R-explor"))
      symbol_selected <- "Type"
  } else {
    choices <- c("None", "Position")
    names(choices) <- c(gettext("None", domain = "R-explor"),
                        gettext("Variable position", domain = "R-explor"))
    symbol_selected <- "None"
  }
  ## Variable color input
  var_col_input <- selectInput("var_col", gettext("Points color :", domain = "R-explor"),
                               choices = choices,  selected = "Position")
  ## Variable symbol input
  var_symbol_input <- selectInput("var_symbol", gettext("Points symbol :", domain = "R-explor"),
                                  choices = choices, selected = symbol_selected)
  ## Variable size input
  var_size_choices <- c("None", "Contrib", "Cos2")
  names(var_size_choices) <- c(gettext("None", domain = "R-explor"),
                               gettext("Contribution", domain = "R-explor"),
                               gettext("Squared cosinus", domain = "R-explor"))
  var_size_input <- selectInput("var_size", 
                                gettext("Points size :", domain = "R-explor"),
                                choices = var_size_choices,
                                selected = "None")
  ## Variable hide elements input
  var_hide_choices <- c("None", "Row", "Column")
  names(var_hide_choices) <- c(gettext("None", domain = "R-explor"),
                               gettext("Rows", domain = "R-explor"),
                               gettext("Columns", domain = "R-explor"))
  var_hide_input <- selectInput("var_hide", 
                                gettext("Hide :", domain = "R-explor"),
                                choices = var_hide_choices,
                                selected = "None")
  var_tab_hide_input <- selectInput("var_tab_hide", 
                                    gettext("Hide :", domain = "R-explor"),
                                    choices = var_hide_choices,
                                    selected = "None")
  
  
    shiny::shinyApp(
    ui = navbarPage(gettext("CA", domain = "R-explor"),
                  header = tags$head(
                  tags$style(explor_css())),
                  tabPanel(gettext("Eigenvalues", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(numericInput("eig_nb", 
                                                           gettext("Dimensions to plot", domain = "R-explor"), 
                                                 min = 2, max = max(res$eig$dim), value = max(res$eig$dim), 
                                                 step = 1))),
                            column(10,
                                    plotOutput("eigplot", height = "600px"))
                             
                             )),
                  
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
                                      var_col_input,
                                      var_symbol_input,
                                      var_size_input,
                                      var_hide_input,
                                      if(has_sup_vars)
                                        checkboxInput("var_sup", 
                                                      HTML(gettext("Supplementary levels", domain = "R-explor")), 
                                                      value = TRUE),                                      
                                      checkboxInput("var_transitions", 
                                                    HTML(gettext("Animations", domain = "R-explor")),
                                                    value = TRUE),
                                      tags$p(actionButton("explor-var-reset-zoom", 
                                                          title = gettext("Reset zoom", domain = "R-explor"),
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             tags$a(id = "explor-var-svg-export", href = "#",
                                                    class = "btn btn-default", 
                                                    title = gettext("Export as SVG", domain = "R-explor"),
                                                    HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span>"))))),
                             column(10,
                                    scatterD3Output("varplot", height = "auto"))
                  )),
                  
                  tabPanel(gettext("Data", domain = "R-explor"),
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("vardim", 
                                                gettext("Dimension", domain = "R-explor"),
                                                choices = res$axes, selected = "1"),
                                    var_tab_hide_input                                    
                                    )),
                             column(10,
                                    h4(gettext("Active levels", domain = "R-explor")),                   
                                    DT::dataTableOutput("vartable"),
                                    if (has_sup_vars) {
                                      list(h4(gettext("Supplementary levels", domain = "R-explor")),
                                           DT::dataTableOutput("vartablesup"))
                                    }
                              ))),
                  footer = shinyBS::bsModal(id = "lasso-modal", trigger = NULL,
                                            title = gettext("Selected points", domain = "R-explor"), 
                                            tags$p(id = "lasso-mod-content"))
    ),
    
    server = function(input, output) {
      
      output$eigplot <- renderPlot({
        tmp <- res$eig[1:input$eig_nb,]
        tmp$dim <- factor(tmp$dim)
        ggplot(data = tmp) +
          geom_bar(aes_string(x = "dim", y = "percent"), stat = "identity") +
          scale_x_discrete(gettext("Axis", domain = "R-explor")) +
          scale_y_continuous(gettext("Percentage of inertia", domain = "R-explor"))
      })

      ## Variables plot reactive data
      var_data <- reactive({
        tmp_x <- res$vars %>% 
          filter(Axis == input$var_x) %>%
          select_("Level", "Position", "Type", "Class", "Coord", "Contrib", "Cos2")
        tmp_y <- res$vars %>% 
          filter(Axis == input$var_y) %>%
          select_("Level", "Position", "Type", "Class", "Coord", "Contrib", "Cos2")
        if (is.null(input$var_sup) || !input$var_sup) {
          tmp_x <- tmp_x %>% filter(Type == 'Active')
          tmp_y <- tmp_y %>% filter(Type == 'Active')
        }
        if (input$var_hide != "None") {
          tmp_x <- tmp_x %>% filter(Position != input$var_hide)
          tmp_y <- tmp_y %>% filter(Position != input$var_hide)
        }
          
        tmp <- tmp_x %>%
          left_join(tmp_y, by = c("Level", "Position", "Type", "Class")) %>%
          mutate(Contrib = Contrib.x + Contrib.y,
                 Cos2 = Cos2.x + Cos2.y,
                 tooltip = paste(paste0("<strong>", Level, "</strong>"),
                                 paste0("<strong>",
                                        gettext("Position", domain = "R-explor"),
                                        ":</strong> ", Position),
                                  paste0("<strong>x:</strong> ", Coord.x),
                                  paste0("<strong>y:</strong> ", Coord.y),
                                  paste0("<strong>",
                                         gettext("Cos2", domain = "R-explor"),
                                         ":</strong> ", Cos2),
                                  paste0("<strong>",
                                         gettext("Contribution:", domain = "R-explor"),
                                         "</strong> ", Contrib),
                                  sep = "<br />"),
                 Lab = ifelse(Contrib >= as.numeric(input$var_lab_min_contrib) | 
                                  (is.na(Contrib) & as.numeric(input$var_lab_min_contrib) == 0), Level, ""))                 
        data.frame(tmp)
      })
      
      ## Variables plot
      output$varplot <- scatterD3::renderScatterD3({
        col_var <- if (input$var_col == "None") NULL else var_data()[, input$var_col]
        symbol_var <- if (input$var_symbol == "None") NULL else var_data()[, input$var_symbol]
        size_var <- if (input$var_size == "None") NULL else var_data()[, input$var_size]
        size_range <- if (input$var_size == "None") c(10,300) else c(30,400) * input$var_point_size / 32
        lab  <- if (input$var_lab_size > 0) var_data()[, "Lab"] else NULL
        key_var <- paste(var_data()[, "Position"], var_data()[, "Level"], sep="-")
        scatterD3::scatterD3(
          x = var_data()[, "Coord.x"],
          y = var_data()[, "Coord.y"],
          xlab = names(res$axes)[res$axes == input$var_x],
          ylab = names(res$axes)[res$axes == input$var_y],
          lab = lab,
          labels_size = input$var_lab_size,
          point_opacity = 1,
          point_size = input$var_point_size,          
          col_var = col_var,
          col_lab = input$var_col,
          symbol_var = symbol_var,
          symbol_lab = input$var_symbol,
          size_var = size_var,
          size_lab = input$var_size,
          size_range = size_range,
          tooltip_text = var_data()[, "tooltip"],
          type_var = "point",
          unit_circle = NULL,
          key_var = key_var,
          fixed = TRUE,
          transitions = input$var_transitions,
          html_id = "explor_var",
          dom_id_reset_zoom = "explor-var-reset-zoom",
          dom_id_svg_export = "explor-var-svg-export",
          lasso = TRUE,
          lasso_callback = explor_lasso_callback()
        )
      })
      

      tableOptions_var <- list(lengthMenu =  c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)

      ## Generate correct datatable order option from a column name
      order_option <- function(table, name, order="desc") {
        index <- which(names(table) == name) - 1
        list(order = list(list(index, order)))
      }
      
      varTable <- reactive({
        tmp <- res$vars %>% 
          filter(Type == "Active", Axis == input$vardim) %>%
          select_(.dots = settings$var_columns)
        if (input$var_tab_hide != "None") {
          tmp <- tmp %>% filter(Position != input$var_tab_hide)
        }
        data.frame(tmp)
      })
      output$vartable <- DT::renderDataTable(
        DT::datatable({varTable()}, 
                      options = c(tableOptions_var, order_option(varTable(), "Contrib")), rownames = FALSE))
      
      ## Supplementary variables
      varTableSup <- reactive({
        tmp <- res$vars %>% 
          filter(Type == "Supplementary", Axis == input$vardim) %>%
          mutate(Level = ifelse(Class == "Quantitative", "-", Level)) %>%
          select_(.dots = settings$varsup_columns)
        if (input$var_tab_hide != "None") {
          tmp <- tmp %>% filter(Position != input$var_tab_hide)
        }
        data.frame(tmp)
      })
      output$vartablesup <- DT::renderDataTable(
        DT::datatable({varTableSup()}, 
                      options = c(tableOptions_var, order_option(varTableSup(), "Coord")), rownames = FALSE))
      
    }
  )
}
