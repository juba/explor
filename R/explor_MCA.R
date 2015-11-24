##' @rdname explor
##' @aliases explor.MCA
##' @details 
##' Interface for multiple correspondence analysis
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive visualisation and exploration of a multiple correspondence analysis.
##' @return
##' The function launches a shiny app in the system web browser.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @seealso \code{\link[FactoMineR]{MCA}}
##' @import shiny
##' @import dplyr
##' @import scatterD3
##' @import ggplot2
##' @export

explor.MCA <- function(obj) {
  
  if (!inherits(obj, "MCA")) stop("obj must be of class MCA")
  
  res <- prepare_results(obj)
  has_sup_vars <- "Supplementary" %in% res$vars$Type 
  has_sup_ind <- "Supplementary" %in% res$ind$Type

  css_string <- "
  .well label, 
  .well input, 
  .well select, 
  .well option,
  .well button,
  .well a,
  input, label, select, option, .selectize-input {
      font-size: 11px !important;
      height: auto !important;
  }
  .well .checkbox { margin-left: 5px !important; }
  .well {padding: 5px !important;}
  .dataTable th, 
  .dataTable td {
      font-size: 11px !important;
      padding: 3px 5px !important; 
  }
  .dataTable th { padding-right: 18px !important }
  .dataTables_wrapper {
    max-width: 850px;
    margin-bottom: 2em;
  }
  .dataTables_info, .dataTables_length, 
  .dataTables_filter, .dataTables_paginate {
      font-size: 11px !important;
  }
  #varplot, #indplot { height: 90vh !important}
  #eigplot { max-width: 850px; }
  .legend .label { font-weight: normal !important; font-size: 10px !important;}
  "
  
  shiny::shinyApp(
    ui = navbarPage("iMCA",
                  header = tags$head(
                  tags$style(HTML(css_string))),
                  tabPanel("Eigenvalues",
                           fluidRow(
                             column(2,
                                    wellPanel(numericInput("eig_nb", "Dimensions to plot", 
                                                 min = 2, max = max(res$eig$dim), value = max(res$eig$dim), 
                                                 step = 1))),
                            column(10,
                                    plotOutput("eigplot", height = "600px"))
                             
                             )),
                  
                  tabPanel("Variables plot",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                      selectInput("var_x", "X axis", choices = res$axes, selected = "1"),
                                      selectInput("var_y", "Y axis", choices = res$axes, selected = "2"),
                                      sliderInput("var_lab_size", "Labels size", 4, 20, 10),
                                      selectInput("var_col", "Points color :",
                                                  choices = c("None" = "None",
                                                              "Variable name" = "Variable",
                                                              "Variable type" = "Type"),
                                                  selected = "Variable"),
                                      selectInput("var_symbol", "Points symbol :",
                                                  choices = c("None" = "None",
                                                              "Variable name" = "Variable",
                                                              "Variable type" = "Type"),
                                                  selected = "Type"),
                                      selectInput("var_size", "Points size :",
                                                  choices = c("None" = "None",
                                                              "Contribution" = "Contrib",
                                                              "Cos2" = "Cos2"),
                                                  selected = "None"),
                                      checkboxInput("var_sup", HTML("Supplementary variables"), value = TRUE),
                                      checkboxInput("var_transitions", HTML("Animations"), value = TRUE),
                                      tags$p(actionButton("imca-var-reset-zoom", 
                                                          title = "Reset zoom",
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             tags$a(id = "imca-var-svg-export", href = "#",
                                                    class = "btn btn-default", 
                                                    title = "Export as SVG",
                                                    HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span>"))))),
                             column(10,
                                    scatterD3Output("varplot", height = "auto"))
                  )),
                  
                  tabPanel("Variables data",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("vardim", "Dimension", choices = res$axes, selected = "1"),
                                    textInput("varpvalue", "Max p-value", 0.1))),
                             column(10,
                                    # h4("Positive coordinates"),
                                    # DT::dataTableOutput("vartablepos"),
                                    # h4("Negative coordinates"),                   
                                    # DT::dataTableOutput("vartableneg"),
                                    h4("Active variables"),                   
                                    DT::dataTableOutput("vartable"),
                                    if (has_sup_vars) {
                                      list(h4("Supplementary variables"),                   
                                           DT::dataTableOutput("vartablesup"))
                                    },
                                    h4("Variables eta2"),                   
                                    DT::dataTableOutput("vartableeta2"),
                                    if (has_sup_vars) {
                                      list(h4("Supplementary variables eta2"),                   
                                           DT::dataTableOutput("vartablesupeta2"))
                                    }
                             ))),
                  
                  tabPanel("Individuals plot",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                      selectInput("ind_x", "X axis", choices = res$axes, selected = "1"),
                                      selectInput("ind_y", "Y axis", choices = res$axes, selected = "2"),
                                      sliderInput("ind_point_size", "Points size", 8, 128, 64),
                                      sliderInput("ind_opacity", "Points opacity", 0, 1, 0.5),
                                      checkboxInput("ind_labels_show", HTML("Show labels"), value = FALSE),                                                                  conditionalPanel(
                                        condition = 'input.ind_labels_show == true',
                                        sliderInput("ind_labels_size", "Labels size", 5, 20, 9)
                                      ),
                                      selectInput("ind_col", "Points color :",
                                                  choices = c("None" = "None",
                                                              "Individual type" = "Type"),
                                                  selected = "Type"),
                                      checkboxInput("ind_sup", HTML("Supplementary individuals"), value = TRUE),
                                      checkboxInput("ind_transitions", HTML("Animations"), value = TRUE),          
                                      tags$p(actionButton("imca-ind-reset-zoom", 
                                                          title = "Reset zoom",
                                                          HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>")),
                                             tags$a(id = "imca-ind-svg-export", href = "#",
                                                    class = "btn btn-default", 
                                                    title = "Export as SVG",
                                                    HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span>"))))),                                 column(10,
                                    scatterD3Output("indplot")))),
                  
                  tabPanel("Individuals data",
                           fluidRow(
                             column(2,
                                    wellPanel(
                                    selectInput("inddim", "Dimension", choices = res$axes, selected = "Axis 1"))),
                             column(10,
                                    # h4("Positive coordinates"),
                                    # DT::dataTableOutput("indtablepos"),
                                    # h4("Negative coordinates"),                   
                                    # DT::dataTableOutput("indtableneg"),
                                    h4("Active individuals"),
                                    DT::dataTableOutput("indtable"),
                                    if (has_sup_ind) {
                                      list(h4("Supplementary individuals"),                   
                                      DT::dataTableOutput("indtablesup"))
                                    }
                             )))
                  
    ),
    
    server = function(input, output) {
      
      output$eigplot <- renderPlot({
        tmp <- res$eig[1:input$eig_nb,]
        tmp$dim <- factor(tmp$dim)
        ggplot(data = tmp) +
          geom_bar(aes_string(x = "dim", y = "percent"), stat = "identity") +
          scale_x_discrete("Axis") +
          scale_y_discrete("Percentage of inertia")
      })

      ## Variables plot reactive data
      var_data <- reactive({
        tmp_x <- res$vars %>% 
          filter(Axis == input$var_x) %>%
          select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2")
        if (!input$var_sup)
          tmp_x <- tmp_x %>% filter(Type == 'Active')
        tmp_y <- res$vars %>% 
          filter(Axis == input$var_y) %>%
          select_("Variable", "Level", "Type", "Class", "Coord", "Contrib", "Cos2")
        if (!input$var_sup)
          tmp_y <- tmp_y %>% filter(Type == 'Active')
        tmp <- tmp_x %>% 
          left_join(tmp_y, by = c("Variable", "Level", "Type", "Class")) %>%
          mutate(Contrib = Contrib.x + Contrib.y,
                 Cos2 = Cos2.x + Cos2.y,
                 tooltip = paste(paste0("<strong>", Level, "</strong>"),
                                 paste0("<strong>Variable:</strong>", Variable),
                                 paste0("<strong>x:</strong> ", Coord.x),
                                 paste0("<strong>y:</strong> ", Coord.y),
                                 paste0("<strong>Cos2:</strong> ", Cos2),
                                 paste0("<strong>Contribution:</strong> ", Contrib),
                                 sep = "<br />"))
        data.frame(tmp)
      })
      
      ## Variables plot
      output$varplot <- scatterD3::renderScatterD3({
        col_var <- if (input$var_col == "None") NULL else var_data()[, input$var_col]
        symbol_var <- if (input$var_symbol == "None") NULL else var_data()[, input$var_symbol]
        size_var <- if (input$var_size == "None") NULL else var_data()[, input$var_size]
        type_var <- ifelse(var_data()[,"Class"] == "Quantitative", "arrow", "point")
        scatterD3::scatterD3(
          x = var_data()[, "Coord.x"],
          y = var_data()[, "Coord.y"],
          xlab = names(res$axes)[res$axes == input$var_x],
          ylab = names(res$axes)[res$axes == input$var_y],
          lab = var_data()[, "Level"],
          labels_size = input$var_lab_size,
          point_opacity = 1,
          col_var = col_var,
          col_lab = input$var_col,
          symbol_var = symbol_var,
          symbol_lab = input$var_symbol,
          size_var = size_var,
          size_lab = input$var_size,
          tooltip_text = var_data()[, "tooltip"],
          type_var = type_var,
          key_var = var_data()[, "Level"],
          fixed = TRUE,
          transitions = input$var_transitions,
          html_id = "imca_var",
          dom_id_reset_zoom = "imca-var-reset-zoom",
          dom_id_svg_export = "imca-var-svg-export"
        )
      })
      

      
      ## Individuals plot reactive data
      ind_data <- reactive({
        tmp_x <- res$ind %>% 
          filter(Axis == input$ind_x) %>%
          select(Name, Type, Coord, Contrib, Cos2)
        if (!input$ind_sup)
          tmp_x <- tmp_x %>% filter(Type == "Active")
        tmp_y <- res$ind %>% 
          filter(Axis == input$ind_y) %>%
          select(Name, Type, Coord, Contrib, Cos2)
        if (!input$ind_sup)
          tmp_y <- tmp_y %>% filter(Type == "Active")
        tmp <- tmp_x %>% 
          left_join(tmp_y, by = c("Name", "Type")) %>%
          mutate(Contrib = Contrib.x + Contrib.y,
                 Cos2 = Cos2.x + Cos2.y,
                 tooltip = paste(paste0("<strong>", Name, "</strong>"),
                                 paste0("<strong>x:</strong> ", Coord.x),
                                 paste0("<strong>y:</strong> ", Coord.y),
                                 paste0("<strong>Cos2:</strong> ", Cos2),
                                 paste0("<strong>Contribution:</strong> ", Contrib),
                                 sep = "<br />"))
        data.frame(tmp)
      })
      
      ## Individuals plot
      output$indplot <- scatterD3::renderScatterD3({
        col_var <- if (input$ind_col == "None") NULL else ind_data()[, input$ind_col]
        lab_var <- if (input$ind_labels_show) ind_data()[, "Name"] else NULL
        scatterD3::scatterD3(
          x = ind_data()[, "Coord.x"],
          y = ind_data()[, "Coord.y"],
          xlab = names(res$axes)[res$axes == input$ind_x],
          ylab = names(res$axes)[res$axes == input$ind_y],
          point_size = input$ind_point_size,
          point_opacity = input$ind_opacity,
          lab = lab_var,
          labels_size = input$ind_labels_size,
          col_var = col_var,
          col_lab = input$ind_col,
          tooltip_text = ind_data()[, "tooltip"],
          key_var = ind_data()[, "Name"],
          fixed = TRUE,
          transitions = input$ind_transitions,
          html_id = "imca_ind",
          dom_id_reset_zoom = "imca-ind-reset-zoom",
          dom_id_svg_export = "imca-ind-svg-export"
        )
      })
      
      tableOptions_var <- list(lengthMenu =  c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = FALSE)
      tableOptions_ind <- list(lengthMenu = c(10,20,50,100), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)
      tableOptions_eta2 <- list(lengthMenu = c(10,20,50), pageLength = 10, orderClasses = TRUE, autoWidth = TRUE, searching = FALSE)
      
      # ## Variables, positive coordinates
      # varTablePos <- reactive({
      #   res$vars %>% 
      #     filter(Type == "Active", Axis == input$vardim, 
      #            Coord >= 0, P.value <= as.numeric(input$varpvalue)) %>%
      #     select(-Type, -Class, -Axis)
      # })
      # output$vartablepos <- DT::renderDataTable(
      #   DT::datatable({varTablePos()}, 
      #                 options=c(tableOptions_var,list(order=list(list(3,'desc')))),rownames=FALSE))
      # 
      # ## Variables, negative coordinates
      # varTableNeg <- reactive({
      #   res$vars %>% 
      #     filter(Type == "Active", Axis == input$vardim, 
      #            Coord < 0, P.value <= as.numeric(input$varpvalue)) %>%
      #     select(-Type, -Class, -Axis)
      # })
      # output$vartableneg <- DT::renderDataTable(
      #   DT::datatable({varTableNeg()}, 
      #                 options=c(tableOptions_var,list(order=list(list(3,'desc')))),rownames=FALSE))      
      ## Active variables
      varTable <- reactive({
        res$vars %>% 
          filter(Type == "Active", Axis == input$vardim, 
                 P.value <= as.numeric(input$varpvalue)) %>%
          select(-Type, -Class, -Axis)
      })
      output$vartable <- DT::renderDataTable(
        DT::datatable({varTable()}, 
                      options=c(tableOptions_var,list(order=list(list(3,'desc')))),rownames=FALSE))
      
      ## Supplementary variables
      varTableSup <- reactive({
        res$vars %>% 
          filter(Type == "Supplementary", Axis == input$vardim, 
                 P.value <= as.numeric(input$varpvalue) | Class == "Quantitative") %>%
          mutate(Level=ifelse(Class=="Quantitative", "-", Level)) %>%
          select(-Type, -Axis, -Contrib)
      })
      output$vartablesup <- DT::renderDataTable(
        DT::datatable({varTableSup()}, 
                      options = c(tableOptions_var,list(order = list(list(2,'desc')))),rownames = FALSE))
      
                  
      ## Variables eta2
      varTableEta2 <- reactive({
        tmp <- res$vareta2 %>% filter(Type == "Active", Axis == input$vardim) %>%
          select(-Type, -Class, -Axis) %>% arrange(eta2)
      })
      output$vartableeta2 <- DT::renderDataTable(
        DT::datatable({varTableEta2()}, 
                      options=tableOptions_eta2,rownames=FALSE))
      
      ## Supplementary variables eta2
      varTableSupEta2 <- reactive({
        tmp <- res$vareta2 %>% filter(Type == "Supplementary", 
                                      Class == "Qualitative", 
                                      Axis == input$vardim) %>%
          select(-Type, -Axis) %>% arrange(eta2)
      })
      output$vartablesupeta2 <- DT::renderDataTable(
        DT::datatable({varTableSupEta2()}, 
                      options=tableOptions_eta2,rownames=FALSE))
      
      # ## Individuals, positive coordinates
      # indTablePos <- reactive({
      #   res$ind %>% 
      #     filter(Type == "Active", Axis == input$inddim, 
      #            Coord >= 0) %>%
      #     select(-Type, -Axis)
      # })
      # output$indtablepos = DT::renderDataTable(
      #   DT::datatable({indTablePos()}, 
      #                 options=c(tableOptions_ind,list(order=list(list(1,'desc')))),rownames=FALSE))
      # 
      # ## Individuals, negative coordinates
      # indTableNeg <- reactive({
      #   res$ind %>% 
      #     filter(Type == "Active", Axis == input$inddim, 
      #            Coord < 0) %>%
      #     select(-Type, -Axis)
      # })
      # output$indtableneg = DT::renderDataTable(
      #   DT::datatable({indTableNeg()}, 
      #                 options=c(tableOptions_ind,list(order=list(list(1,'asc')))),rownames=FALSE))

      ## Active individuals
      indTable <- reactive({
        res$ind %>% 
          filter(Type == "Active", Axis == input$inddim) %>%
          select(-Type, -Axis)
      })
      output$indtable = DT::renderDataTable(
        DT::datatable({indTable()}, 
                      options=c(tableOptions_ind,list(order=list(list(2,'desc')))),rownames=FALSE))

      
      ## Supplementary individuals
      indTableSup <- reactive({
        res$ind %>% 
          filter(Type == "Supplementary", Axis == input$inddim) %>%
          select(-Type, -Axis, -Contrib)
      })
      output$indtablesup = DT::renderDataTable(
        DT::datatable({indTableSup()}, 
                      options = c(tableOptions_ind, list(order = list(list(1,'asc')))), rownames = FALSE))
      
          
    }
  )
}
