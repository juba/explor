if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term"))

##' @rdname explor
##' @aliases explor.dfm
##' @export

explor.dfm <- function(obj, ...) {
    
    if (!inherits(obj, "dfm")) stop(gettext("obj must be of class dfm", domain = "R-explor"))
    
    ## Settings
    settings <- list()

    ## Launch interface
    explor_dfm(obj)
    
}


## Custom CSS
explor_dfm_css <- function() {
  shiny::HTML("
              body {margin: 0 15px;}
              
              #sidebar {padding: 5px 10px;}

              #sidebar label, 
              #sidebar input, 
              #sidebar select, 
              #sidebar option,
              #sidebar button,
              #sidebar a,
              div.option,
              input, label, select, option, .selectize-input {
                  font-size: 12px !important;
                  height: auto !important;
              }

              #sidebar .btn { padding: 6px 10px; }

              #sidebar .shiny-input-checkboxgroup .shiny-options-group {
                  max-height: 200px; 
                  overflow-y: scroll; 
              }

              #sidebar .shiny-input-container {
                  margin-bottom: 0px;
              }

              #filters .shiny-input-checkboxgroup .shiny-options-group,
              #filters .shiny-input-container > .irs,
              #filters .shiny-input-container > .input-daterange {
                  margin: 8px 5px;
              }

              #filters .shiny-input-container {
                  border-bottom: 1px solid #BBB;
                  margin: 0px;
                  padding: 5px 4px;
                  background-color: #FAFAFA;
              }

              #filters .shiny-input-container:first-child {
                  border-top: 1px solid #BBB;
                  margin-top: 30px;
              }

              #filters {
                  margin-bottom: 30px;
              }

              #filters .shiny-input-container > label {
                  cursor: pointer;
                  font-size: 14px !important;
                  font-weight: normal;
                  width: 100%;
              }

              #filters .shiny-input-container label  i {
                  min-width: 1.3em;
              }

              #sidebar h4 {
                  margin-top: 30px;
              }

              #sidebar h4:first-child {
                  margin-top: 10px;
              }

              ul.nav-pills {
                  margin-top: 15px;
                  margin-bottom: 30px;
              }

              .checkbox { margin-bottom: 3px;}
              
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

              .inline-small * {
                  display: inline;    
                  font-size: 80% !important;
              }
              .inline-small .btn {
                  padding: 3px 5px;
              }

              /* Syntax highlighting */
              span.hl.str { color: #d14;}
              span.hl.kwa { color: #099;}
              span.hl.num { color: #099;}
              span.hl.kwd { color: #333; font-weight: bold;}
              span.hl.com { color: #888; font-style: italic;}
              ")
    }

# Custom JavaScript
explor_dfm_js <- function() {
  shiny::HTML("
(function($) {

var triggered = 0;

$('#filters').on('shiny:visualchange', function(event) {

  triggered += 1;

  if (triggered == 3) {
    $('#filters .shiny-options-group').hide();
    $('#filters .shiny-input-container > .irs').hide();
    $('#filters .shiny-input-container > .input-daterange').hide();

    $('#filters .shiny-input-container > label').prepend('<i class=\"fa fa-chevron-right\"></i> ');

    $('#filters .shiny-input-container > label').click(function() {
      $(this).next().toggle('fast');
      $(this).find('i').toggleClass('fa-chevron-right');
      $(this).find('i').toggleClass('fa-chevron-down');
    });
  };

});
    

})(jQuery);
  ")
}



##' @import shiny
##' @import quanteda
##' @importFrom highr hi_html

explor_dfm <- function(dfm, settings) { 
    

    ## Document level variables
    vars <- lapply(docvars(dfm), unique)
    nvalues <- lapply(vars, length)
    vars <- vars[nvalues > 1 & nvalues < 100]


    shiny::shinyApp(
      ui = navbarPage(gettext("Dfm", domain = "R-explor"),
                      header = tags$head(
                        tags$style(explor_corpus_css())),
                      tabPanel(gettext("Terms", domain = "R-explor"),
                         sidebarLayout(
                           sidebarPanel(id = "sidebar",
                                        numericInput("term_min_occurrences", gettext("Filter terms on minimal frequency", domain = "R-explor"), 0, 0, 1000, 1),
                                        tags$script(explor_corpus_js())
                           ),
                           mainPanel(
                             tabsetPanel(
                               
                               ## "Frequent terms" tab
                               tabPanel(gettext("Frequent terms", domain = "R-explor"),
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        p(HTML("<strong>", gettext("Number of documents", domain = "R-explor"), "&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
                                        DT::dataTableOutput("freqtable")),
                               
                               ## "Terms search" tab                      
                               tabPanel(gettext("Terms search", domain = "R-explor"),
                                        h3(gettext("Terms selection", domain = "R-explor")),
                                        HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :', domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(8, textInput("terms", gettext("Terms", domain = "R-explor"), width = "100%"))),
                                        fluidRow(
                                            column(4, selectInput("term_group",
                                                                gettext("Group by", domain = "R-explor"),
                                                                choices = names(vars)))),
                                        tags$p(actionButton("launch_search", gettext("Search", domain = "R-explor"))),
                                        uiOutput("termsAlert"),
                                        uiOutput("evalAlert"),
                                        h3(gettext("Selected terms frequency", domain = "R-explor")),
                                        htmlOutput("freqterm_query"),
                                        htmlOutput("freqterm_total"),
                                        tabsetPanel(type = "pills",
                                                    tabPanel(gettext("Table", domain = "R-explor"),
                                                             DT::dataTableOutput("freqtermtable")
                                                    ),
                                                    tabPanel(gettext("Plot", domain = "R-explor"),
                                                             plotOutput("freqtermplot")
                                                    )
                                        )
                               ),
                               
                               ## "Similarities" tab
                               tabPanel(gettext("Similarities", domain = "R-explor"),
                                        h3(gettext("Term", domain = "R-explor")),
                                        HTML("<p>", gettext("Enter a term :</p>", domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(6, textInput("termsim", gettext("Term", domain = "R-explor"))),
                                          column(6,  selectInput("simmethod", gettext("Similarity", domain = "R-explor"),
                                                                 choices = c("correlation", "cosine", "jaccard"),
                                                                 selected = "correlation"))),
                                        uiOutput("termsimAlert"),
                                        h3(gettext("Associated terms", domain = "R-explor")),
                                        DT::dataTableOutput("simtermtable")
                               )
                               
                             )
                           )
                         )
                      )),
      

               server = function(input, output, session) {

                 
 
                 
                 ## Filtered DTM
                 dtm <- reactive({
                   dtm <- dfm_trim(dfm, min_count = input$term_min_occurrences)
                   dtm
                 })  
                 
                 ## Terms input
                 terms <- reactive({
                   tmp <- unlist(stri_extract_all_words(input$terms))
                   if (length(tmp) == 1 && is.na(tmp)) return(NULL)
                   tmp <- tolower(tmp[tmp != ""])
                 })
                 
                 
                 ## Run the query on the document term matrix as environment,
                 ## and returns the result
                 terms_query <- reactive({
                   error <- NULL
                   if (length(terms()) == 0) return(list(res = NULL, error = NULL))
                   dtm_terms <- dtm() %>% 
                     dfm_select(features = terms(), selection = "keep") %>% 
                     as.data.frame()
                   ## Convert count to presence / absence
                   if (ncol(dtm_terms) > 0) {
                     dtm_terms[dtm_terms > 0] <- 1
                   }
                   res <- try(
                     eval(parse(text = input$terms), envir = dtm_terms) %>% 
                       data.frame()
                     , silent = TRUE)
                   if (inherits(res, "try-error")) {
                     res <- NULL
                     error <- geterrmessage()
                   }
                   list(res = res, error = error)
                 })
                 
                 ## Invalid terms in terms input
                 invalid_terms <- reactive({
                   tmp_terms <- terms()
                   tmp_terms[!(tmp_terms %in% colnames(dtm()))]    
                 })
                 ## Alert if term is missing from corpus
                 output$termsAlert <- renderUI({
                   if (length(invalid_terms() > 0) && invalid_terms() != "") {
                     tmp_terms <- paste(invalid_terms(), collapse = ", ")
                     div(class = "alert alert-warning",
                         HTML(paste(gettext("<strong>Warning :</strong> the following terms are missing from the corpus : <i>", domain = "R-explor"), tmp_terms, "</i>")))
                   }
                 })
                 
                 ## Alert if error in search expression
                 output$evalAlert <- renderUI({
                   input$launch_search
                   e <- terms_query()$error
                   if (!is.null(e)) {
                     div(class = "alert alert-danger",
                         HTML(paste(gettext("<strong>Warning :</strong> Query error : <i>", domain = "R-explor"), e, "</i>")))
                   }
                 })
                 
                 ## Number of documents
                 output$nbdocs <- renderText({
                   ndoc(dtm())
                 })
                 
                 ## Global table options
                 tableOptions <- list(lengthMenu =  c(10,20,50,100), 
                                      pageLength = 20, orderClasses = TRUE, 
                                      autoWidth = TRUE, searching = TRUE)
                 ## Generate correct datatable order option from a column name
                 order_option <- function(table, name, order="desc") {
                   index <- which(names(table) == name) - 1
                   list(order = list(list(index, order)))
                 }
                 
                 ## Most frequent terms table
                 output$freqtable <- DT::renderDataTable({
                   if (is.null(dtm())) return(NULL)
                   frq <- data.frame(topfeatures(dtm(), n = 10000))
                   names(frq) <- "nb_terms"
                   frq$term <- rownames(frq)
                   docf <- data.frame(docfreq(dtm(), scheme = "count"))
                   names(docf) <- "nb_docs"
                   docf$term <- rownames(docf)
                   docf$prop_docs <- (round(docf$nb_docs / ndoc(dtm()) * 100, 2))
                   tab <- frq %>% left_join(docf, by = "term") %>% select(term, nb_terms, nb_docs, prop_docs)
                   names(tab) <- c(gettext("Term", domain = "R-explor"),
                                   gettext("Term frequency", domain = "R-explor"),
                                   gettext("Number of documents", domain = "R-explor"),
                                   gettext("Percentage of documents", domain = "R-explor"))
                   DT::datatable(tab, 
                                 options = c(tableOptions, order_option(tab, gettext("Term frequency", domain = "R-explor"))), rownames = FALSE)
                 })
                 
                 ## Search term frequency table
                 tab_term <- reactive({
                   tmp_dtm <- terms_query()$res
                   if (is.null(tmp_dtm)) return(NULL)
                   updateNumericInput(session, "start_documents", value = 1)
                   tmp_dtm <- docvars(dtm()) %>% select_(input$term_group) %>% bind_cols(tmp_dtm)
                   names(tmp_dtm) <- c("group", "n")
                   res <- tmp_dtm %>% group_by(group) %>% summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1))
                   res
                 })
                 
                 
                 ## Search term total frequency
                 tab_term_tot <- reactive({
                   tmp_dtm <- terms_query()$res
                   if (is.null(tmp_dtm)) return(NULL)
                   names(tmp_dtm) <- "n"
                   res <- tmp_dtm %>% 
                     summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1)) %>%
                     mutate(nom = gettext("Total", domain = "R-explor")) %>% select(nom, nb_docs, prop_docs)
                   res
                 })
                 
                 nb_docs_term <- reactive({
                   if (is.null(tab_term())) return(0)
                   as.numeric(tab_term() %>% summarise(n = sum(nb_docs)))
                 })
                 
                 ## Searched terms query text
                 output$freqterm_query <- renderText({
                   ## Dependency on search button
                   input$launch_search
                   isolate({
                     if (input$terms == "") {
                       return("")
                     }
                     res <- paste0(gettext("<p><strong>Query :</strong> <code>", domain = "R-explor"), input$terms, "</code>.</p>")
                     return(HTML(res))
                   })
                 })
                 
                 ## Total searched terms frequency text
                 output$freqterm_total <- renderText({
                   ## Dependency on search button
                   input$launch_search
                   isolate({
                     if (is.null(tab_term_tot())) {
                       return("")
                     }
                     tab <- tab_term_tot()
                     res <- paste0(gettext("<p><strong>Frequency in corpus :</strong> ", domain = "R-explor"), tab$nb_docs, gettext(" documents (", domain = "R-explor"), tab$prop_docs, "%).</p>")
                     return(HTML(res))
                   })
                 })
                 
                 ## Searched terms frequency table
                 output$freqtermtable <- DT::renderDataTable({
                   ## Dependency on search button
                   input$launch_search
                   isolate({
                     if (is.null(tab_term())) {
                       return(DT::datatable(data.frame(table = character())))
                     }
                     tab <- tab_term()
                     names(tab) <- c(gettext("Group", domain = "R-explor"),
                                     gettext("Number of documents", domain = "R-explor"),
                                     gettext("Percentage of documents", domain = "R-explor"))
                     tab <- DT::datatable(tab, 
                                   options = c(tableOptions, order_option(tab, gettext("Percentage of documents", domain = "R-explor"))), rownames = FALSE)
                   })
                   tab
                 })

                output$freqtermplot <- renderPlot({
                  input$launch_search
                  isolate({
                    if (is.null(tab_term())) {
                      return()
                    }
                    tab <- tab_term()
                    group <- quo(input$term_group)
                    var <- docvars(dtm()) %>% pull(!!group)
                    g <- NULL
                    if (is.character(var)) {
                      tab <- tab %>% 
                        filter(prop_docs > 0) %>%
                        mutate(group = stats::reorder(group, prop_docs))
                      g <- ggplot(tab) + 
                        geom_bar(aes(x = stats::reorder(group, prop_docs), y = prop_docs), stat = "identity") +
                        xlab(input$term_group) +
                        ylab(gettext("Percentage of documents", domain = "R-explor")) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    } 
                    if (is.numeric(var) || inherits(var, "Date")) {
                      g <- ggplot(tab, aes(x = group, y = prop_docs)) + 
                        geom_line() +
                        geom_smooth() +
                        xlab(input$term_group) +
                        ylab(gettext("Percentage of documents", domain = "R-explor"))
                    }
                    
                  })
                  g
                })
                 

                 ## Similarities
                 invalid_sim_term <- reactive({
                   !(input$termsim %in% colnames(dtm()))
                 })
                 sim_term <- reactive({
                   if (is.null(input$termsim) || input$termsim == "" || invalid_sim_term()) return(NULL)
                   sim <- as.matrix(textstat_simil(dtm(), selection = input$termsim, margin = "features", method = input$simmethod))
                   sim_nb <- as.matrix(textstat_simil(dtm(), selection = input$termsim, margin = "features", method = "simple matching"))
                   res <- data.frame(term = rownames(sim), similarity = round(as.vector(sim),4), nb_docs_commun = as.vector(sim_nb))
                   tmp <- dtm()
                   tmp[tmp > 0] <- 1
                   res$nb_docs_communs <- as.vector(t(tmp) %*% tmp[, input$termsim])
                   res
                 })
                 
                 ## Alert if term in similarity term is missing from corpus
                 output$termsimAlert <- renderUI({
                   if (input$termsim != "" && invalid_sim_term()) {
                     div(class = "alert alert-warning",
                         HTML(paste(gettext("<strong>Warning :</strong> term not found in the corpus : <i>", domain = "R-explor"), input$termsim, "</i>")))
                   }
                 })
                 
                 ## Similarities table
                 output$simtermtable <- DT::renderDataTable({
                   if (is.null(sim_term())) {
                     return(DT::datatable(data.frame(table = character())))
                   }
                   DT::datatable(sim_term(), 
                                 options = c(tableOptions, order_option(sim_term(), "similarity")), rownames = FALSE)
                 })
                 
               })
}