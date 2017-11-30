

##' @rdname explor
##' @aliases explor.dfm
##' @export

explor.dfm <- function(obj, ...) {
    
    if (!inherits(obj, "dfm")) stop(gettext("obj must be of class dfm", domain = "R-explor"))
    
    ## Settings
    settings <- list(dfm_name = deparse(substitute(obj)))

    ## Launch interface
    explor_dfm(obj, settings)
    
}


##' @import shiny
##' @import quanteda
##' @importFrom highr hi_html

explor_dfm <- function(dfm, settings) { 
    

    ## Document level variables
    vars <- lapply(docvars(dfm), unique)
    nvalues <- lapply(vars, length)
    vars <- vars[nvalues > 1 & nvalues < 100]
    vars <- names(vars)

    freq_choices <- c("tf", "idf", "tf_idf")
    
    shiny::shinyApp(
      ui = navbarPage(gettext("Dfm", domain = "R-explor"),
                      header = tags$head(
                        tags$style(explor_text_css())),
                      tabPanel(gettext("Document-feature matrix", domain = "R-explor"),
                         sidebarLayout(
                           
                           ## Sidebar -------------------------------------------------
                           
                           sidebarPanel(id = "sidebar",
                                        selectInput("doc_group", gettext("Group documents by", domain = "R-explor"), choices = c("none", vars)),
                                        selectInput("dfm_weight", gettext("Weight dfm by", domain = "R-explor"), choices = c(
                                          "Count" = "frequency", 
                                          "tf-idf" = "tfidf",
                                          "Relative frequency" = "relfreq",
                                          "Maximum relative frequency" = "relmaxfreq",
                                          "Frequency logarithm" = "logfreq")),
                                        numericInput("term_min_occurrences", gettext("Filter terms on minimal frequency", domain = "R-explor"), value = 0, min = 0)
                           ),
                           mainPanel(
                             tabsetPanel(
                               
                               ## Wordcloud tab ----------------------------------------
                               
                               tabPanel(gettext("Wordcloud", domain = "R-explor"),
                                        h3(gettext("Wordcloud", domain = "R-explor")),
                                        numericInput("wordcloud_maxwords", gettext("Maximum number of words per plot", domain = "R-explor"), min = 5, max = 1000, value = 30),
                                        checkboxInput("wordcloud_compare", gettext("Compare by documents", domain = "R-explor"), value = FALSE),
                                        plotOutput("plot_wordcloud")
                               ),
                               

                               
                               ## "Top features" tab --------------------------------
                               
                               tabPanel(gettext("Top features", domain = "R-explor"),
                                        h3(gettext("Top features", domain = "R-explor")),
                                        checkboxInput("topfeat_scheme", gettext("Compare by documents", domain = "R-explor"), value = FALSE),
                                        selectInput("topfeat_group", gettext("Group by", domain = "R-explor"), choices = c("none", vars)),
                                        numericInput("topfeat_n", gettext("Number of features to keep", domain = "R-explor"), value = 20, min = 10, max = 1000),
                                        p(HTML("<strong>", gettext("Number of documents", domain = "R-explor"), "&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
                                        tabsetPanel(type = "pills",
                                                    tabPanel(gettext("Table", domain = "R-explor"),
                                                             DT::dataTableOutput("topfeat_table"),
                                                             tags$p(actionButton("code_topfeat_table",
                                                                                 class = "btn-success",
                                                                                 icon = icon("code"),
                                                                                 label = gettext("Get R code", domain = "R-explor")))
                                                    ),
                                                    tabPanel(gettext("Plot", domain = "R-explor"),
                                                             tags$p(htmlOutput("topfeat_plot_text")),
                                                             plotOutput("topfeat_plot"),
                                                             tags$p(actionButton("code_topfeat_plot",
                                                                                 class = "btn-success",
                                                                                 icon = icon("code"),
                                                                                 label = gettext("Get R code", domain = "R-explor")))
                                                             
                                                    )
                                        )
                               ),
                                        
                                        
                               
                               ## "Terms search" tab ----------------------------------
                               
                               tabPanel(gettext("Terms search", domain = "R-explor"),
                                        h3(gettext("Terms search", domain = "R-explor")),
                                        HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :', domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(8, textInput("terms", gettext("Terms", domain = "R-explor"), width = "100%")),
                                          column(4, selectInput("term_group",
                                                                gettext("Group by", domain = "R-explor"),
                                                                choices = names(vars)))),
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
                                                             tags$p(htmlOutput("freqtermplottext")),
                                                             plotOutput("freqtermplot")
                                                    )
                                        )
                               ),
                               
                               
                               ## "Similarities" tab ----------------------------------------
                               
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

                 
                 ## dfm computation code
                 dfm_code <- reactive({
                   code <- ""
                   if (!is.na(input$term_min_occurrences) && input$term_min_occurrences > 0) {
                     code <- paste0("tmp_dfm <- dfm_trim(tmp_dfm, min_count = ", input$term_min_occurrences, ")\n")
                   }
                   if (input$doc_group != "none") {
                     code <- paste0(code, "tmp_dfm <- dfm_group(tmp_dfm, groups = '", input$doc_group, "')\n")
                   }
                   if (input$dfm_weight != "frequency") {
                     code <- paste0(code, "tmp_dfm <- dfm_weight(tmp_dfm, type = '", input$dfm_weight, "')\n")
                   }
                   if (code != "") {
                      code <- paste("tmp_dfm <- %s", code, sep = "\n")
                   }
                   code
                 })
                 get_dfm_code <- function(dfm_name) {
                   code <- dfm_code()
                   if (code != "") code <- sprintf(code, dfm_name)
                   code
                 }
                 
                 ## Return filtered dfm
                 dtm <- reactive({
                   code <- get_dfm_code("dfm")
                   if (code != "") {
                     withProgress(message = gettext("Recomputing dfm", domain = "R-explor"), value = 0.3, {
                       eval(parse(text = code))
                       incProgress(0.7)
                       return(tmp_dfm)
                     })
                   } else {
                     return(dfm)
                   }
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
                 
                 
                 ## WORDCLOUD ----------------------------------------------------------
                 
                 output$plot_wordcloud <- renderPlot({
                   textplot_wordcloud(dtm(), comparison = input$wordcloud_compare, max.words = input$wordcloud_maxwords)
                 })
                 
                 
                 ## TOP FEATURES -----------------------------------------------------
                 
                 ## Number of documents
                 output$nbdocs <- renderText({
                   ndoc(dtm())
                 })
                 
                 
                 topfeat_tab_code <- reactive({
                   scheme <- ifelse(input$topfeat_scheme, "docfreq", "count")
                   group <- input$topfeat_group
                   if (!is.na(input$topfeat_n) && input$topfeat_n > 0) {
                     n <- input$topfeat_n
                   } else {
                     n <- 10
                   }
                   if (group == "none") group <- NULL
                   code <- paste0("topf_tab <- topfeatures(%s, scheme = '", scheme, "', n = ", n)
                   if (!is.null(group)) {
                     code <- paste0(code, ", groups = '", group, "'")
                   }
                   code <- paste0(code, ")")
                   code <- paste(code,
                                 "topf_tab <- data.frame(topf_tab)", sep="\n")
                   code
                 })
                 get_topfeat_tab_code <- function(dtm_name) {
                   code <- sprintf(topfeat_tab_code(), dtm_name)
                   code
                 }
                 
                 output$topfeat_table <- DT::renderDataTable(({
                   if (is.null(dtm())) return(NULL)
                   code <- get_topfeat_tab_code("dtm()")
                   code <- paste(
                     "tableOptions <- list(lengthMenu =  c(10,20,50,100), pageLength = 20, orderClasses = TRUE, autoWidth = TRUE, searching = TRUE)",
                     code,
                     "DT::datatable(topf_tab, options = c(tableOptions), rownames = TRUE)",
                     sep = "\n")
                   eval(parse(text = code))
                 }))
                 
                 output$topfeat_plot_text <- renderText({
                   if (is.null(dtm())) return("No data")
                   return("")
                 })
                 
                 output$topfeat_plot <- renderPlot({
                   if (is.null(dtm())) return(NULL)
                   scheme <- ifelse(input$topfeat_scheme, "docfreq", "count")
                   group <- input$topfeat_group
                   if (group == "none") group <- NULL
                   tab <- data.frame(topfeatures(dtm(), scheme = scheme, groups = group, n = input$topfeat_n))
                   
                 })
                 
                 
                 ## SEARCH TERMS --------------------------------------------
                 
                 ## Terms input
                 terms <- reactive({
                   tmp <- unlist(stri_extract_all_words(input$terms))
                   if (length(tmp) == 1 && is.na(tmp)) return(NULL)
                   tmp <- tolower(tmp[tmp != ""])
                 })
                 ## Invalid terms in terms input
                 invalid_terms <- reactive({
                   tmp_terms <- terms()
                   tmp_terms[!(tmp_terms %in% colnames(dtm()))]    
                 })
                 
                 ## Run the query on the document-feature matrix as environment,
                 ## and returns the result
                 terms_query <- reactive({
                   
                   ## Progress
                   query_progress <- shiny::Progress$new()
                   on.exit(query_progress$close())
                   query_progress$set(message = gettext("Running query", domain = "R-explor"), value = 0)
                   
                   error <- NULL
                   if (length(terms()) == 0) return(list(res = NULL, error = NULL))
                   dfm_terms <- dtm() %>% 
                     dfm_select(pattern = terms(), valuetype = "fixed", selection = "keep") %>% 
                     as.data.frame()
                   query_progress$inc(0.3)
                   ## Convert count to presence / absence
                   if (ncol(dfm_terms) > 0) {
                     dfm_terms[dfm_terms > 0] <- 1
                   }
                   query_progress$inc(0.1)
                   res <- try(
                     eval(parse(text = input$terms), envir = dfm_terms) %>% 
                       data.frame()
                     , silent = TRUE)
                   query_progress$inc(0.6)
                   if (inherits(res, "try-error")) {
                     res <- NULL
                     error <- geterrmessage()
                   }
                   list(res = res, error = error)
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
                   e <- terms_query()$error
                   if (!is.null(e)) {
                     div(class = "alert alert-danger",
                         HTML(paste(gettext("<strong>Warning :</strong> Query error : <i>", domain = "R-explor"), e, "</i>")))
                   }
                 })
                 
                 ## Search term frequency table
                 tab_term <- reactive({
                   tmp_dfm <- terms_query()$res
                   if (is.null(tmp_dfm)) return(NULL)
                   updateNumericInput(session, "start_documents", value = 1)
                   tmp_dfm <- docvars(dtm()) %>% select_(input$term_group) %>% bind_cols(tmp_dfm)
                   names(tmp_dfm) <- c("group", "n")
                   res <- tmp_dfm %>% group_by(group) %>% summarise(nb_docs = sum(n), prop_docs = round(nb_docs / n() * 100, 1))
                   res
                 })
                 
                 
                 ## Search term total frequency
                 tab_term_tot <- reactive({
                   tmp_dfm <- terms_query()$res
                   if (is.null(tmp_dfm)) return(NULL)
                   names(tmp_dfm) <- "n"
                   res <- tmp_dfm %>% 
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
                   if (input$terms == "") {
                     return("")
                   }
                   res <- paste0(gettext("<p><strong>Query :</strong> <code>", domain = "R-explor"), input$terms, "</code>.</p>")
                   return(HTML(res))
                 })
                 
                 ## Total searched terms frequency text
                 output$freqterm_total <- renderText({
                   if (is.null(tab_term_tot())) {
                     return("")
                   }
                   tab <- tab_term_tot()
                   res <- paste0(gettext("<p><strong>Frequency in corpus :</strong> ", domain = "R-explor"), tab$nb_docs, gettext(" documents (", domain = "R-explor"), tab$prop_docs, "%).</p>")
                   return(HTML(res))
                 })
                 
                 ## Searched terms frequency table
                 output$freqtermtable <- DT::renderDataTable({
                   if (is.null(tab_term()) || nb_docs_term() == 0) {
                     return(DT::datatable(data.frame(table = character())))
                   }
                   tab <- tab_term()
                   names(tab) <- c(gettext("Group", domain = "R-explor"),
                                   gettext("Number of documents", domain = "R-explor"),
                                   gettext("Percentage of documents", domain = "R-explor"))
                   tab <- DT::datatable(tab, 
                                        options = c(tableOptions, order_option(tab, gettext("Percentage of documents", domain = "R-explor"))), rownames = FALSE)
                   tab
                 })
                 
                 output$freqtermplottext <- renderText({
                   if (is.null(tab_term()) || nb_docs_term() == 0) {
                     return(gettext("No document found", domain = "R-explor"))
                   } else {
                     text <- paste0(nb_docs_term(), gettext(" documents found. ", domain = "R-explor"))
                   }
                   return(HTML(text))
                 })
                 
                 ## Searched terms frequency plot
                 output$freqtermplot <- renderPlot({
                   if (is.null(tab_term()) || nb_docs_term() == 0) {
                     return()
                   }
                   tab <- tab_term()
                   group <- quo(input$term_group)
                   var <- docvars(dtm()) %>% pull(!!group)
                   g <- NULL
                   if (is.character(var) || is.factor(var)) {
                     tab <- tab %>% 
                       filter(prop_docs > 0) %>%
                       mutate(group = stats::reorder(group, prop_docs))
                     g <- ggplot(tab) + 
                       geom_bar(aes(x = stats::reorder(group, prop_docs), y = prop_docs), stat = "identity") +
                       xlab(input$term_group) +
                       ylab(gettext("Percentage of documents", domain = "R-explor")) +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                       expand_limits(y = 0)
                   } 
                   if (is.numeric(var) || inherits(var, "Date")) {
                     g <- ggplot(tab, aes(x = group, y = prop_docs)) + 
                       geom_line() +
                       geom_smooth() +
                       xlab(input$term_group) +
                       ylab(gettext("Percentage of documents", domain = "R-explor")) +
                       expand_limits(y = 0)
                   }
                   g
                 })
                 
                 
 

                 ## SIMILARITIES -------------------------------------------
                 
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
                 
                 ### CODE EXPORT ---------------------------------------------------------------------
                 
                 ## Code export modal dialog
                 show_code <- function(code) {
                   code <- formatR::tidy_source(text = code, 
                                                width.cutoff = 75, 
                                                output = FALSE)$text.tidy
                   showModal(modalDialog(
                     title = gettext("Export R code", domain = "R-explor"), size = "l", 
                     HTML(paste0(gettext("Copy, paste and run the following code in your script to compute the displayed results :", domain = "R-explor"),
                                 "<pre><code>",
                                 paste(highr::hi_html(code), collapse = "\n"),
                                 "</code></pre>")),
                     easyClose = TRUE))
                 }
                 observeEvent(input$code_topfeat_table, {
                   code <- ""
                   dfm_name <- settings$dfm_name
                   dfm_code <- get_dfm_code(dfm_name)
                   if (dfm_code != "") {
                     code <- paste0("## ", gettext("Dfm treatment", domain = "R-explor"), "\n")
                     code <- paste0(code, dfm_code, "\n")
                     dfm_name <- "tmp_dfm"
                   }
                   code <- paste0(code, "## ", gettext("Top features", domain = "R-explor"), "\n")
                   code <- paste0(code, get_topfeat_tab_code(dfm_name))
                   code <- paste0(code, "## ", gettext("Optional DT::datable output", domain = "R-explor"), "\n")
                   show_code(code)
                 })
                 
               })
}