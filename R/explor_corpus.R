if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term"))

##' @rdname explor
##' @param obj_brut optional raw documents corpus
##' @param stopwords stopwords character vector
##' @param thesaurus quanteda thesaurus list
##' @aliases explor.Corpus
##' @export

explor.Corpus <- function(obj, obj_brut = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "Corpus")) stop(gettext("obj must be of class Corpus", domain = "R-explor"))
    
    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(obj_brut)) {
      if (!inherits(obj_brut, "Corpus")) stop(gettext("obj_brut must be of class Corpus", domain = "R-explor"))
      obj_brut <- prepare_results(obj_brut)
    }
    
    ## Settings
    settings <- list(qco_brut = obj_brut,
                     stopwords = stopwords,
                     thesaurus = thesaurus,
                     corpus_name = deparse(substitute(obj)),
                     corpus_brut_name = deparse(substitute(obj_brut)),
                     stopwords_name = deparse(substitute(stopwords)),
                     thesaurus_name = deparse(substitute(thesaurus)))

    ## Launch interface
    explor_corpus(corpus, settings)
    
}

##' @rdname explor
##' @aliases explor.corpus
##' @export

explor.corpus <- function(obj, obj_brut = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "corpus")) stop(gettext("obj must be of class corpus", domain = "R-explor"))

    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(obj_brut)) {
      if (!inherits(obj_brut, "corpus")) stop(gettext("obj_brut must be of class corpus", domain = "R-explor"))
      obj_brut <- prepare_results(obj_brut)
    }
    
    ## Settings
    settings <- list(qco_brut = obj_brut,
                     stopwords = stopwords,
                     thesaurus = thesaurus,
                     corpus_name = deparse(substitute(obj)),
                     corpus_brut_name = deparse(substitute(obj_brut)),
                     stopwords_name = deparse(substitute(stopwords)),
                     thesaurus_name = deparse(substitute(thesaurus)))

    ## Launch interface
    explor_corpus(corpus, settings)
    
}



## Custom CSS
explor_corpus_css <- function() {
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

              #sidebar p {
                  margin-top: 10px;
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
              
              .document-content {
                  font-size: 12px !important;
                  background-color: #EEE;
                  border-radius: 8px;
                  padding: 10px;
                  margin: 20px 5px;
              }
              .highlight {background-color: yellow;}
              
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
explor_corpus_js <- function() {
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


##' @import stringi
explor_corpus_highlight <- function(x, str) {
  stringi::stri_replace_all_fixed(x, pattern = str,  replacement = paste0("<span class='highlight'>",str,"</span>"),
                         vectorize_all = FALSE,
                         opts_fixed = stri_opts_fixed(case_insensitive = TRUE))
}


##' @import shiny
##' @import quanteda
##' @importFrom highr hi_html

explor_corpus <- function(qco, settings) { 
    

    ## Document level variables
    vars <- lapply(docvars(qco), unique)
    nvalues <- lapply(vars, length)
    vars <- vars[nvalues > 1 & nvalues < 100]

    ## Document level variables filters
    filter_inputs <- lapply(names(vars), function(name) {
      v <- vars[[name]]
      input_name <- paste0("meta_", name)
      if (class(v) == "numeric") {
        return(sliderInput(input_name, name, min(v), max(v), value = c(min(v), max(v))))
      }
      if (class(v) == "factor") {
        v <- as.character(v)
      }
      if (class(v) == "character") {
        levels <- unique(v)
        if (length(levels) == 1) return(NULL)
        levels[is.na(levels)] <- "NA"
        return(checkboxGroupInput(input_name, name, choices = levels))
      }
      if (class(v) == "Date") {
        return(dateRangeInput(input_name, name, start = min(v), end = max(v))) 
      }
    })
    
    ## n-grams
    m_ngrams <- 1:5
    names(m_ngrams) <- paste0(1:5, "-gram")
    
    shiny::shinyApp(
      ui = navbarPage(gettext("Corpus", domain = "R-explor"),
                      header = tags$head(
                        tags$style(explor_corpus_css())),
                      tabPanel(gettext("Terms", domain = "R-explor"),
                         sidebarLayout(
                           sidebarPanel(id = "sidebar",
                                        h4(gettext("Corpus treatment", domain = "R-explor")),
                                        checkboxInput("treat_tolower", gettext("Convert to lowercase", domain = "R-explor"), value = TRUE),
                                        checkboxInput("treat_removepunct", gettext("Remove punctuation", domain = "R-explor"), value = TRUE),
                                        checkboxInput("treat_rmnum", gettext("Remove numbers", domain = "R-explor"), value = TRUE),
                                        if (!is.null(settings$stopwords)) 
                                          checkboxInput("treat_stopwords", gettext("Remove stopwords", domain = "R-explor"), value = TRUE),
                                        if (!is.null(settings$thesaurus)) 
                                          checkboxInput("treat_thesaurus", gettext("Apply thesaurus", domain = "R-explor"), value = TRUE),
                                        checkboxInput("treat_stem", gettext("Stem words", domain = "R-explor"), value = FALSE),                                        
                                        h4(gettext("Corpus filtering", domain = "R-explor")),
                                        p(gettext("If nothing is selected, no filter is applied.", domain = "R-explor")),
                                        uiOutput("filters"),
                                        numericInput("term_min_occurrences", gettext("Filter terms on minimal frequency", domain = "R-explor"), 0, 0, 1000, 1),
                                        tags$p(actionButton("get_r_code",
                                                            icon = icon("code"),
                                                            label = gettext("Get R code", domain = "R-explor"))),
                                        tags$script(explor_corpus_js())
                           ),
                           mainPanel(
                             tabsetPanel(
                               
                               ## "Frequent terms" tab
                               tabPanel(gettext("Frequent terms", domain = "R-explor"),
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        checkboxGroupInput("ngrams",
                                                           gettext("Ngrams", domain = "R-explor"),
                                                           m_ngrams,
                                                           selected = 1),
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
                                                    ),
                                                    tabPanel(gettext("Documents", domain = "R-explor"),
                                                             div(style = "display: none;",
                                                                 numericInput("start_documents", gettext("From", domain = "R-explor"), value = 1)),
                                                             div(class = "inline-small form-inline",
                                                                 actionButton("prev_documents", gettext("Previous", domain = "R-explor"), icon("arrow-left")),
                                                                 textOutput("documents_pagination"),
                                                                 #numericInput("nb_documents", "Nombre", 10, min = 1, width = "4em"),
                                                                 actionButton("next_documents", gettext("Next", domain = "R-explor"), icon("arrow-right")),
                                                                 numericInput("nb_documents_display", gettext(" Number : ", domain = "R-explor"), 
                                                                              value = 10, min = 1, max = 100, step = 1, width = "auto")),
                                                             htmlOutput("documenttable")
                                                    )
                                        )
                               ),
                               

                               ## "Help" tab
                               tabPanel(gettext("Help", domain = "R-explor"),
                                        h2(gettext("Help", domain = "R-explor")),
                                        
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        p(HTML(gettext("How to read the table :", domain = "R-explor"))),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>Term frequency</code> : number of times this term is found in the selected corpus", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Number of documents</code> : number of documents in the selected corpus in which this term appears at least once", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Percentage of documents</code> : percentage of documents in the selected corpus in which this term appears at least once", domain = "R-explor")))
                                        ),
                                        
                                        h3(gettext("Tearms search", domain = "R-explor")),
                                        p(HTML(gettext("Allows to search for terms or terms combinations in the selected corpus, and to display both frequencies and the corresponding documents. Note that the search is made on the cleaned corpus (after filtering, stemming, removing of stopwords, etc.). Also note that highlighting is not perfect : for example, if searching for <code>I</code>, every \"i\" in the documents will be highlighted, but the search has been made only on the word \"I\".", domain = "R-explor"))),
                                        p("Query examples :"),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>I</code> : search for documents with the term \"I\" (or \"i\")", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>!i</code> : search for documents without the term \"I\"", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>i | me | we</code> : search for documents with \"i\", \"me\" or \"we\" (or any combination)", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>i & we</code> : search for documents with both \"i\" and \"we\"", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>sky & (sea | ocean)</code> : search for documents with \"sky\" and the terms \"sea\" or \"ocean\" (or both)", domain = "R-explor")))
                                        )
                                        
                               )
                             )
                           )
                         )
                      )),
      

               server = function(input, output, session) {

                 
                 ## Variable inputs
                 output$filters <- renderUI({
                   filter_inputs  
                 })
                 
                 
                 ## Corpus filtering R Code
                 filtering_code <- reactive({
                   code <- ""
                   metas <- grep("^meta_", names(input), value = TRUE)
                   for (meta in metas) {
                     if (is.null(input[[meta]])) next()
                     varname <- gsub("^meta_", "", meta)
                     var <- docvars(qco)[[varname]]
                     selected_values <- input[[meta]]
                     test <- ""
                     ## Date variables (date range)
                     if (inherits(var, "Date")) {
                       if (selected_values[1] != min(var) || selected_values[2] != max(var)) {
                         test <- sprintf("%s >= %s & %s <= %s", varname, selected_values[1], varname, selected_values[2])
                       }
                     }
                     ## Character variables (checkboxes)
                     if (is.character(selected_values)) {
                       selected_values[selected_values == "NA"] <- NA
                       selected_values <- utils::capture.output(dput(selected_values))
                       test <- paste0(varname, " %in% ", selected_values)
                     }
                     ## Numeric variables (range)
                     if (is.numeric(selected_values)) {
                       if (selected_values[1] != min(var) || selected_values[2] != max(var)) {
                         test <- sprintf("%s >= %s & %s <= %s", varname, selected_values[1], varname, selected_values[2])
                       }
                     } 
                     if (test != "") {
                       code <- paste0(code, sprintf("tmp_corpus <- corpus_subset(tmp_corpus, %s)\n", test))
                     }
                   }
                   code
                 })
                 corpus_filtering_code <- function(corpus_name) {
                   code <- filtering_code()
                   if (code != "") {
                     code <- paste0("tmp_corpus <- ", corpus_name, "\n", code)
                   }
                   return(code)
                 }   
                                  
                 ## Create logical vector from selected filter elements
                 selected_elements <- function(corpus, meta) {
                   varname <- gsub("^meta_", "", meta)
                   var <- docvars(corpus)[[varname]]
                   selected_values <- input[[meta]]
                   ## Date variables (date range)
                   if (inherits(var, "Date")) {
                     return(var >= selected_values[1] &
                              var <= selected_values[2])
                   }
                   ## Character variables (checkboxes)
                   if (is.character(selected_values)) {
                     selected_values[selected_values == "NA"] <- NA
                     return(var %in% selected_values)
                   }
                   ## Numeric variables (range)
                   if (is.numeric(selected_values)) {
                     return(var >= selected_values[1] &
                              var <= selected_values[2])
                   }
                 }
                 
                 ## Clean corpus filtered
                 co <- reactive({
                   code <- corpus_filtering_code(settings$corpus_name)
                   if (code != "") {
                     eval(parse(text = code))
                     return(tmp_corpus)
                   } else {
                     return(qco)
                   }
                 })
                 
                 ## Brut corpus filtered
                 co_brut <- reactive({
                   code <- corpus_filtering_code(settings$corpus_brut_name)
                   if (code != "") {
                     eval(parse(text = code))
                     return(tmp_corpus)
                   } else {
                     return(qco_brut)
                   }
                 })
                 
                 ## DTM computation code
                 dtm_code <- reactive({
                   if (!is.null(input$treat_stopwords) && input$treat_stopwords) {
                     remove <- settings$stopwords_name
                   } else {
                     remove <- "NULL"
                   }
                   if (!is.null(input$treat_thesaurus) && input$treat_thesaurus) {
                     thesaurus <- settings$thesaurus_name
                   } else {
                     thesaurus <- "NULL"
                   }
                   code <- paste0("dtm <- dfm(%s, what = 'fastestword',\n", 
                                  "           tolower = ", input$treat_tolower, ", remove_punct = ", input$treat_removepunct, ",\n",
                                  "           remove_numbers = ", input$treat_rmnum, ", stem = ", input$treat_stem, ",\n",
                                  "           remove = ", remove, ",\n",
                                  "           thesaurus = ", thesaurus, ",\n",
                                  "           ngrams = ", input$ngrams, ")\n")
                   if (input$term_min_occurrences > 0) {
                     code <- paste0(code, "dtm <- dfm_trim(dtm, min_count = ", input$term_min_occurrences, ")")
                   }
                   code
                 })
                 corpus_dtm_code <- function(corpus_name) {
                   sprintf(dtm_code(), corpus_name)
                 }
                 
                 ## Clean corpus DTM filtered by n-grams
                 dtm <- reactive({
                   if (length(input$ngrams) == 0 || is.null(co()) || ndoc(co()) == 0) { 
                     return(NULL)
                   }
                   code <- corpus_dtm_code("co()")
                   eval(parse(text = code))
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
                   ndoc(co())
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
                   # names(docf) <- c(gettext("Term frequency", domain = "R-explor"),
                   #                  gettext("Documents frequency", domain = "R-explor"),
                   #                  gettext("Documents proportion", domain = "R-explor"))
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
                   tmp_dtm <- docvars(co()) %>% select_(input$term_group) %>% bind_cols(tmp_dtm)
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
                    var <- docvars(co()) %>% pull(!!group)
                    g <- NULL
                    if (is.character(var)) {
                      tab <- tab %>% 
                        filter(prop_docs > 0) %>%
                        mutate(group = reorder(group, prop_docs))
                      g <- ggplot(tab) + 
                        geom_bar(aes(x = reorder(group, prop_docs), y = prop_docs), stat = "identity") +
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
                 
                 
                 ## Observers on pagination events
                 observeEvent(input$prev_documents, {
                   updateNumericInput(session, "start_documents", value = max(1, isolate(input$start_documents) - isolate(input$nb_documents_display)), min = 1)
                 })
                 observeEvent(input$next_documents, {
                   val <- isolate(input$start_documents) + isolate(input$nb_documents_display)
                   if (val <= nb_docs_term()) {
                     print("obs")
                     updateNumericInput(session, "start_documents", value = val)
                   }
                 })
                 
                 ## Documents list
                 output$documenttable <- renderText({
                   ## Acquire dependencies
                   input$launch_search
                   start <- input$start_documents
                   nb_documents <- input$nb_documents_display
                   isolate({
                     if (is.null(terms_query()$res)) return(gettext("<div class='document-content'>No document found.</p>", domain = "R-explor"))
                     indexes <- which(as.vector(terms_query()$res) > 0)
                     end <- min(start + nb_documents - 1, nb_docs_term())
                     indexes <- indexes[start:end]
                     out <- ""
                     for (i in indexes) {
                       out <- paste(out, "<div class='document-content'>")
                       out <- paste(out, "<p><strong>", docvars(co())$orientation[i] ,"</strong><br />")
                       out <- paste(out, explor_corpus_highlight(co_brut()[i], terms()))
                       out <- paste(out, "</p></div>")
                     }
                     HTML(out)
                   })
                 })
                 
                 ## documents list pagination text
                 output$documents_pagination <- renderText({
                   input$launch_search
                   start <- input$start_documents
                   nb_documents <- input$nb_documents_display
                   isolate({
                     end <- min(start + nb_documents - 1, nb_docs_term())
                     if (end == 0) start <- 0
                     sprintf(gettext("%s to %s of %s", domain = "R-explor"), start, end, nb_docs_term())
                   })
                 })

                 
                 ## Code export modal dialog
                 observeEvent(input$get_r_code, {
                   code <- ""
                   corpus_name <- settings$corpus_name
                   filter_code <- corpus_filtering_code(corpus_name)
                   if (filter_code != "") {
                     code <- paste0("## ", gettext("Corpus filtering", domain = "R-explor"), "\n")
                     code <- paste0(code, filter_code, "\n")
                     corpus_name <- "tmp_corpus"
                   }
                   code <- paste0(code, "## ", gettext("Document term matrix computation", domain = "R-explor"), "\n")
                   code <- paste0(code, corpus_dtm_code(corpus_name))
                   showModal(modalDialog(
                     title = gettext("Export R code", domain = "R-explor"),
                     HTML(paste0(gettext("Copy, paste and run the following code in your script to compute the corresponding document-term matrix (DTM) :", domain = "R-explor"),
                                 "<pre><code>",
                                 paste(highr::hi_html(code), collapse = "\n"),
                                 "</code></pre>")),
                     easyClose = TRUE))
                 })
                 
               })
}