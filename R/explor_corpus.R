if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term", "tmp_corpus"))

##' @rdname explor
##' @param raw_corpus optional raw documents corpus
##' @param stopwords stopwords character vector
##' @param thesaurus quanteda thesaurus list
##' @aliases explor.Corpus
##' @export

explor.Corpus <- function(obj, raw_corpus = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "Corpus")) stop(gettext("obj must be of class Corpus", domain = "R-explor"))
    
    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(raw_corpus)) {
      if (!inherits(raw_corpus, "Corpus")) stop(gettext("raw_corpus must be of class Corpus", domain = "R-explor"))
      raw_corpus <- prepare_results(raw_corpus)
    }
    
    ## Settings
    settings <- list(raw_corpus = raw_corpus,
                     stopwords = stopwords,
                     thesaurus = thesaurus,
                     corpus_name = deparse(substitute(obj)),
                     raw_courpus_name = deparse(substitute(raw_corpus)),
                     stopwords_name = deparse(substitute(stopwords)),
                     thesaurus_name = deparse(substitute(thesaurus)))

    ## Launch interface
    explor_corpus(corpus, settings)
    
}

##' @rdname explor
##' @aliases explor.corpus
##' @export

explor.corpus <- function(obj, raw_corpus = NULL, stopwords = NULL, thesaurus = NULL, ...) {
    
    if (!inherits(obj, "corpus")) stop(gettext("obj must be of class corpus", domain = "R-explor"))

    ## corpus preparation
    corpus <- prepare_results(obj)
    if (!is.null(raw_corpus)) {
      if (!inherits(raw_corpus, "corpus")) stop(gettext("raw_corpus must be of class corpus", domain = "R-explor"))
      raw_corpus <- prepare_results(raw_corpus)
    }
    
    ## Settings
    settings <- list(raw_corpus = raw_corpus,
                     stopwords = stopwords,
                     thesaurus = thesaurus,
                     corpus_name = deparse(substitute(obj)),
                     raw_corpus_name = deparse(substitute(raw_corpus)),
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

              #treatment .checkbox { margin: 0; }

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
              input[type=checkbox] { margin: 0;}
              
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
              .document-content .metadata {
                  margin-top: 10px;
                  border-left: 2px solid #BBB;
                  padding-left: 5px;
                  font-size: 90%;
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
##' @importFrom SnowballC getStemLanguages
##' @importFrom formatR tidy_source

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
    
    ## Document corpus choices
    doc_corpus_choices <- "clean"
    tmp_names <- gettext("Clean corpus", domain = "R-explor")
    if (!is.null(settings$raw_corpus)) {
      doc_corpus_choices <- c(doc_corpus_choices, "raw")
      tmp_names <- c(tmp_names, gettext("Raw corpus", domain = "R-explor"))
      names(doc_corpus_choices) <- tmp_names
    }

    
    shiny::shinyApp(
      
      ui = navbarPage(gettext("Corpus", domain = "R-explor"),
                      header = tags$head(
                        tags$style(explor_corpus_css())),
                      tabPanel(gettext("Exploration", domain = "R-explor"),
                         sidebarLayout(
                           
                           ## Sidebar ---------------------------------------------------------------
                           
                           sidebarPanel(id = "sidebar",
                                        h4(gettext("Corpus treatment", domain = "R-explor")),
                                        div(id = "treatment",
                                            checkboxInput("treat_tolower", gettext("Convert to lowercase", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_numbers", gettext("Remove numbers", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_punct", gettext("Remove punctuation", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_symbols", gettext("Remove symbols", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_twitter", gettext("Remove Twitter", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_hyphens", gettext("Remove hyphens", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_remove_url", gettext("Remove URLs", domain = "R-explor"), value = TRUE),
                                            if (!is.null(settings$stopwords)) 
                                              checkboxInput("treat_stopwords", gettext("Remove stopwords", domain = "R-explor"), value = TRUE),
                                            if (!is.null(settings$thesaurus)) 
                                              checkboxInput("treat_thesaurus", gettext("Apply thesaurus", domain = "R-explor"), value = TRUE),
                                            checkboxInput("treat_stem", gettext("Stem words", domain = "R-explor"), value = FALSE),
                                            conditionalPanel("input.treat_stem",
                                                             selectInput("treat_stem_lang", gettext("Stemming language", domain = "R-explor"),
                                                                         choices = SnowballC::getStemLanguages(), selected = "english"), width = "50%")
                                        ),
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
                               
                               ## "Frequent terms" tab -----------------------------------------------
                               
                               tabPanel(gettext("Frequent terms", domain = "R-explor"),
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        checkboxGroupInput("ngrams",
                                                           gettext("Ngrams", domain = "R-explor"),
                                                           m_ngrams,
                                                           selected = 1),
                                        p(HTML("<strong>", gettext("Number of documents", domain = "R-explor"), "&nbsp;:</strong>"), textOutput("nbdocs", inline = TRUE)),
                                        DT::dataTableOutput("freqtable")),
                               
                               ## "Terms search" tab --------------------------------------------------   
                               
                               tabPanel(gettext("Terms search", domain = "R-explor"),
                                        h3(gettext("Terms search", domain = "R-explor")),
                                        HTML("<p>", gettext('Enter one or more terms. You can use logical operators like <code>&</code> ("and"), <code>|</code> ("or"), <code>!</code> ("not") and parentheses :', domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(5, textInput("terms", gettext("Terms", domain = "R-explor"), width = "100%")),
                                          column(3, selectInput("term_group",
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
                                                             fluidRow(
                                                               if (!is.null(settings$raw_corpus)) {
                                                                 column(4,
                                                                        selectInput("doc_corpus", 
                                                                            gettext("Display documents from", domain = "R-explor"), 
                                                                            choices = doc_corpus_choices))
                                                               },
                                                               column(4,
                                                                      selectInput("doc_display", 
                                                                                  gettext("Display", domain = "R-explor"),
                                                                                  choices = c("Documents", "Kwics")))
                                                             ),
                                                             fluidRow(
                                                               column(4,
                                                                      checkboxInput("doc_metadata", gettext("Display metadata", domain = "R-explor"), value = TRUE))
                                                             ),
                                                             div(class = "inline-small form-inline",
                                                                 actionButton("prev_documents", gettext("Previous", domain = "R-explor"), icon("arrow-left")),
                                                                 textOutput("documents_pagination"),
                                                                 actionButton("next_documents", gettext("Next", domain = "R-explor"), icon("arrow-right")),
                                                                 numericInput("nb_documents_display", gettext(" Number : ", domain = "R-explor"), 
                                                                              value = 10, min = 1, max = 100, step = 1, width = "auto")),
                                                             htmlOutput("documenttable")
                                                    )
                                        )
                               ),
                               
                               ## "Terms location" tab --------------------------------------------
                               
                               tabPanel(gettext("Terms location", domain = "R-explor"),
                                        h3(gettext("Terms location", domain = "R-explor")),
                                        HTML("<p>", gettext('Enter one or more terms :', domain = "R-explor"), "</p>"),
                                        fluidRow(
                                          column(8, textInput("location_terms", gettext("Terms", domain = "R-explor"), width = "100%")),
                                          column(4, radioButtons("location_terms_type", label = NULL,
                                                                 choices = c(gettext("Words", domain = "R-explor"),
                                                                             gettext("Phrase", domain = "R-explor"))))),
                                        tags$p(actionButton("launch_location_search", gettext("Search", domain = "R-explor"))),
                                        uiOutput("loctermsAlert"),
                                        h3(gettext("Selected terms location", domain = "R-explor")),
                                        tabsetPanel(type = "pills",
                                                    tabPanel(gettext("Plot", domain = "R-explor"),
                                                             p(htmlOutput("loctermplottext")),
                                                             plotOutput("loctermplot")
                                                    ),
                                                    tabPanel(gettext("Table", domain = "R-explor"),
                                                             DT::dataTableOutput("loctermtable")
                                                    )
                                        )
                               ),
                               

                                                              
                               ## "Help" tab -----------------------------------------------
                               
                               tabPanel(gettext("Help", domain = "R-explor"),
                                        h2(gettext("Help", domain = "R-explor")),
                                        
                                        h3(gettext("Most frequent terms", domain = "R-explor")),
                                        p(HTML(gettext("How to read the table :", domain = "R-explor"))),
                                        tags$ul(
                                          tags$li(HTML(gettext("<code>Term frequency</code> : number of times this term is found in the selected corpus", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Number of documents</code> : number of documents in the selected corpus in which this term appears at least once", domain = "R-explor"))),
                                          tags$li(HTML(gettext("<code>Percentage of documents</code> : percentage of documents in the selected corpus in which this term appears at least once", domain = "R-explor")))
                                        ),
                                        
                                        h3(gettext("Terms search", domain = "R-explor")),
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
                   code <- corpus_filtering_code("qco")
                   if (code != "") {
                     eval(parse(text = code))
                     return(tmp_corpus)
                   } else {
                     return(qco)
                   }
                 })
                 
                 ## Raw corpus filtered
                 raw_co <- reactive({
                   if (is.null(settings$raw_corpus)) return(co())
                   code <- corpus_filtering_code(settings$raw_corpus)
                   if (code != "") {
                     eval(parse(text = code))
                     return(tmp_corpus)
                   } else {
                     return(settings$raw_corpus)
                   }
                 })
                 
                 ## Tokenization code
                 tokens_code <- reactive({
                   code <- paste0("corpus_tokens <- tokens(%s, what='word'",
                                  ", remove_punct = ", input$treat_remove_punct, 
                                  ", remove_symbols = ", input$treat_remove_symbols, 
                                  ", remove_twitter = ", input$treat_remove_twitter, 
                                  ", remove_hyphens = ", input$treat_remove_hyphens, 
                                  ", remove_url = ", input$treat_remove_url, 
                                  ", remove_numbers = ", input$treat_remove_numbers, ")")
                   if (input$treat_tolower) {
                     code <- paste0(code, "\n", 
                                    "corpus_tokens <- tokens_tolower(corpus_tokens)")
                   }
                   if (!is.null(input$treat_thesaurus) && input$treat_thesaurus) {
                     code <- paste0(code, "\n", 
                                    "corpus_tokens <- tokens_lookup(corpus_tokens, dictionary(%s), exclusive = FALSE)")
                   }
                   if (!is.null(input$treat_stopwords) && input$treat_stopwords) {
                     code <- paste0(code, "\n", 
                                   "corpus_tokens <- tokens_remove(corpus_tokens, %s)")
                   }
                   ngrams <- utils::capture.output(dput(as.numeric(input$ngrams)))
                   if (ngrams != 1) {
                     code <- paste0(code, "\n", 
                                    "corpus_tokens <- tokens_ngrams(corpus_tokens, n = ", ngrams, ")")
                   }
                   code
                 })
                 ## DTM computation code
                 dtm_code <- reactive({
                   ngrams <- utils::capture.output(dput(as.numeric(input$ngrams)))
                   code <- paste0("dtm <- dfm(corpus_tokens, tolower = FALSE)\n")
                   if (input$treat_stem) {
                     code <- paste0(code, "dtm <- dfm_wordstem(dtm, language = '", input$treat_stem_lang, "')\n")
                   }
                   if (input$term_min_occurrences > 0) {
                     code <- paste0(code, "dtm <- dfm_trim(dtm, min_count = ", input$term_min_occurrences, ")")
                   }
                   code
                 })
                 corpus_dtm_code <- function(corpus_name, stopwords_name, thesaurus_name) {
                   out <- sprintf(tokens_code(), corpus_name, thesaurus_name, stopwords_name)
                   out <- paste(out, dtm_code(), sep = "\n")
                   out
                 }
                 
                 ## Clean corpus DTM filtered by n-grams
                 dtm <- reactive({
                   if (length(input$ngrams) == 0 || is.null(co()) || ndoc(co()) == 0) { 
                     return(NULL)
                   }
                   code <- corpus_dtm_code("co()", "settings$stopwords", "settings$thesaurus")
                   eval(parse(text = code))
                   dtm
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
                 
                 ### FREQUENT TERMS -----------------------------------------------------------
                 
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
                 
                 ### TERMS SEARCH ---------------------------------------------------------------------
                 
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
                 
                 ## Run the query on the document term matrix as environment,
                 ## and returns the result
                 terms_query <- reactive({
                   error <- NULL
                   if (length(terms()) == 0) return(list(res = NULL, error = NULL))
                   dtm_terms <- dtm() %>% 
                     dfm_select(pattern = terms(), valuetype = "fixed", selection = "keep") %>% 
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

                ## Searched terms frequency plot
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
                    if (is.character(var) || is.factor(var)) {
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
                 
                 
                 ## Observers on pagination events
                 observeEvent(input$prev_documents, {
                   updateNumericInput(session, "start_documents", value = max(1, isolate(input$start_documents) - isolate(input$nb_documents_display)), min = 1)
                 })
                 observeEvent(input$next_documents, {
                   val <- isolate(input$start_documents) + isolate(input$nb_documents_display)
                   if (val <= nb_docs_term()) {
                     updateNumericInput(session, "start_documents", value = val)
                   }
                 })
                 
                 ## Documents list
                 output$documenttable <- renderText({
                   ## Acquire dependencies
                   input$launch_search
                   input$doc_metadata
                   input$doc_corpus
                   input$doc_display
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
                       out <- paste(out, "<p><strong>", rownames(docvars(co()))[i] ,"</strong></p>")
                       if (is.null(input$doc_corpus) || req(input$doc_corpus) == "clean") {
                         tmp_corp <- co()
                       } else { 
                         tmp_corp <- raw_co()
                       }
                       if (input$doc_display == "Documents") {
                         out <- paste(out, explor_corpus_highlight(tmp_corp[i], terms()))                       
                       }
                       if (input$doc_display == "Kwics") {
                         kwics <- kwic(tmp_corp[i], pattern = terms(), window = 7, valuetype = "fixed")
                         kwics$text <- paste("...", kwics$pre, strong(kwics$keyword), kwics$post, "...")
                         kwics <- paste(kwics$text, collapse = "<br />")
                         out <- paste(out, kwics)
                       }
                       if (input$doc_metadata) {
                         meta <- docvars(co())[i,]
                         metadata <- character(0)
                         for (m in colnames(meta)) {
                           metadata <- append(metadata, paste0("<span>", m, "</span>: <code>", meta[, m], "</code>"))
                         }
                         metadata <- paste(metadata, collapse = "<br />")
                         out <- paste(out, HTML("<p class='metadata'>", metadata, "</p>"))
                       }
                       out <- paste(out, "</div>")
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

                 
                 ### TERMS LOCATION ---------------------------------------------------------------------
                 
                 ## Location terms
                 loc_terms <- reactive({
                   tmp <- unlist(stri_extract_all_words(input$location_terms))
                   if (length(tmp) == 1 && is.na(tmp)) return(NULL)
                   tmp <- tolower(tmp[tmp != ""])
                 })
               
                 ## Location terms kwic object
                 kwic_loc_terms <- reactive({
                   terms <- loc_terms()
                   if (is.null(terms)) {
                     return(NULL)
                   }
                   if (input$location_terms_type == "Phrase") {
                     terms <- phrase(input$location_terms)
                   }
                   kwic(co(), terms, window = 7)
                 })
                 ## Alert if location term is missing from corpus
                 output$loctermsAlert <- renderUI({
                   input$launch_location_search
                   isolate({
                     if (length(loc_terms()) > 0  && nrow(kwic_loc_terms()) == 0) {
                       div(class = "alert alert-warning",
                           HTML(paste(gettext("<strong>Warning :</strong> terms not found in corpus", domain = "R-explor"))))
                     }
                   })
                 })

                 ## Terms location table
                 output$loctermtable <- DT::renderDataTable({
                   input$launch_location_search
                   isolate({
                     if (is.null(kwic_loc_terms()) || nrow(kwic_loc_terms()) == 0) {
                       return(DT::datatable(data.frame(table = character())))
                     }
                     tab <- data.frame(kwic_loc_terms())
                     tab$text <- paste(tab$pre, strong(tab$keyword), tab$post)
                     tab <- tab %>% select("docname", "from", "to", "text") 
                     tab <- DT::datatable(tab, options = c(tableOptions), rownames = FALSE, escape = FALSE)
                   })
                   tab
                 })
                 
                 ## Maximum number of documents to display location plot
                 kwic_loc_nb_max <- 80
                 ## Terms location plot text
                 output$loctermplottext <- renderText({
                   input$launch_location_search
                   isolate({
                     text <- ""
                     kw <- kwic_loc_terms()
                     if (is.null(kw) || nrow(kw) == 0) {
                       return(gettext("No document found", domain = "R-explor"))
                     } else {
                       text <- paste0(nrow(kw), gettext(" documents found. ", domain = "R-explor"))
                     }
                     if (nrow(kw) > kwic_loc_nb_max) {
                       text <- paste0(text,
                                     sprintf(gettext("<strong>Only the first %s ones are displayed.</strong>", domain = "R-explor"), kwic_loc_nb_max))
                     }
                     return(HTML(text))
                   })
                 })
                 ## Terms location plot
                 output$loctermplot <- renderPlot({
                   input$launch_location_search
                   isolate({
                     kw <- kwic_loc_terms()
                     if (is.null(kw) || nrow(kw) == 0) return(NULL)
                     if (nrow(kw) > kwic_loc_nb_max) kw <- head(kw, kwic_loc_nb_max)
                     g <- textplot_xray(kw)
                   })
                   g
                 }, height = 650)
                 
                 
                 ### CODE EXPORT ---------------------------------------------------------------------
                 
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
                   code <- paste0(code, corpus_dtm_code(corpus_name, settings$stopwords_name, settings$thesaurus_name))
                   code <- formatR::tidy_source(text = code, 
                                                width.cutoff = 75, 
                                                output = FALSE)$text.tidy
                   showModal(modalDialog(
                     title = gettext("Export R code", domain = "R-explor"), size = "l", 
                     HTML(paste0(gettext("Copy, paste and run the following code in your script to compute the corresponding document-term matrix (DTM) :", domain = "R-explor"),
                                 "<pre><code>",
                                 paste(highr::hi_html(code), collapse = "\n"),
                                 "</code></pre>")),
                     easyClose = TRUE))
                 })
                 
               })
}