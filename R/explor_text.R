if (getRversion() >= "2.15.1")  
  utils::globalVariables(c("group", "nb_docs", "nb_terms", "nom", "prop_docs", "term", "tmp_corpus"))

##' @import shiny

explor_text_css <- function() {
  shiny::HTML("
              body {margin: 0 15px;}
              
              #sidebar {padding: 5px 10px;}
              
              #sidebar label, 
              #sidebar input, 
              #sidebar select, 
              #sidebar option,
              #sidebar a,
              div.option,
              input, label, select, option, .selectize-input {
                  font-size: 12px !important;
                  height: auto !important;
              }
              
              #sidebar .btn { padding: 6px 10px; }
              
              #filters .shiny-input-checkboxgroup .shiny-options-group {
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

              #get_r_code { 
                  font-size: 14px !important; 
                  margin-top: 15px; 
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
              .dataTables_wrapper label {
                  font-weight: normal;
                  font-size: 90%;
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