#' Conditional Trait Render for Fieldbook Web Table
#' @description This function highlight all the values which are out of range in a fieldbook WebTable. 
#' @param fieldbook The fieldbook data
#' @param trait The abbreviation(s) of the trait(s) used in fieldbooks.  
#' @param trait_dict The trait dictionary on crop ontology format.
#' @return An web table spreadsheet with conditional format according trait conditions 
#' @export
#' 

col_render_trait <- function(fieldbook,trait,trait_dict){
  
  #n <- length(trait)
  fieldbook <- as.data.frame(fieldbook)
  fieldbook$PLOT <- as.integer(fieldbook$PLOT)
  fieldbook$REP <- as.integer(fieldbook$REP)
  fieldbook$INSTN <- as.factor(fieldbook$INSTN)
  if("FACTOR" %in% names(fieldbook)) fieldbook$FACTOR<- as.factor(fieldbook$FACTOR)
  if("PHASE" %in% names(fieldbook)) fieldbook$PHASE<- as.factor(fieldbook$PHASE)
  if("IDENTIFIED_CRITERIA" %in% names(fieldbook)) fieldbook$IDENTIFIED_CRITERIA<- as.factor(fieldbook$IDENTIFIED_CRITERIA)
  if("STYPE" %in% names(fieldbook)) fieldbook$STYPE<- as.factor(fieldbook$STYPE)
  
  
  #Validator validates all the trait which produce render_values different from ("")
  #validator <- lapply(trait,function(x) v <- render_trait(trait = x,trait_dict = trait_dict))
  validator <- lapply(trait,function(x) v <- render_trait_ext(data=fieldbook, trait = x,trait_dict = trait_dict))
  
  trait <- trait[validator!=""]
  n <- length(trait)

  out_temp <- list()
  renderer_trait <-  list()
  for(i in 1:n){
      out_temp[[1]]<- rhandsontable::rhandsontable(data = fieldbook, readOnly = FALSE, useTypes = TRUE) #%>%  
      #renderer_trait[[i]] <- render_trait(trait[i],trait_dict)
      renderer_trait[[i]] <- render_trait_ext(data=fieldbook, trait[i], trait_dict)
      #if(renderer_trait[[i]]==""){print("no render trait column")}
      #if(renderer_trait[[i]]!=""){
      j <- i+1
      out_temp[[j]] <- rhandsontable::hot_col(hot = out_temp[[i]], col = trait[i] ,readOnly = FALSE,
                               allowInvalid = TRUE,copyable = TRUE, renderer = renderer_trait[[i]]) 
      #}
      
  } 
  #k <- n+1
  #out_temp[[k]] 
  #if(export){
  k <- n+1
  out_temp[[k]] %>%
    rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    rhandsontable::hot_cols(fixedColumnsLeft = 3)  %>%
    rhandsontable::hot_rows(fixedRowsTop = 1)
  
  
#     hot_cols(colWidths = 100) %>%
#     hot_rows(rowHeights = 50)
  
#   out_temp[[k]] %>%
#     rhandsontable::hot_context_menu(
#       customOpts = list(
#         csv = list(name = "Download to CSV",
#                    callback = htmlwidgets::JS(
#                      "function (key, options) {
#                      var csv = csvString(this);
#                      var link = document.createElement('a');
#                      link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
#                      encodeURIComponent(csv));
#                      link.setAttribute('download', 'data.csv');
#                      document.body.appendChild(link);
#                      link.click();
#                      document.body.removeChild(link);
#        }"))))
#}
  
} 
