#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
 
#################
#'Get the type of trait into the trait dictionary
#' @param trait trait abreviation
#' @param trait_dict trait dictionary
#' @author omar benites
#' @return The type of trait
#' @examples
#' \dontrun{
#' # The data
#' data(potato_yield)
#' #str(potato_yield)
#' # NTP: Number of tuber planted
#' get_trait_type("NTP", potato_yield)
#' }
#' @export
#' 
#' 
get_trait_type <- function(trait,trait_dict)
{
  tp <- as.character(trait_dict[trait_dict$ABBR==trait,c("TYPE")])
  #tp <- na.exclude(tp)
  stringr::str_trim(tp,side="both")
  
  if(is.na(tp) || length(tp)==0){
    tp <- "none"
  }
  tp
  #return(tp)
}

#######################################################################
#'Get the full name of the trait into crop trait dictionary
#'
#' @param trait trait abreviation
#' @param trait_dict trait dictionary
#' @author omar benites
#' @return The full name of the trait
#' @examples
#' \dontrun{
#' # The data
#' data(potato_yield)
#' #str(potato_yield)
#' # NTP: Number of tuber planted
#' get_trait_name("NTP", potato_yield)
#' }
#' @export
#' 
get_trait_name <- function(trait,trait_dict)
{
  tp <- as.character(trait_dict[trait_dict$ABBR==trait,c("VAR")])
  tp <- na.exclude(tp)
  stringr::str_trim(tp,side="both")[1]
  
  if(is.na(tp) || length(tp)==0){
    tp <- "It not Defined"
  }
  tp
  #return(tp)
}

######################################################################
#'Get the units of trait into the trait dictionary
#' @param trait trait abreviation
#' @param trait_dict trait dictionary
#' @author omar benites
#' @return The units of the trait in which has been measured
#' @examples
#' \dontrun{
#' # The data
#' data(potato_yield)
#' #str(potato_yield)
#' # NTP: Number of tuber planted
#' get_trait_units("NTP", potato_yield)
#' }
#' @export
#' 
#' 
get_trait_units <- function(trait,trait_dict)
{
  tp <- as.character(trait_dict[trait_dict$ABBR==trait,c("UNITS")])
  #tp <- na.exclude(tp)
  stringr::str_trim(tp,side="both")
  
  if(is.na(tp) || length(tp)==0){
    tp <- "none"
  }
  tp
  #return(tp)
}

########################################################################
#'Get the crop which one trait belong
#' @param trait trait abreviation
#' @param trait_dict trait dictionary
#' @author omar benites
#' @return The crop 
#' @examples
#' \dontrun{
#' # The data
#' data(potato_yield)
#' #str(potato_yield)
#' # NTP: Number of tuber planted
#' get_trait_crop("NTP", potato_yield)
#' }
#' @export
#' 
get_trait_crop <- function(trait,trait_dict)
{
  #print(trait)
  tp <- as.character(trait_dict[trait_dict$ABBR==trait,c("VAR")])
  #tp <- na.exclude(tp)
  stringr::str_trim(tp,side="both")
  
  
  if(is.na(tp) || length(tp)==0){
    tp <- "none"
  }
  tp
  #return(tp)
}

#########################################################################
#'Get a scale condition for quantitative or cualitative traits
#'
#' @param trait trait abreviation
#' @param trait_dict trait dictionary
#' @author omar benites
#' @description Function to get the scale of differents trait, 
#' @examples
#' \dontrun{
#' # The data
#' data(potato_yield)
#' #str(potato_yield)
#' # NTP: Number of tuber planted
#' get_scale_trait("NTP", potato_yield)
#' }
#' @export
#' 
get_scale_trait <- function(trait,trait_dict){
  print(trait)
  tp <- get_trait_type(trait = trait,trait_dict = trait_dict)
  #tp <- na.exclude(tp)
  #print(tp)
  if(tp=="Continuous"||tp=="Discrete"||tp=="Numerical"){
    
    ll <- as.numeric(trait_dict[trait_dict$ABBR==trait,c("LOWER")])
    #In case trait is defined but dont have ll scale values will take 0
    if(is.na(ll)) {ll <- 0}
    #print(ll)
    ul <- as.numeric(trait_dict[trait_dict$ABBR==trait,c("UPPER")])
    #In case trait is defined but dont have ul scale values will take 100000
    if(is.na(ul)) {ul <- 1000000}
    #print(ul)
    output <- list(ll=ll,ul=ul)
  }
  
  if(tp=="Categorical"||tp=="Nominal"||tp=="Ordinal"){
    cat_scale <- trait_dict[trait_dict$ABBR == trait, c("CLASS1","CLASS2","CLASS3","CLASS4","CLASS5","CLASS6","CLASS7","CLASS8","CLASS9","CLASS10")]
    pattern <- "= .*$"
    cat_scale <- gsub(pattern=pattern,replacement = "",x = cat_scale)
    cat_scale <- suppressWarnings(as.numeric(cat_scale))
    cat_scale <- as.numeric(stringr::str_trim(cat_scale[!is.na(cat_scale)],side="both"))
    #In case trait is defined but dont have scale values will take 1,2,3,4,5
    if(length(cat_scale)==0) {cat_scale <- c(1,2,3,4,5)}
    output <- list(cat_scale=cat_scale)
  }
  
  if(tp=="none"){
    #Non-defined trait will be considered as quantative
    #print("none")
    ll <- 0
    ul <- 1000000
    #print(ll)
    #print(ul)
    output <- list(ll=ll,ul=ul)
  }
  
  invisible(output)
  
}

##############################################################################
#'RJavascript generator extended to render quantitative traits with javascript conditions and outlier detection
#'
#' @param ll lower limit values of the trait
#' @param ul upper limmit values of the trait
#' @param ol lower outlier bound
#' @param ou upper outlier bound
#' @author omar benites
#' @description Function to generate Javascript code for quantitative trait validation and outlier detection
#' in Rhansontable 
#' @export
#' 

render_quantitative_ext <- function(ll,ul,ol,ou){
  
  out <-  paste("function (instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);
              if (value <", ll," ) {
              td.style.background = 'pink';
              } else if (value >", ul,") {
              td.style.background = 'pink';
              } else if (value <=", ol,"  && value >=", ll,") {
              td.style.background = 'gold';
              } else if (value >=", ou,"   &&  value <=", ul,") {
              td.style.background = 'gold';
              }
              }",sep="")
  out
}

#####################################################################
#'RJavascript generator for quantitative traits in javascript conditions
#'
#' @param ll lower limit values of the trait
#' @param ul upper limmit values of the trait
#' @author omar benites
#' @description Function to generate Javascript code for cualitative trait validation in Rhansontable 
#' @export
#' 
render_quantitative <- function(ll,ul){
  out <- paste("function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments); 
               if (value <", ll ,"  ) {
               td.style.background = 'pink';
               } else if (value >", ul,") {
               td.style.background = 'pink';
               } 
}", sep="")
    #cat(out,"\n"," ")
  #return(out)
  out
  }

#########################################################################
#'RJavascript generator for cualitative trait renders
#'
#' @param scale_condition scale condition of the trait
#' @author omar benites
#' @description Function to generate Javascript code for quantitative trait validation in Rhansontable. 
#' @export
#' 
render_categorical <- function(scale_condition){ 
  out <- paste("function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (",scale_condition,"  ) {
               td.style.background = 'pink';
               } 
}",sep="")
    #cat(out,"\n"," ")
  #return(out)
  out
  } 

#########################################################################
#'Javascript Generator for Trait Renders usigin Trai Dictionary from Crop Ontology
#'
#' @param trait trait
#' @param trait_dict trait dictionary
#' @author omar benites
#' @description  Javascript code generator for render trait variables for Rhandsontable
#' @export 
#' 
render_trait<- function(trait,trait_dict){
  
  tp <- get_trait_type(trait,trait_dict)  
  scale_trait_values <- get_scale_trait(trait = trait,trait_dict = trait_dict)
  
  if(tp == "Continuous"|| tp == "Discrete"||tp=="Numerical"){
    
    ul <- scale_trait_values$ul  
    ll <- scale_trait_values$ll 
    
    #assign_trait <- paste("renderer_",trait," <- ",sep="") 
    render_trait <- render_quantitative(ll=ll,ul=ul)
    #render_trait_rule <- paste(assign_trait,"\"",render_trait,"\"")
    #cat("\"",render_trait,"\"","\n"," ")
    #out <- paste("\"",render_trait,"\"","\n"," ")
    out <- paste(render_trait)
    
  }
  
  if(tp =="Categorical"||tp=="Nominal"||tp=="Ordinal"){
    
    categorical_scale <- scale_trait_values$cat_scale
    n <- length(categorical_scale)
    scale_rule_first <- paste("value!=",categorical_scale[1], sep = "") #1st class of categorical trait
    scale_rule_global <- paste(scale_rule_first,"&&","value!=",categorical_scale[2:n]) #%>%  paste(.,collapse = " && ")
    
    scale_rule_global <- paste(scale_rule_global, collapse = " && ")
    
    #assign_trait <- paste("renderer_",trait," <- ",sep="") 
    render_trait <- render_categorical(scale_condition = scale_rule_global)
    #render_trait_rule <- paste(assign_trait,"\"",render_trait,"\"")
    #cat("\"",render_trait,"\"","\n"," ")
    out <- paste(render_trait)
  }
  
  if(tp=="none"){
    out <- print("none")
  }      
  out
  #return(out)
}

#########################################################################
#'Javascript Generator Extended for Trait Renders usigin Trai Dictionary from Crop Ontology
#'
#' @param data The name of the data frame.
#' @param trait trait
#' @param trait_dict trait dictionary
#' @author omar benites
#' @description  Javascript code generator for render trait variables for Rhandsontable
#' @export 
#' 

render_trait_ext<- function(data,trait,trait_dict){
  
  #print(trait)
  tp <- get_trait_type(trait,trait_dict)  
  scale_trait_values <- get_scale_trait(trait = trait,trait_dict = trait_dict)
  
  if(tp == "Continuous"|| tp == "Discrete"||tp=="Numerical"){
    
    ul <- scale_trait_values$ul  
    ll <- scale_trait_values$ll
    
    ol <- outlier_val(data[,trait])$ol
    ou <- outlier_val(data[,trait])$ou
    
    render_trait <- render_quantitative_ext(ll,ul,ol,ou)
    out <- paste(render_trait)
    
  }
  
  if(tp =="Categorical"||tp=="Nominal"||tp=="Ordinal"){
    
    categorical_scale <- scale_trait_values$cat_scale
    n <- length(categorical_scale)
    scale_rule_first <- paste("value!=",categorical_scale[1], sep = "") #1st class of categorical trait
    scale_rule_global <- paste(scale_rule_first,"&&","value!=",categorical_scale[2:n]) #%>%  paste(.,collapse = " && ")
    scale_rule_global <- paste(scale_rule_global, collapse = " && ")
    render_trait <- render_categorical(scale_condition = scale_rule_global)
    out <- paste(render_trait)
  }
  
  if(tp=="none"){
    #out <- print("")
    ul <- 10000000  
    ll <- 0
    
    ol <- outlier_val(data[,trait])$ol
    ou <- outlier_val(data[,trait])$ou
    
    render_trait <- render_quantitative_ext(ll,ul,ol,ou)
    out <- paste(render_trait)
  
  }      
  out
}

# rhandsontable(data = datos ,readOnly = FALSE) %>%
# hot_col("NOPS",renderer = renderer)  

