#' Detect outlier trait values of fieldbook data
#' @description this function calculates the outlier values using interquantile ranges to
#' determine atypical values in trait data.
#' @param trait_data a vector which contains the data of the trait
#' @param f parameter for determining exteme values. By default it's 3.
#' @return Return a list of two values ol(lower outlier bound) and lu(upper outlier bound) 
#' @author Raul Eyzaguirre
#' @export

outlier_val <- function(trait_data, f = 3){
  
  
  logical_val <- which(stringr::str_detect(trait_data,"[[:digit:]]")==FALSE)
  if(length(logical_val)>0) {trait_data <- as.numeric(trait_data[-logical_val])}
  
  #print(trait_data)
  #print("pass")
  linf <- (quantile(trait_data, 0.25, na.rm = T) - f * IQR(trait_data, na.rm = TRUE))[[1]]
  
  lsup <- (quantile(trait_data, 0.75, na.rm = T) + f * IQR(trait_data, na.rm = TRUE))[[1]]

  if(is.na(linf) && is.na(lsup)){
    linf <- -100000000000000
    lsup <-  100000000000000
  }
    
  if(linf == lsup){
    linf <- -100000000000000
    lsup <-  100000000000000
  }
  out <- list(ol = linf, ou = lsup)
}

#' Mode of the data.
#' @description An statistical function to obtain the mode.
#' @param x A vector which contains the trait data.
#' @param na.rm Logical value to ommit NA values
#' @export 

themode <-function(x,na.rm=TRUE){
  tv=table(x)
  paste(names(tv[tv==max(tv,na.rm=na.rm)]),collapse=", ")
}

#' Lenght of the values
#' @description A lenght modified function to manipulate data in case of missing values. 
#' @param x A vector
#' @param na.rm Logical value to ommit NA values
#' @export

length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else length(x)
}

#' Function to calculate summary statistics for fieldbook data from excel files
#' @description This function is capable of divide the information in categorial or quantitative data based on a data dictionary for potato and sweepotato.
#' If it is categorical, returns the count and mode. And if it is quantitative, returns the count, media and standart desviation.
#' @param fieldbook A data frame of the fieldbook
#' @param trait The name or position of the measured variable that will be summarized.
#' @param genotype The column name of the genotype
#' @param factor The column name of the factor
#' @param trait_dict The data frame of the data dictionary for potato and sweetpotato
#' @param na.rm A boolean that indicates whether to ignore NA
#' @return A data frame with the count, mean and standard desviation of the trait
#' @author Omar Benites
#' @export 

trait_summary <- function(fieldbook, trait, genotype=NA, factor = NA, trait_dict, na.rm = TRUE){
  #print(trait)
  if(missing(fieldbook)){
    stop("Please enter your data")
  }
  if(missing(trait)){
    stop("Please enter the name or position of the trait")
  }
  if(missing(genotype)){
    stop("Please enter the name of columns that contain genotypes")
  }
  
  
  datos <- as.data.frame(fieldbook)
  vvv <- datos[,trait]
  lbl <- names(datos[trait])   #extract the trait's label name
  measurevar <- lbl    #the trait's name
  tp <- get_trait_type(trait = trait, trait_dict = trait_dict )    #the type of variable
  print(tp)
  if(tp=="Continuous" || tp=="Discrete" || tp== "none"){
    # filter continuous and discrete data
    
    if(!is.element(factor,names(fieldbook))){
    formula <- as.formula(paste(measurevar, paste(genotype, collapse=" + "), sep=" ~ "))
    }
    
    if(is.element(factor,names(fieldbook))){
    formula <- as.formula(paste(measurevar, paste(c(genotype,factor), collapse=" + "), sep=" ~ "))
    }
    
    datac <- doBy::summaryBy(formula, data=fieldbook, FUN=c(length2,mean,sd), na.rm=na.rm)
    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- paste(measurevar,"_Mean",sep="")  
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- paste(measurevar,"_sd",sep = "")
  }  #Cuantitativa
  
  if(tp=="Categorical"){
    #filter categorical data
    #formula <- as.formula(paste(measurevar, paste(groupfactors, collapse=" + "), sep=" ~ "))
    
    if(!is.element(factor,names(fieldbook))){
      formula <- as.formula(paste(measurevar, paste(genotype, collapse=" + "), sep=" ~ "))
    }
    
    if(is.element(factor,names(fieldbook))){
      formula <- as.formula(paste(measurevar, paste(c(genotype,factor), collapse=" + "), sep=" ~ "))
    }
    
    datac <- doBy::summaryBy(formula, data=fieldbook, FUN=c(length2,themode), na.rm = na.rm) #quit the na.rm
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
    names(datac)[ names(datac) == paste(measurevar, ".themode", sep="") ] <- paste(measurevar,"_Mode",sep="")                               
  }  #Cualitativa
  
  return(datac)
}

#' Summarize data by statistical design
#' @description This function make a easy summary by clone (and treatment) based on statistical design
#' @param fieldbook A data.frame with the fieldbook data.
#' @param trait The column name of the trait into the fieldbook.
#' @param genotype The column name of the genotype
#' @param factor The column name of the factor
#' @param design The fieldbook design.
#' @param trait_dict A dataframe with the data dictionary based on crop ontologies.
#' @return Return a data.frame summary based on statistical designs.
#' @export
#'   

summary_by_design <-function(fieldbook, trait, genotype, factor,
                             design = "RCBD", trait_dict){
                             #design = "Randomized Complete Block Design (RCBD)", trait_dict){
  
  fieldbook <- as.data.frame(fieldbook)
  
  #if(design == "Randomized Complete Block Design (RCBD)"){
  if(design == "RCBD"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=NA, trait_dict = trait_dict, na.rm = TRUE)
  }
  
  #if (design == "Completely Randomized Design (CRD)"){
  if (design == "CRD"){ 
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=NA, trait_dict = trait_dict, na.rm = TRUE)
  }
  
  #if (design == "Two-Way Factorial in CRD"){
  if (design == "F2CRD"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=factor, trait_dict = trait_dict, na.rm = TRUE)
      }
  
  #if (design == "Two-Way Factorial in RCBD"){
  if (design == "F2RCBD"){  
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=factor, trait_dict = trait_dict, na.rm = TRUE)
    
  }
  
  #if (design == "Latin Square Design (LSD)"){
  if (design == "LSD"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=NA, trait_dict = trait_dict, na.rm = TRUE)
  }
  
  #if (design == "Augmented Block Design (ABD)"){
  if (design == "ABD"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=NA, trait_dict = trait_dict, na.rm = TRUE)   
  }
  
  #if (design == "Split Plot with Plots in CRD (SPCRD)"){
  if (design == "SPCRD"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=factor, trait_dict = trait_dict, na.rm = TRUE)
  }
  
  #if (design == "Split Plot with Plots in RCBD (SPRCBD)"){
  if (design == "SPRCBD"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=factor, trait_dict = trait_dict, na.rm = TRUE)
  }
  
  #if (design == "Strip Plot Design (STRIP)"){
  if (design == "STRIP"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=factor, trait_dict = trait_dict, na.rm = TRUE)
  }
  
  #if (design == "Alpha(0,1) Design (A01D)"){
  if (design == "A01D"){
    out <- trait_summary(fieldbook = fieldbook, trait = trait,
                         genotype= genotype, factor=NA, trait_dict = trait_dict, na.rm = TRUE)            
  }
  
  out
}

#Function to merge data.frames
merge.all <- function(x, y, bycol) {
   merge(x, y, all=TRUE, by=bycol)
}

#' Join all the summarized results of fieldbook's traits  
#' @description This function sumarizes one or more traits acording to trait_dictionary. 
#' It show the mean (quantitative), mode (cualitative),
#' standard desviation and length of the genotypes.
#' @param fieldbook A data.frame with the fieldbook data.
#' @param trait The column name of the trait into the fieldbook.
#' @param genotype The column name of the genotype
#' @param factor The column name of the factor
#' @param design The fieldbook design.
#' @param trait_dict The trait dictionary on crop ontology format.
#' @return An excel file with conditional format according trait conditions 
#' @export
#' 

trait_summary_join <- function(fieldbook, trait, genotype = NA, factor =NA, 
                               design= "Randomized Complete Block Design (RCBD)", trait_dict){

    if(!is.data.frame(fieldbook)){print("Please enter data.frame")}  
      
     trait <- trait
     summary_list <- lapply(trait, function(x) summary_by_design(fieldbook= fieldbook, trait = x,genotype = genotype,
                                                                  factor=factor, design = design, trait_dict = trait_dict))

    if(is.element(factor,names(fieldbook))){       
      output <- Reduce(function(...) merge.all(...,bycol=c(genotype,factor)), summary_list)
    }   
    
    if(!is.element(factor,names(fieldbook))){
        output <- Reduce(function(...) merge.all(...,bycol=genotype), summary_list)
    }
       
   output
}


# trait_summary_excel <- function(file,fbsheet,trait,genotype=NA,factor= NA,trait_dict){
#   
#   ext <- tools::file_ext(file)
#   if(ext!="xlsx"){ stop("traittools can not read .xls or .xlm files. Just xlsx")}
#   wb <- openxlsx::loadWorkbook(file)
#   
#   fieldbook <- readxl::read_excel(file,sheet = fbsheet)
#   fieldbook <- as.data.frame(fieldbook)
#   
#   trait <- get_trait_fb(fieldbook)
#   validator <- is.element(trait,trait_dict$ABBR)
#   trait <- trait[validator]
#   
#   summary <- trait_summary_join(fieldbook = fieldbook, genotype = genotype, factor= factor, trait =trait, 
#                                 design= "Randomized Complete Block Design (RCBD)", trait_dict= trait_dict)
# 
#   
#   sheets <- readxl::excel_sheets(path = file)
#   if("Summary" %in% sheets){    
#     openxlsx::removeWorksheet(wb, "Summary")
#    # openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE) 
#   }
#   
#   #wb <- loadWorkbook(file)
#   openxlsx::addWorksheet(wb = wb,sheetName = "Summary",gridLines = TRUE)
#   openxlsx::writeDataTable(wb,sheet = "Summary",x = summary ,colNames = TRUE, withFilter = FALSE)
#   openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE) 
#   
# }
# 
