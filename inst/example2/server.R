library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)
library(date)
library(agricolae)
library(doBy)

shinyServer(function(input, output, session) {
  
  volumes <- shinyFiles::getVolumes()
  shinyFileChoose(input, 'file', roots=volumes, session=session,
                  restrictions=system.file(package='base'),filetypes=c('xlsx'))
  #shinyFileSave(input, 'save', roots=volumes, session=session, restrictions=system.file(package='base'))

  hot_path <- reactive ({
    
    validate(
      need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    )
    
    if(length(input$file)==0){return (NULL)}
    if(length(input$file)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
    }
  })
  
  hot_params <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Installation")
      #hot_design <- get_fb_param(hot_param,"Experimental design")
      hot_design <- get_fb_param(hot_param,"Experimental_design")
      
      #hot_design <- get_fb_param(hot_param,"Experimental design")
      hot_design <- get_fb_param(hot_param,"Experimental_design")
      
      #hot_plot_size <- get_fb_param(hot_param,"Plot size (m2)")
      hot_plot_size <- get_fb_param(hot_param,"Plot_size_(m2)")
      
      #hot_plant_den <- get_fb_param(hot_param,"Planting density (plants/Ha)")
      hot_plant_den <- get_fb_param(hot_param,"Planting_density_(plants/Ha)")
      
      hot_params_list <- list(hot_design = hot_design, hot_plot_size = hot_plot_size,
                              hot_plant_den =  hot_plant_den)
    }
  })  
  
  hot_crop <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
      hot_crop <- get_fb_param(hot_param,"Crop")# In DC and HiDAP 
      
    }
  })
  
  hot_trial <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
      #hot_trial <- get_fb_param(hot_param,"Type of Trial") in DataCollector
      hot_trial <- get_fb_param(hot_param,"Type_of_Trial") #in HiDAP
    }
  })
  
  ###extra code
  hot_mgt <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      #hot_mgt <- reactive_excel_metadata(file_id = hot_file , "Crop_management")
      #hot_mgt <- openxlsx::read.xlsx(xlsxFile = hot_file, sheet = "Crop_management") #in DataCollector
      hot_mgt <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Crop_management", detectDates = TRUE) #in HiDAP
      hot_mgt
      #print(hot_mgt)
    }
  })

  hot_mtl <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      #hot_mtl <- reactive_excel_metadata(file_id =hot_file , "Material List")
      #hot_mtl <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Material List", detectDates = TRUE)
      hot_mtl <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Material_List", detectDates = TRUE) #
      hot_mtl
      #print(hot_mtl)
    }
  })
  #print(hot_mtl())
  
  ###end extra code
  
  output$hot_btable = renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_btable = hot_bdata()
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
    } else if (!is.null(values[["hot_btable"]])) {
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      
      #print(hot_mtl())
      #hot_file <- hot_path()
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
#       print(hot_plot_size)
#       print(hot_plant_den)
#       print(hot_trial())
#       print(hot_mtl())

      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      # print(DF)
      #DF <- sbformula::sbcalculate(fb = DF,plot.size =hot_plot_size, plant.den = hot_plant_den)
      DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      #print(DF)
    }
    
    if(!is.null(DF)){
      
      traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_fieldbook.rds")
      #print(DF)
      #print("DF")
      crop <- hot_crop()
      trial <- hot_trial()
      trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      
    }
    #}
    
  })
  
  output$hot_td_trait = renderRHandsontable({ 
    td_trait <- orderBy(~ABBR, td_trait)
    rhandsontable(data = td_trait)
    })
  
  
  shiny::observeEvent(input$exportButton, {
    
    withProgress(message = "Downloading Fieldbook and Applying Format...",value= 0,
                 {
                   DF <- readRDS("hot_fieldbook.rds")
                   #print("1")
                   trait <- get_trait_fb(DF)
                   crop <- hot_crop()
                   trial <- hot_trial()
                   #trait_dict <- get_crop_ontology(crop = crop,trial = trial)
                   trait_dict <- td_crop
                   #print("2")
                   #validator <- is.element(trait,trait_dict$ABBR)
                   #trait <- trait[validator]
                   #print("3")
                   hot_design <- as.character(hot_params()$hot_design)
                   
                   if(is.element("FACTOR", names(DF))){
                     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",factor="FACTOR",trait = trait, 
                                                   design = hot_design, trait_dict = trait_dict)
                   }
                   #print("4")
                   if(!is.element("FACTOR", names(DF))){
                     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",trait = trait, 
                                                   design = hot_design, trait_dict = trait_dict)
                   }
                   #print(summary)
                   #print("5")
                   hot_file <- hot_path() 
                   #print(hot_file)
                   #print("6")
                   wb <- openxlsx::loadWorkbook(hot_file)
                   sheets <- readxl::excel_sheets(path = hot_file)
                   #print("7")
                   if(is.element("Fieldbook",sheets)){    
                     openxlsx::removeWorksheet(wb, "Fieldbook")
                   }
                   #print("8")
                   if(is.element("Summary",sheets)){    
                     openxlsx::removeWorksheet(wb, "Summary")
                     # openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE) 
                   }
                   #print("9")
                   openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE)
                   openxlsx::writeDataTable(wb,sheet = "Fieldbook", x = DF,colNames = TRUE, withFilter = FALSE)
                   #print("10")
                   openxlsx::addWorksheet(wb = wb,sheetName = "Summary",gridLines = TRUE)
                   openxlsx::writeDataTable(wb,sheet = "Summary", x = summary ,colNames = TRUE, withFilter = FALSE)
                   #      
                   openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                   
                   traits <- traittools::get_trait_fb(DF)
                   #print("11")
                   traittools::col_validation_trait(file = hot_file,fbsheet = "Fieldbook",trait = traits,trait_dict = trait_dict)
                   #print("12")
                   traittools::col_trait_outlier(file = hot_file, sumsheet = "Summary",trait = trait)
                   #print("13")
                   shell.exec(hot_file)
                   
                 })
    
  })  
  
})
