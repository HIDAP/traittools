#' Calculation of trait variables
#' @description A generalized function to calculate all the posibles derivated trait from Fieldbook. Use as a 
#' backend sbformulas.
#' @param fb Data frame with the fieldbook
#' @param plot_size plot size
#' @param plant_den plant density
#' @param mgt Crop Managment Data
#' @param mtl Material List Data
#' @param trial_type the type of trial
#' @export
#'   
calculate_trait_variables <- function(fb, plot_size=NA, plant_den=NA, mgt, mtl, trial_type=NA){

  fieldbook_names <- names(fb)
  fieldbook<- sbcalculate(fb = fb,plot.size=plot_size,plant.den=plant_den)
  
  if(trial_type =="late blight"){
    
    rel.days <- get.rel.days(mgt)
    lb.control <- get.lb.control(mtl)
    
    lbf = c("LB1","LB2","LB3","LB4","LB5","LB6","LB7","LB8","LB9","LB10","LB11","LB12")
    yy = names(fieldbook)
    
    if("AUDPC" %in% names(fieldbook)){
      fieldbook=within(fieldbook,{
        AUDPC  = xaudpc(
          eval = fieldbook[,yy[yy %in% lbf]] ,
          #eval = cbind(LB1,LB2,LB3,LB4,LB5,LB6,LB7) ,
          dates= rel.days,
          type = "absolute")
      })
    }
    
    if("rAUDPC" %in% names(fieldbook)){
      fieldbook=within(fieldbook,{
        rAUDPC	= xaudpc(
          eval = fieldbook[,yy[yy %in% lbf]] ,
          #eval = cbind(LB1,LB2,LB3,LB4,LB5,LB6,LB7) ,
          dates= rel.days,
          type = "relative")
      })
    }
    
    if("SAUDPC" %in% names(fieldbook)){
      fieldbook=within(fieldbook,{
        SAUDPC	= saudpc(INSTN,AUDPC,REP, lb.control)
      })
    }
    
    
  }
  
  fieldbook <- fieldbook[,fieldbook_names]
  
  return(fieldbook)
  
}
