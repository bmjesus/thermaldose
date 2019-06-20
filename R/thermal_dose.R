#' @title Calculates the thermal dose
#' @description Estimates aA and TA parameters
#' @param stress numeric vector with the time each thermal stress was applied.
#' @param activity numeric vector with the activity associated to each stress (in percentage)
#' @param aA aA parameter value estimated using the estimate_aA() function
#' @param TA TA parameter value estimated using the estimate_aA() function
#' @return numeric vector with the thermal dose associated to each temperature/time tested
#' @keywords internal
#' @export

thermal_dose<-function(stress_time,activity,aA,TA){

  td<-stress_time* exp(-aA*(activity - TA))

  return(td)

  }

