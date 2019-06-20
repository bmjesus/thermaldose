#' @title Fits a model to an activity vs thermal dose and fits a shape parameter (n)
#' @description Fits a model to an activity vs thermal dose and fits a shape parameter (n)
#' @param activity numeric vector with the activity associated to each thermal dose (in percentage)
#' @param thermal_dose numeric vector with the thermal dose for each activity.
#' @param  aA a starting value for the shape parameter (n) to be fitted (numeric)
#' @return returns a list with three elements: the value of the fitted value for the shape parameter n, a numeric vector with thermal dose values and a numeric vector with predicted activity values.
#' @keywords internal
#' @export
model_thermal_dose<-function(activity,thermal_dose,n){

  plot(thermal_dose,activity,ylab="PSII activity (%)",xlab = "Thermal dose (units?)",
       pch=21,bg=2,las=1)

  #fitting a line to the plot

  final_model<-minpack.lm::nlsLM(activity~ activity[1]*exp(-thermal_dose^n),start = list(n=1),trace = TRUE)

  #creating a new x-axis with more values
  new_x<-seq(min(thermal_dose),max(thermal_dose),by=0.1)

  lines(new_x,predict(final_model,newdata = data.frame(thermal_dose=new_x)))

  #creating objects to export from the function
  n<-coefficients(final_model)
  x<-new_x
  predicted_values<-predict(final_model,newdata = data.frame(thermal_dose=new_x))

  return(list(n,x,predicted_values))

}
