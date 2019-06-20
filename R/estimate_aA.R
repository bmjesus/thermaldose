#' @title Estimates aA and TA parameters
#' @description Estimates aA and TA parameters
#' @param temperatures numeric vector with the tested temperatures
#' @param half_life numeric vector with the half-life parameters for each tested temperature, estimated using the fit_half_life() function
#' @param aA Starting value for the aA parameter (numeric)
#' @param TA Starting value for the TA parameter (numeric)
#' @return A list with the two fitted parameters, i.e. aA and TA
#' @keywords internal
#' @export
estimate_aA<-function(temperatures, half_life, aA,TA){

  #fitting the non-linear model
  nls_model<-  minpack.lm::nlsLM(half_life~exp(aA*(temperatures-TA)),start = list(aA=aA,TA=TA),trace = TRUE)

  #creating a new x-axis with more values
  new_x<-seq(min(temperatures),max(temperatures),by=1)

  #plotting the results
  plot(temperatures,log(half_life),
       ylab="Half-life (log)",xlab="Temperature (C)",pch=21,bg=2)

  lines(new_x,log(predict(nls_model,newdata = data.frame(temperatures=new_x))))

  #storing the results in a list
  my_list<-list(aA=coefficients(nls_model)[1],TA=coefficients(nls_model)[2])

  return(my_list)

}
