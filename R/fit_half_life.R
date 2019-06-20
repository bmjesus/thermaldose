#' @title Fitting the half life parameter to a curve activity vs time
#' @description Fitting the half life parameter to a curve activity vs time.
#' @param activity Numeric vector with activity in percentage (Ao should be percentage), activity could be any of the measured parameters, e.g. PSII efficiency, ETRmax, etc
#' @param my_time numeric vector with the duration of exposure (expressed in min or seconds)
#' @param half_life Starting value for the half_life parameter (numeric)
#' @param shape Starting value for the shape parameter (numeric)
#' @return Returns a list with the two fitted parameters, i.e. half_life and shape
#' @keywords internal
#' @export
fit_half_life<-function(activity, my_time,half_life,shape)
{

  #fitting the non-linear model
  nls_model<-minpack.lm::nlsLM(activity~activity[1]*exp(-(my_time/l)^n),
                               start=list(l=half_life,n=shape),
                               control=list(maxiter= 150))

  #creating a new x-axis with more values
  new_x<-seq(min(my_time),max(my_time),by=1)

  #plotting the results
  plot(my_time,activity,
       ylab="PSII efficiency (%)",xlab="Time (min)",pch=21,bg=2, ylim = c(0,100))
  lines(new_x,predict(nls_model,newdata = data.frame(my_time=new_x)))

  #storing the results in a list
  my_list<-list(half_life=coefficients(nls_model)[1],shape=coefficients(nls_model)[2])

  return(my_list)

}
