#hotspot utils file
#functions to generate hotspot information

require(data.table)

# This function is one possible smoother. In this case, it is a v basic penalized cubic spline regression of
# y on x (for example, x=Date, y=smoothed cases)
cubic_spline_normal <- function(x,y,knot_interval=3) {
#  as.vector(predict(mgcv::gam(y~s(x,bs="cs"))))
  knots = floor(sum(!is.na(x))/knot_interval)
  tryCatch(expr = return(predict(mgcv::gam(y~s(x,bs="cs",k=knots),model=F),data.frame(x=x))),
           error = function(e) return(as.double(rep(NA,length(x)))))
}


# This function is one possible smoother. In this case, it is a v basic penalized cubic spline regression of
# y on x (for example, x=Date, y=smoothed cases)
cubic_spline_poisson <- function(x,y,knot_interval=3) {
  #Note that for poisson, x cannot be less than zero
  y[y<0] <- NA
  knots = floor(sum(!is.na(x))/knot_interval)
  tryCatch(expr = return(exp(predict(mgcv::gam(y~s(x,bs="cs",k=knots),model=F, family="poisson"),data.frame(x=x)))),
           error = function(e) return(as.double(rep(NA,length(x)))))
}


# This function will the smoothed values over a grouped (i.e. by) dataframe (dt), given x, y, and a method (i.e. smoothing function)
gen_smooth <- function(dt, x="Date", y="Confirmed", by="FIPS", smoother=cubic_spline_normal, knot_interval=3,verbose=T) {
  if(verbose) cat(paste0("This may take some time... smoothing over ", by),"\n")
  elapsed <- system.time({
    #this is the key line of the function. It estimates the smoothed curve, by group, using the smoother function passed
    sm <- dt[,smoother(as.numeric(get(x)), get(y),knot_interval=knot_interval), by=get(by)]$V1
  })
  if(verbose) cat("\t",paste0("Complete in ",round(elapsed[3],2)," seconds."),"\n")
  return(sm)
}

# This function numerically estimates derivative, using arrays of equal length
deriv <- function(x,y) {
  d = data.table(x,y)[,(y-shift(y,-1))/as.numeric(x-shift(x,-1))][1:(length(x)-1)]
  return(c(d,d[length(d)]))
}

#This is a very fast way to get derivative, by group.(Note, the dt is assumed to be passed in the correct sort order)
# x and y and bycol are string variable names that must exist in dt
# deriv_by_group <- function(dt,x,y,bycol) {
#   fderiv <- deriv(dt[[x]],dt[[y]])
#   row_nums <- dt[,.I[.N], by=bycol]$V1
#   fderiv[row_nums] <- NA
#   fderiv <- zoo::na.locf(fderiv,na.rm=F)
#   return(fderiv)
# }

#Wrapper to get smooth, first, second derivative of that smooth
get_smoothing_vars <- function(df, date_col="Date", 
                               raw_col = "Confirmed",
                               sm_col_name = "Smoothed",
                               smooth_alg = cubic_spline_normal,
                               knot_interval=3,
                               geo_level=NULL) {
  res = setDT(df)
  

  #get the smoothed values
  res[, (sm_col_name) := smooth_alg(as.numeric(get(date_col)), get(raw_col),knot_interval=knot_interval), by=geo_level]
  #get the first and second deriv
  res[, fderiv := deriv(as.numeric(get(date_col)), get(sm_col_name)), by=geo_level]
  res[, sderiv := deriv(as.numeric(get(date_col)), fderiv), by=geo_level]
  res[]
}


#Hot spot algorithm
#Function to get hotspot information
get_hotspots <- function(srcdt, #                   source data frame
                         date=F, #    default is all dates, but can set to specific date
                         target_column, # string name of target outcome
                         trend_function = c("deriv","day-over-day"),
                         cumul_inc_threshold = 100, #default is 18/100,000,
                         abs_cases_threshold = 500, #default is 500,
                         retain_indicators=F, #     keep the indicator variables generated while estimating hotspot defintion?
                         other_keep_vars=NULL,
                         geo_level=NULL) { #      default is to not group by, but could do so, if had many hotspots to calculate.

  trend_function = match.arg(trend_function)
  
  #set to copy, so the original data frame is not changed
  res = copy(srcdt)
  
  #get the total new cases in the past 7 days
  res[,conf_last_week := Reduce(`+`, shift(get(target_column),0:6)), by=geo_level]
  #get the 7-day cumulative incidence
  res[,cumul_inc_7_days:=conf_last_week*100000/Population]
  
  #get total new cases per 100,000 for each day
  res[,conf_per_100k:=get(target_column)*100000/Population]
  
  if(trend_function=="deriv") res[,trend:=first_derivative_incr(fderiv), by=geo_level]
  if(trend_function=="day-over-day") res[,trend:=day_over_day_inc_avg(conf_per_100k), by=geo_level]
  

  #indicator for >50 in past week
  res[,conf_prior_wk_exceeds50 := conf_last_week>50]
  
  res[,hotspot:=conf_prior_wk_exceeds50 & trend & (conf_last_week>abs_cases_threshold | cumul_inc_7_days>cumul_inc_threshold)]
  
  #restrict to date of interest if date not F
  if(!(date==F)) res <- res[Date==as.Date(date),]
  
  #get vector of variable names to keep
  keepvars = c(geo_level,"Date","hotspot",other_keep_vars)
  #update this vector if retain_indicators has been requested
  if(retain_indicators) keepvars = c(keepvars,"conf_prior_wk_exceeds50","trend","conf_last_week","conf_per_100k", "cumul_inc_7_days")
  res <- res[,..keepvars]
  
  return(res[])

}


#Trend function #1 - simply returns whether or not the average of the past 7-days day-over-day inc change is positive
day_over_day_inc_avg <-function(inc) {
  change = inc-shift(inc,1)
  avgchange = Reduce(`+`,shift(change,0:6))/7.0
  return(avgchange>0)
} 

#Trend function #2 - returns positive if f' is increasing
first_derivative_incr <-function(deriv) {
  return(deriv>0)
} 

#Function to return trend_cateogry, based on first, and second derivative
derivative_trend_category <- function(fderiv,sderiv,outcome="Cases") {
  return(dplyr::case_when(
    fderiv>0 & sderiv>0~paste0(outcome, " Increasing (Accelerating)"),
    fderiv>0 & sderiv<=0~paste0(outcome, " Increasing (Decelerating)"),
    fderiv<0~paste0(outcome, " Decreasing")
  ))
}
 

# test functions
 # library(rawcoviddata)
 # library(data.table)
 # all_us_counties <- us_empirical_by_level("csse")$county
 # all_md_counties <- all_us_counties[USPS=="MD"]
 # howard <- all_us_counties[FIPS=="24027"]
 # 
 # #summarize a county in terms of smoothing and derivativees
 # howard <- get_smoothing_vars(howard)
 # howard_hs <- get_hotspots(howard,target_column="Smoothed",retain_indicators = T, other_keep_vars = c("Smoothed","Confirmed"))
 # 
 # #summarize a state in terms of smoothing and derivativees
 # all_md_counties <- get_smoothing_vars(all_md_counties,geo_level = "FIPS")
 # all_md_counties_hs <- get_hotspots(all_md_counties,
 #                                    target_column="Smoothed",
 #                                    retain_indicators = T, 
 #                                    other_keep_vars = c("Smoothed","Confirmed"),
 #                                    geo_level = "FIPS")
 # 