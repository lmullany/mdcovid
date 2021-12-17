library(data.table)
library(ggplot2)

  #df <- fread("https://opendata.arcgis.com/datasets/5f459467ee7a4ffda968139011f06c46_0.csv")
  df <- jsonlite::fromJSON("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_MASTER_ZIP_CODE_CASES/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  df <- setDT(df$features[[1]])
  
  newnames= c("id","zip",stringr::str_extract(names(df)[3:length(names(df))],"(?<=(F)|(total))[0-9]{1,2}_[0-9]{1,2}_202[01]"))
  setnames(df,new=newnames)
  df <- melt(df,id.vars = c("id","zip"),variable.name = "date",value.name = "cases")
  df[,date:=as.IDate(as.Date(date,"%m_%d_%Y"))]
  df[,zip:=as.integer(zip)]
  
  #get daily
  setorder(df,zip,date)
  df[,daily:=cases-shift(cases,1),by=zip]
  
  pop <- fread("mdzippops.csv")
  #left join the population in
  df <- pop[,!"rank"][df,on="zip"]
  #get rate per 100,000
  df[,rate:=daily*100000/pop]
  
  # #get zip code names, at least for baltimore
  # zipcodenames <- setnames(fread("baltimore_county_zips"),new=c("zip","zname","countyname"))
  # #drop duplicate baltimore rows
  # zipcodenames <- zipcodenames[zname!="Baltimore",.("zname" = paste0(unique(zname),collapse=",")),by=zip]
  # #left join to the df
  zipcodenames <- fread("zipcodes_maryland.csv")
  zipcodenames[county!="Baltimore City", county:=paste0(county," County")]
  zipcodenames[,zname:=paste0(zip,": ",name,", ",county)]
  
  df <- zipcodenames[df,on="zip"]


smooth_predictions <- function(x,y) {
  tryCatch(expr = return(predict(mgcv::gam(y~s(x,bs="cs"),model=F),data.frame(x=x))),
           error = function(e) return(as.double(rep(NA,length(x)))))
}
