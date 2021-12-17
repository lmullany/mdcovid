rm(list=ls())
library(jsonlite)
library(data.table)
zipcase_json <- "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_MASTER_ZIP_CODE_CASES/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
system.time(zip <- fromJSON(zipcase_json))

zipcase_url <- "https://opendata.arcgis.com/datasets/5f459467ee7a4ffda968139011f06c46_0.csv"
system.time(zip <- data.table::fread(zipcase_url))

caserates_url <- "https://opendata.arcgis.com/datasets/1c17f788b0c14843bfee185bb9c5516c_0.csv"
system.time({
  caserates <- melt(
    data.table::fread(caserates_url)[,date:=as.IDate(stringr::str_sub(ReportDate,1,10))][
      ,!c('OBJECTID','ReportDate')],
    id.vars="date")
})

casecounts_url <- "https://opendata.arcgis.com/datasets/0573e90adab5434f97b082590c503bc1_0.csv"
system.time({
  casecounts<- melt(
    data.table::fread(casecounts_url)[,date:=as.IDate(stringr::str_sub(DATE,1,10))][
      ,!c('OBJECTID','DATE')],
    id.vars="date")
  casecounts[,daily:=value-shift(value,1),by=variable]
})

caserates[casecounts,on=c("date","variable")][variable=="Allegany"][, pop:=daily*100000/value][] %>% as.data.frame()


