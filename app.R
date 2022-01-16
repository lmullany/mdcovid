
library(shiny)
library(tidyverse)
library(data.table)
library(DT)

#load hotspot utils
source("www/hotspot_functions.R")
source("www/plotly_colored_trend_line_function.R")

getzips <- function(county_name,zips) {
  zips_for_this_county <- unique(zips[county==county_name, zname])
  return(zips_for_this_county)
}

get_data_subset <- function(inputs,df) {
  res <- copy(df)
  if(length(inputs$county)==1 && inputs$county=="All Counties") {
    res = res[,lapply(.SD,sum, na.rm=T), by=c("Date"), .SDcols=c("Population", "cumConfirmed", "Confirmed")]
    res[,zname:="All Zip Codes"]
    return(res)
  }
  else {
    if(is.null(inputs$zip)) inputs$zip="All Zip Codes"
    res = res[county %in% inputs$county]
    if(inputs$zip[1] != "All Zip Codes") {
      res = res[zname %in% inputs$zip] 
      res = res[,lapply(.SD,sum, na.rm=T), by=c("zname", "Date"), .SDcols=c("Population", "cumConfirmed", "Confirmed")]
    } else {
      res = res[,lapply(.SD,sum, na.rm=T), by=c("Date"), .SDcols=c("Population", "cumConfirmed", "Confirmed")]
      res[,zname:="All Zip Codes"]
    }
    return(res)
    #} else {
    #  res=res[zname %in% inputs$zip]
    #  return(res)
    #}
  }  
}

get_locale_data <- function(inputs,df) {
  res <- get_data_subset(inputs, df)
  if(nrow(res)<=0) return(NULL)
  
  if(inputs[["dist"]] == "poisson") {
    chosen_dist = cubic_spline_poisson
  } else {
    chosen_dist = cubic_spline_normal
  }
  
  #get smoothing data:
  res <- get_smoothing_vars(
    res,
    raw_col = "Confirmed",
    smooth_alg = chosen_dist,
    knot_interval = inputs$knot_interval,
    geo_level="zname")
  #get derivative trend
  res[,deriv_trend:=derivative_trend_category(fderiv,sderiv, outcome = "Cases")]
  #get the hotspot status and retain indicators
  res_hs <- get_hotspots(res,
                        target_column="Smoothed",
                        retain_indicators = T,
                        cumul_inc_threshold = inputs[["dailyinc"]]*7,
                        abs_cases_threshold = inputs[["abscases7"]]*7,
                        other_keep_vars = c("zname","Smoothed","Confirmed","deriv_trend","Population","fderiv"))
  
  res_hs[fderiv>0,hotspot:=TRUE]
  
  return(res_hs)
}

get_plot <- function(ginput, outcome, burdenline, burdenvalues=NULL, trans="identity", freey=T) {
  
  outcomename="Cases"
  
  if(outcome=="Rate") {
    outcomename="Rate (per 100 K)"
    outcome="Confirmed"
    ginput[,`:=`(Smoothed=Smoothed*100000/Population, Confirmed=Confirmed*100000/Population)]
    ginput[,deriv_trend:=stringr::str_replace(deriv_trend,"Cases","Rate (per 100 K)")]
    if(burdenline) {
      ginput[fderiv>0 & Smoothed>burdenvalues[1],hotspot:=T]
      ginput[fderiv<=0 | Smoothed<=burdenvalues[1],hotspot:=F]
    }
  } else {
    burdenline=F
  }
  scale_values = c("red", "orange","green")
  names(scale_values) <- c(paste0(outcomename, " Increasing (Accelerating)"),
                           paste0(outcomename, " Increasing (Decelerating)"),
                           paste0(outcomename, " Decreasing"))
  
  
  
  if(is.null(ginput)) return(NULL)
  trans = dplyr::case_when(
    trans %in% c("Normal","identity")~"identity",
    trans=="Square Root"~"sqrt",
    trans=="Log"~"log"
  )
  
  #what daily number of cases req'd for 7 day cumul inc > 100/100K?
  #its Population, divided by ~7000
  yaxisbreaks = round(c(pretty(ginput[[outcome]]),0))
  custom_caption="Source: Maryland Department of Health"
  if(burdenline) {
    inc_cutoff = unique(ginput$Population)*burdenvalues[1]/100000
    case_cutoff = burdenvalues[2]
    ylinevalue <- min(inc_cutoff, case_cutoff)
    yaxisbreaks = round(c(yaxisbreaks, ylinevalue),0)
    custom_caption=paste0("Horizontal line drawn at ",burdenvalues[1]," per 100,000)")
  }
  
  
  ginput <- ginput[,hotspot:=factor(hotspot)]
  ginput <- ginput[!is.na(hotspot) & Date>="2020-03-01" & get(outcome)>=0]
  
  plt <- ggplot(ginput) + 
    geom_point(aes(x=Date, y=!!ensym(outcome)),fill="black", shape=21, size=2) + 
    geom_path(aes(x=Date, y=Smoothed, color=deriv_trend, group=1), size=1.3) +
    facet_wrap(~zname, scales=ifelse(freey,'free_y','fixed')) + 
    scale_color_manual(values=scale_values) + 
    scale_y_continuous(trans=trans, breaks=yaxisbreaks) +
    ylab(outcomename) + 
    labs(fill="Hotspot Status", color="Trend", caption=custom_caption) +
    theme(legend.position="bottom", legend.background = element_blank(), legend.text = element_text(size = 8),
          legend.box="vertical" )

  #if(burdenline) plt <- plt + geom_hline(aes(yintercept=ylinevalue),size=1.3)
  if(outcomename=="Rate (per 100 K)") {
    plt <- plt + geom_hline(aes(yintercept=20),size=1.3)
  }

  plt <- plt + guides(fill = guide_legend(order=1),color = guide_legend(order=2))

  return(plt)
}

get_plotly <- function(ginput, outcome, burdenline, burdenvalues=NULL, trans="identity", freey=T) {
 plt <- plot_trend_line(ginput) 
}

prepare_table <- function(srcdata, outcome) {
  
  newnames <- c("Date","Hotspot","Zip Code","Smoothed Cases", "Confirmed Cases", "Deriv Trend","Population","First Deriv",
                            "Cases in Prior Wk>50","Trend","Conf Cases Prior Wk", "Daily Cases per 100K","Cumul Inc Prior Wk")
  if(outcome=="Deaths") {
    newnames <- str_replace_all(newnames,"Cases","Deaths")
  }
  
  res <- tibble(copy(srcdata))
  res <- res %>%  rename_with(~newnames) %>% select(3,1,5,4,6,9,11,12,13) %>% 
    #round any numeric variables to two digits
    mutate_if(.predicate = is.numeric, round, digits=2) %>% 
    #arrange by date (most recent date first)
    arrange(desc(Date))
  return(res)
  
}
  
ui <- fluidPage(
  ##theme and title div
  theme="styles.css",
  div(class="myTitle", titlePanel("COVID-19 MD / County / Zip Hotspots")),
  wellPanel(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "countyselect", label="Select County: ", choices=NULL),
        selectizeInput(inputId = "zipselect",label = "Select Zip: ", choices = NULL, multiple=TRUE),
        radioButtons(inputId = "outcome", label="Outcome: ", choices=c("Cases" = "Confirmed", "Daily Rate per 100K" = "Rate"), inline=T, selected="Rate"),
        numericInput(inputId = "knot_interval",label="Smoothing knot-interval (weeks, default=3)",value=3,min=1,max=52,step=1),
        radioButtons(inputId = "dist", label="Distribution", choices=c("Gaussian" = "gaussian","Poisson" = "poisson"),selected = "poisson")
        # checkboxInput(inputId = "includeBurdenInd", label="Add threshold line?", value=TRUE),
        # conditionalPanel(condition = "input.outcome=='Rate'",
        #                  numericInput(inputId = "dailyinc",label = "Smoothed Daily Cases per 100,000",value = 18,min=0,step = 1)
        # )
        # 
      
      ),
      mainPanel(
        htmlOutput("location"),
        #htmlOutput("textdatasrc"),
        tabsetPanel(
          tabPanel("Trend Plot", 
                   plotlyOutput(outputId="ctplot"),
                   radioButtons(inputId = "trans",label = "Y-axis transform?: ", choices =c("Normal","Square Root", "Log"), inline=T),
                   checkboxInput(inputId = "freey",label= "Independent Y Scales?",value=T)
                   )
          ,tabPanel("Data", dataTableOutput(outputId = "cttable"),style="font-size:50%")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #Need a reactive poll function to get the data
  sourcedata <- reactivePoll(
    intervalMillis = 1e7,session = NULL,
    checkFunc <- function() {runif(1)},
    valueFunc <- function() {
      source("www/pull_county_data.R",chdir=T)
      setnames(df,old=c("pop","date","cases","daily"),new=c("Population","Date","cumConfirmed","Confirmed"))
      #get unique states and counties
      counties <- unique(zipcodenames[,county])
      updateSelectizeInput(session,"countyselect", choices= c("All Counties",counties),selected="Baltimore County")
      
      zips = df[,.SD[1], by=zname][,.(zname,zip,name,county)]

      return(list("ctcovid"=df,
                  "counties" = counties,
                  "zips"=zips))
    }
  )
  
  #output$location = renderText({paste0("<b>Maryland - ",county(), " - ", zip(), " (Population: ",unique(localedata()$Population),")</b>")})
  output$location = renderText({paste0("<b>MD - ", county()," - ",paste0(stringr::str_sub(zip(),1,5),collapse=","), "</b>")})
  
  il <- reactive({
    list(
    "county" = input$countyselect,
    "zip" = input$zipselect,
    "outcome"="Confirmed",
    "knot_interval" = input$knot_interval*7,
    "dist" = input$dist,
    # "dailyinc" = input$dailyinc,
    # "abscases7" = input$dailyinc,
    # "includeBurdenInd" = input$includeBurdenInd)
    "dailyinc" = 18,
    "abscases7" = 18,
    "includeBurdenInd" = F)
    
  })
  
  #observe for changes in county, to update the zip sublist
    county <- reactive(input$countyselect)
    zip <- reactive(input$zipselect)
    
   
  observeEvent(county(),{
    updateSelectizeInput(session,"zipselect", choices= c("All Zip Codes", getzips(county(), sourcedata()$zips)))
  })

  localedata <- eventReactive(il(),{
    
    if(il()$county=="") return(NULL)
    #county has changed. We need to get new information
    locale_info <- get_locale_data(il(),sourcedata()$ctcovid)
    return(locale_info)
  })
  

  # output$ctplot <- renderPlot({
  #   if(!is.null(localedata()) && all(is.na(localedata()$Smoothed))==FALSE && all(is.na(localedata()$deriv_trend))==FALSE) {
  #     get_plot(copy(localedata()),outcome = input$outcome, burdenline=il()$includeBurdenInd,
  #              burdenvalues = c(il()$dailyinc, il()$abscases7),input$trans, input$freey)
  #   }
  # })

  output$ctplot <- renderPlotly({
    if(!is.null(localedata()) && all(is.na(localedata()$Smoothed))==FALSE && all(is.na(localedata()$deriv_trend))==FALSE) {
      get_plotly(copy(localedata()),outcome = input$outcome, burdenline=il()$includeBurdenInd,
               burdenvalues = c(il()$dailyinc, il()$abscases7),input$trans, input$freey)
    }
  })
  
    
  output$cttable <- renderDataTable({
    
    locale_data_table <- prepare_table(localedata(), il()$outcome)
    
    DT::datatable(locale_data_table, rownames=F,
                  options=list(dom='tp', pageLength=100))
  })
  
}

shinyApp(ui, server)
