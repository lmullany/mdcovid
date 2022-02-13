library(plotly)


# plot_trend_markers <- function(data, trend_var) {
#   trend_vals = unique(data[[trend_var]])
#   p <- plot_ly(type="scatter",mode="markers") %>% 
#     add_trace(data=data[get(trend_var)==trend_vals[1]], x=~Date, y=~Smoothed, mode="markers", name=trend_vals[1], marker=list(color="red")) %>% 
#     add_trace(data=data[get(trend_var)==trend_vals[2]], x=~Date, y=~Smoothed, mode="markers", name=trend_vals[2], marker=list(color="orange")) %>% 
#     add_trace(data=data[get(trend_var)==trend_vals[3]], x=~Date, y=~Smoothed, mode="markers", name=trend_vals[3], marker=list(color="green")) %>% 
#     add_trace(data=data[Confirmed>=0], x=~Date, y=~Confirmed, name="Observed", mode="markers", marker=list(color="black")) %>% 
#     layout(legend=list(orientation='h'))
#   
#   return(p)
# }
# 
# plot_trend_markers(gginput, "deriv_trend")

plot_trend_line <- function(data, outcome, interpolate=T) {


  outcomename="Cases"
  
  if(outcome=="Rate") {
    outcomename="Rate (per 100 K)"
    outcome="Confirmed"
    data[,`:=`(Smoothed=Smoothed*100000/Population, Confirmed=Confirmed*100000/Population)]
    data[,deriv_trend:=stringr::str_replace(deriv_trend,"Cases","Rate (per 100 K)")]
  }
  
  
  if(is.null(data)) {
    print("no data")
    return(NULL)
  }
  custom_caption="Source: Maryland Department of Health"

  
  data <- data[Date>="2020-03-01" & get(outcome)>=0]
  
  data[, trend_seg:=rleid(deriv_trend)]
  
  if(interpolate) {
    new_rows = interpolate_trend(data)
    data <- rbind(data,new_rows,fill=TRUE)[order(trend_seg,Date)]
  }

  base_colors = list(
    `Rate (per 100 K) Decreasing` = "green",
    `Rate (per 100 K) Increasing (Accelerating)` = "red",
    `Rate (per 100 K) Increasing (Decelerating)` = "orange"
  )
  
  trend_names = unique(data$deriv_trend)
  trend_colors=base_colors[trend_names]
  #trend_colors =list(rep(as.character(NA),times=length(trend_names)))
  #trend_colors = list("red","orange","green")
  #names(trend_colors) = trend_names
  #return(base_colors[trend_names])
  
  legend_on = data[data[, .I[1], by=deriv_trend]$V1,trend_seg]
  #return(legend_on)
  
  p <- plot_ly(type="scatter", mode="lines")
  for(trend_val in seq(1,data[,max(trend_seg)])) {
    
    
    trend_name = unique(data[trend_seg==trend_val, deriv_trend])
    color = trend_colors[[trend_name]]
    p <- p %>%
      add_trace(data=data[trend_seg==trend_val],
                x=~Date,
                y=~Smoothed,
                mode="lines",
                type="scatter",
                name=trend_name,
                #legendgroup = trend_name,
                legendgroup = "Trend",
                legendgrouptitle= "Trend",
                showlegend = trend_val %in% legend_on,
                line=list(color=color, width=4)
                )
  }
  p <- p %>% 
    add_trace(data=data[Confirmed>=0],
              x=~Date,
              y=~Confirmed,
              name="Observed",
              mode="markers",
              marker=list(color="black", opacity=0.3)) %>% 
    layout(
      legend=list(x=.1, y=.75, yref="paper", xref="paper",orientation='h'),
      yaxis=list(title=list(text=outcomename)),
      xaxis=list(title=list(text="")),
      annotations = list(
        x = 1, y = -0.1, text = custom_caption,
        showarrow = F, xref='paper', yref='paper',
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        font=list(size=10, color="black"))
      )
  
  return(p)
}

interpolate_trend <- function(gginput) {
  key_rows = gginput[trend_seg!=shift(trend_seg) | trend_seg!=shift(trend_seg,-1)]
  if(nrow(key_rows)%%2!=0) {
    key_rows <- rbind(
      key_rows[trend_seg!=shift(trend_seg,1) & trend_seg!=shift(trend_seg,-1)],
      key_rows
    )[order(trend_seg,Date)]
  }
  
  new_rows = key_rows[, .(Date = mean(Date), Smoothed = mean(Smoothed)),by = rep(seq(1, nrow(key_rows), 2), each = 2)]
  
  new_rows = new_rows[rep(1:.N,each=2)]
  new_rows[,`:=`(trend_seg=key_rows$trend_seg, Confirmed=NA, deriv_trend=key_rows$deriv_trend, rep=NULL)]
  return(new_rows)
}

# inputs = list(county = "Carroll County", "zip" = NULL, outcome="Confirmed", knot_interval = 21, dist="poisson")
# gginput <- get_locale_data(inputs, df)[]
# 
# gginput[, trend_seg:=rleid(deriv_trend)]
# plot_trend_line(gginput, outcome="Confirmed")
# #interpolate_trend(gginput)[]
