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

plot_trend_line <- function(data, interpolate=T) {
  
  data[, trend_seg:=rleid(deriv_trend)]
  
  if(interpolate) {
    new_rows = interpolate_trend(data)
    data <- rbind(data,new_rows,fill=TRUE)[order(trend_seg,Date)]
  }
  print(data[1:6])
  
  trend_names = unique(data[["deriv_trend"]])
  trend_colors = list("red","orange","green")
  names(trend_colors) = trend_names
  
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
    layout(legend=list(x=.1, y=.75, yref="paper", xref="paper",orientation='h'))
  
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

inputs = list(county = "Carroll County", "zip" = NULL, outcome="Confirmed", knot_interval = 21, dist="poisson")
gginput <- get_locale_data(inputs, df)[]

gginput[, trend_seg:=rleid(deriv_trend)]
plot_trend_line(gginput)
#interpolate_trend(gginput)[]
