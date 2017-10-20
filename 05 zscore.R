output$zplot <- renderPlotly({
  
  a <- list(
    x = c(2001, 2006, 2009),
    y = zdata$zlist[c(4, 9, 12)],
    text = c("Early 2000s Recession", "United States Housing Bubble", "Global Financial Crisis"), 
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
  
  plot_ly() %>%
    add_lines(x = ~1998:2016, y = ~round(zdata$zlist, 4), type = 'scatter', mode = 'lines', line = list(shape = "spline", color = "#16a6b6")) %>%
    layout(xaxis = list(title = "",
                        dtick = 2),
           yaxis = list(title = "",
                        showgrid = FALSE),
           margin = list(r = 30),
           annotations = a,
           hovermode = "x") %>% 
    config(displayModeBar = F)
})

output$ztable <- renderDataTable({
  
  data = data.frame("Year" = 1998:2016, "Zscore" = round(zdata$zlist, 4))
  
  datatable(data, 
            rownames = FALSE, 
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollY = '460px',
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
})

output$rho <- renderValueBox({
  valueBox(
    color = "red",
    
    icon = icon("flag"),
    
    subtitle = "Optimal Rho",
    
    value = zdata$rho
  )
})