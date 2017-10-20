output$zmacrotable <- renderDataTable({
  
  data = data.frame("Year" = 1998:2016, "Zscore" = round(zdata$zlist, 3), "Macro" = round(macro$x[, input$macro], 3))
  
  datatable(data, 
            rownames = FALSE, 
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollY = '460px',
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
})

output$zmacro <- renderPlotly({
  
  plot_ly(x = ~1998:2016) %>%
    add_lines(y = ~round(zdata$zlist,4), name = "ZScore", line = list(shape = "spline", color = "#16a6b6")) %>%
    add_lines(y = ~round(macro$x[, input$macro],4), name = "Macro", yaxis = "y2", line = list(shape = "spline", color = "#ee6557")) %>%
    layout(
      yaxis  = list(tickfont = list(color = "#16a6b6"), titlefont = list(color = "#16a6b6"), showgrid = FALSE, title = "Z Score"), 
      yaxis2 = list(tickfont = list(color = "#ee6557"), titlefont = list(color = "#ee6557"), showgrid = FALSE, title = "Macro", overlaying = "y", side = "right", zeroline = FALSE),
      xaxis = list(title = "", ticks = "", dtick = 2),
      hovermode = "x",
      legend = list(orientation = "h", x = 0.4, y = -0.1),
      margin = list(r = 50)) %>% 
    config(displayModeBar = F)
})