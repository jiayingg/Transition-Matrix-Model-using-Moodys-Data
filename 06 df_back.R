output$backtest <- renderPlotly({
  
  plot_ly(data = df_back, x=~Year) %>%
    add_trace(y=~round(Default*100, 2), name = "Actual", type = "scatter", marker = list(color = "#16a6b6")) %>%
    add_lines(y=~round(BackTest*100, 2), name = "BackTesting", type = "scatter", mode = "lines", line = list(shape = "spline", color = "#ee6557")) %>%
    layout(xaxis = list(title = "",
                        dtick = 2),
           yaxis = list(title = "",
                        ticksuffix = "%",
                        showgrid = FALSE),
           legend = list(orientation = "h", x = 0.35, y = -0.1),
           margin = list(r = 30),
           hovermode = "x") %>% 
    config(displayModeBar = F)
})

output$dfback_tb <- renderDataTable({
  
  datatable(df_back,
            colnames = c("Year", "Actual", "BackTesting"),
            rownames = FALSE,
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollY = '460px',
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
    formatPercentage(2:3, digits = 4)
})

output$rho2 <- renderValueBox({
  valueBox(
    color = "red",
    
    icon = icon("flag"),
    
    subtitle = "Optimal Rho",
    
    value = zdata$rho
  )
})