output$df_rt <- renderPlotly({
  df = loan[,c(1,2,11)]
  plot_ly(df) %>% add_lines(x = ~ Year, 
                            y = ~ Default * 100, 
                            line = list(shape = "spline"),
                            color = ~ Rating,
                            colors = viridis_pal(direction = -1)(7)) %>%
    layout(xaxis = list(title = "",
                        dtick = 2),
           yaxis = list(title = "",
                        ticksuffix = "%",
                        showgrid = FALSE),
           hovermode = "x") %>% 
    config(displayModeBar = F)
})

output$df_tb <- renderDataTable({
  df = acast(loan[, c(1,2,11)], Year ~ Rating, value.var = "Default")
  datatable(df, 
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
    formatPercentage(1:7, digits = 2) %>%
    formatStyle(0, fontWeight = "bold") %>%
    formatStyle(1:7, color = styleEqual(c(0), c("lightgray")))
})