output$macro_desc <- renderGvis({
  # datatable(macro_desc, 
  #           rownames = FALSE,
  #           options = list(searching = FALSE,
  #                          paging = FALSE,
  #                          ordering = FALSE,
  #                          dom = 't'))
  
  gvisTable(macro_desc, options = list(width = "100%"))
})

output$cor_tb <- renderDataTable({
  data = cbind(zdata$zlist, macro$x[,-1]); colnames(data)[1] = "Z Score"
  corr = as.data.frame(t(cor(data)[1,-1])); rownames(corr) = "Correlation/Z"
  
  datatable(corr,
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollX = TRUE,
                           dom = 't')) %>% formatRound(colnames(corr))
})

output$cor_z <- renderPlotly({
  
  data = cbind(zdata$zlist, macro$x[,-1]); colnames(data)[1] = "Z Score"
  corr = matrix(cor(data)[1,-1], nrow = 1, byrow = TRUE)
  
  plot_ly(x = colnames(data)[-1], y = "Z Score", z = round(corr, 4), type = "heatmap", showscale = FALSE, hoverinfo = round(corr, 2)) %>%
    layout(xaxis = list(title = "", showticklabels = FALSE),
           yaxis = list(side = "right"),
           margin = list(r = 130, t = 0, l = 0, b = 10)) %>% 
    config(displayModeBar = F)
})

output$cor_plot <- renderPlotly({
  
  corr = cor(macro$x[,-1])
  
  plot_ly(x = colnames(corr), y = rev(rownames(corr)), z = round(corr[rev(colnames(corr)),rownames(corr)], 4), type = "heatmap", showscale = FALSE) %>%
    layout(xaxis = list(side = "bottom"),
           yaxis = list(side = "right"),
           margin = list(r = 130, t = 0, l = 0, b = 130)) %>% 
    config(displayModeBar = F)
})