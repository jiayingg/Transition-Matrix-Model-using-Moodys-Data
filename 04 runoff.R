dataInput_run <- reactive({
  
  startingcount = loan %>%
    group_by(Year) %>% summarize(Aaa=sum(Aaa_cnt), Aa=sum(Aa_cnt), A=sum(A_cnt), 
                                 Baa=sum(Baa_cnt), Ba=sum(Ba_cnt), B=sum(B_cnt), 
                                 Caa.C=sum(Caa.C_cnt)) %>% 
    melt(id = "Year") %>% arrange(Year)
  new_enter = cbind(startingcount, c(loan[-(1:7),3], rep(NA, 7)))
  new_enter$Year = new_enter$Year + 1
  colnames(new_enter) = c("Year", "Rating", "LastYear", "ThisYear")
  
  new_enter$NetNewEnter = new_enter$ThisYear - new_enter$LastYear
  new_enter$NewRate = new_enter$NetNewEnter/new_enter$LastYear
  
  new_enter$Rating = as.character(new_enter$Rating)
  new_enter$LastYear = round(new_enter$LastYear, 0)
  new_enter$NetNewEnter = round(new_enter$NetNewEnter, 0)
  new_enter = filter(new_enter, Year <= 2016)
  
  return(new_enter %>% filter(Rating == input$run_rating) %>% select(-2))
})

output$run_off <- renderPlotly({
  data = dataInput_run()
  color_pos_neg = ifelse(data$NetNewEnter>=0, "#16a6b6", "#ee6557")
  
  plot_ly(data) %>%
    add_trace(x = ~Year, y = ~round(100*data$NewRate, 2), type = "bar", name = "Net New Loan", marker = list(color = color_pos_neg)) %>%
    add_trace(x = ~Year, y = ~round(100*data$NewRate, 2), type = "scatter", mode = "lines", line = list(shape = "spline", color = "black"), name = "New Loan Rate", yaxis = "y2") %>%
    layout(legend = list(orientation = "h", x = 0.25, y = -0.1),
           margin = list(r = 80), hovermode = "x", 
           xaxis = list(title = "", dtick = 2),
           yaxis  = list(side = "left", title = "Net New Loan", showgrid = FALSE, zeroline = FALSE),
           yaxis2 = list(side = "right", title = "New Loan Rate", showgrid = FALSE, zeroline = FALSE, overlaying = "y", ticksuffix = "%")) %>% 
    config(displayModeBar = F)
})

output$run_tb <- renderDataTable({
  data = dataInput_run()
  rownames(data) = data$Year
  datatable(data[,-1],
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
    formatPercentage(4, digits = 2) %>%
    formatStyle(0, fontWeight = "bold") %>%
    formatStyle(1:4, 3, color = styleInterval(0, c("#ee6557", "#16a6b6")))
})