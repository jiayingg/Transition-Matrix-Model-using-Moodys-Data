dataInput <- reactive({
  rownames(loan) = paste(loan$Year, loan$Rating)
  count_prop = loan[, 4:11]
  count = count_prop*loan[,3]
  count_avg = acast(melt(cbind(loan$Rating, count), 
                         value.name = "Rating", 
                         id.vars = "loan$Rating"), 
                    loan$Rating ~ variable, 
                    value.var = "Rating",
                    drop = FALSE, 
                    fill = 0, 
                    fun.aggregate = sum)[c(3,2,1,6,5,4,7),]
  count_avg_prop = prop.table(count_avg, 1)
  
  final = list("count" = count, 
               "count_prop" = count_prop, 
               "count_avg" = count_avg, 
               "count_avg_prop" = count_avg_prop)
  
  return(final)
})

output$migrate <- renderPlotly({
  
  Year = 1998:2016
  list = data.frame(Year = Year)
  start = c("Aaa", "Aa", "A", "Baa", "Ba", "B", "Caa.C")
  end = c("Aaa", "Aa", "A", "Baa", "Ba", "B", "Caa.C", "DF")
  
  withProgress(message = "Please Wait..", {
    for(i in 1:7)
    {
      RName = start[i]
      for(j in 1:8)
      {
        CName = end[j]
        list = cbind(list, dataInput()$count_prop[seq(i,nrow(dataInput()$count_prop),7),j])
        colnames(list)[length(colnames(list))] = paste(RName,"_",CName,sep = "")
      }
    }
    incProgress(1/2, detail = "Generating Plots")
    
    list_m = melt(list, id = "Year")
    
    p = list_m %>%
      group_by(variable) %>%
      do(cnt = one_plot(.)) %>%
      subplot(nrows = 7,shareX = TRUE, shareY = TRUE) %>%
      layout(
        showlegend = FALSE,
        hovermode = "x") %>% 
      config(displayModeBar = F)
    
    incProgress(1/2, detail = "Generating Plots")
  })
  
  return(p)
})