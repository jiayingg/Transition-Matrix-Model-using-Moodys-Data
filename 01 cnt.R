dataInput_cnt <- reactive({
  
  start = acast(loan[,1:3], Year ~ Rating, value.var = "Cnt")
  end = loan %>% 
    group_by(Year) %>% 
    summarize(Default = sum(Default_cnt), Caa.C = sum(Caa.C_cnt),
              B = sum(B_cnt), Ba = sum(Ba_cnt), Baa = sum(Baa_cnt), 
              A = sum(A_cnt), Aa = sum(Aa_cnt), Aaa = sum(Aaa_cnt)) %>%
    select(-1) %>% as.matrix() %>% round(0)
  rownames(end) = rownames(start)
  
  return(list("start" = start, "end" = end))
})

output$start1 <- renderPlotly({
  
  start = dataInput_cnt()$start
  cum_start = data.frame(t(apply(start, 1, cumsum)))
  color_plt = viridis_pal(option = "D")(9)
  
  plot_ly(x = rownames(cum_start)) %>%
    add_lines(y = cum_start[,7], name = colnames(cum_start)[7] ,line = list(shape = "spline", color = color_plt[1]), fill = "tozeroy", fillcolor = color_plt[3], hovertext = start[,7]) %>%
    add_lines(y = cum_start[,6], name = colnames(cum_start)[6] ,line = list(shape = "spline", color = color_plt[2]), fill = "tozeroy", fillcolor = color_plt[4], hovertext = start[,6]) %>%
    add_lines(y = cum_start[,5], name = colnames(cum_start)[5] ,line = list(shape = "spline", color = color_plt[3]), fill = "tozeroy", fillcolor = color_plt[5], hovertext = start[,5]) %>%
    add_lines(y = cum_start[,4], name = colnames(cum_start)[4] ,line = list(shape = "spline", color = color_plt[4]), fill = "tozeroy", fillcolor = color_plt[6], hovertext = start[,4]) %>%
    add_lines(y = cum_start[,3], name = colnames(cum_start)[3] ,line = list(shape = "spline", color = color_plt[5]), fill = "tozeroy", fillcolor = color_plt[7], hovertext = start[,3]) %>%
    add_lines(y = cum_start[,2], name = colnames(cum_start)[2] ,line = list(shape = "spline", color = color_plt[6]), fill = "tozeroy", fillcolor = color_plt[8], hovertext = start[,2]) %>%
    add_lines(y = cum_start[,1], name = colnames(cum_start)[1], line = list(shape = "spline", color = color_plt[7]), fill = "tozeroy", fillcolor = color_plt[9]) %>%
    layout(xaxis = list(title = "",
                        dtick = 2,
                        showgrid = FALSE),
           yaxis = list(title = "Loan Counts",
                        showgrid = FALSE),
           hovermode = "x") %>% 
    config(displayModeBar = F)
})

output$start2 <- renderPlotly({
  
  start_prop = prop.table(dataInput_cnt()$start, 1)
  cum_start_prop = round(t(apply(start_prop, 1, cumsum))*100, 2)
  color_plt = viridis_pal(option = "D")(9)
  
  plot_ly(x = rownames(cum_start_prop)) %>%
    add_lines(y = cum_start_prop[,7], name = colnames(cum_start_prop)[7] ,line = list(shape = "spline", color = color_plt[1]), fill = "tozeroy", fillcolor = color_plt[3], hovertext = percent(start_prop[,7])) %>%
    add_lines(y = cum_start_prop[,6], name = colnames(cum_start_prop)[6] ,line = list(shape = "spline", color = color_plt[2]), fill = "tozeroy", fillcolor = color_plt[4], hovertext = percent(start_prop[,6])) %>%
    add_lines(y = cum_start_prop[,5], name = colnames(cum_start_prop)[5] ,line = list(shape = "spline", color = color_plt[3]), fill = "tozeroy", fillcolor = color_plt[5], hovertext = percent(start_prop[,5])) %>%
    add_lines(y = cum_start_prop[,4], name = colnames(cum_start_prop)[4] ,line = list(shape = "spline", color = color_plt[4]), fill = "tozeroy", fillcolor = color_plt[6], hovertext = percent(start_prop[,4])) %>%
    add_lines(y = cum_start_prop[,3], name = colnames(cum_start_prop)[3] ,line = list(shape = "spline", color = color_plt[5]), fill = "tozeroy", fillcolor = color_plt[7], hovertext = percent(start_prop[,3])) %>%
    add_lines(y = cum_start_prop[,2], name = colnames(cum_start_prop)[2] ,line = list(shape = "spline", color = color_plt[6]), fill = "tozeroy", fillcolor = color_plt[8], hovertext = percent(start_prop[,2])) %>%
    add_lines(y = cum_start_prop[,1], name = colnames(cum_start_prop)[1], line = list(shape = "spline", color = color_plt[7]), fill = "tozeroy", fillcolor = color_plt[9]) %>%
    layout(xaxis = list(title = "",
                        dtick = 2,
                        showgrid = FALSE),
           yaxis = list(title = "Proportion from the Loan Counts",
                        showgrid = FALSE,
                        ticksuffix = "%"),
           hovermode = "x") %>% 
    config(displayModeBar = F)
})

output$end1 <- renderPlotly({
  
  end = dataInput_cnt()$end
  cum_end = data.frame(t(apply(end, 1, cumsum)))
  color_plt = viridis_pal(option = "C")(10)
  
  plot_ly(x = rownames(cum_end)) %>%
    add_lines(y = cum_end[,8], name = colnames(cum_end)[8] ,line = list(shape = "spline", color = color_plt[1]), fill = "tozeroy", fillcolor = color_plt[3], hovertext = end[,8]) %>%
    add_lines(y = cum_end[,7], name = colnames(cum_end)[7] ,line = list(shape = "spline", color = color_plt[2]), fill = "tozeroy", fillcolor = color_plt[4], hovertext = end[,7]) %>%
    add_lines(y = cum_end[,6], name = colnames(cum_end)[6] ,line = list(shape = "spline", color = color_plt[3]), fill = "tozeroy", fillcolor = color_plt[5], hovertext = end[,6]) %>%
    add_lines(y = cum_end[,5], name = colnames(cum_end)[5] ,line = list(shape = "spline", color = color_plt[4]), fill = "tozeroy", fillcolor = color_plt[6], hovertext = end[,5]) %>%
    add_lines(y = cum_end[,4], name = colnames(cum_end)[4] ,line = list(shape = "spline", color = color_plt[5]), fill = "tozeroy", fillcolor = color_plt[7], hovertext = end[,4]) %>%
    add_lines(y = cum_end[,3], name = colnames(cum_end)[3] ,line = list(shape = "spline", color = color_plt[6]), fill = "tozeroy", fillcolor = color_plt[8], hovertext = end[,3]) %>%
    add_lines(y = cum_end[,2], name = colnames(cum_end)[2] ,line = list(shape = "spline", color = color_plt[7]), fill = "tozeroy", fillcolor = color_plt[9], hovertext = end[,2]) %>%
    add_lines(y = cum_end[,1], name = colnames(cum_end)[1], line = list(shape = "spline", color = color_plt[8]), fill = "tozeroy", fillcolor = color_plt[10]) %>%
    layout(xaxis = list(title = "",
                        dtick = 2,
                        showgrid = FALSE),
           yaxis = list(title = "Loan Counts",
                        showgrid = FALSE),
           hovermode = "x") %>% 
    config(displayModeBar = F)
})

output$end2 <- renderPlotly({
  
  end_prop = prop.table(dataInput_cnt()$end, 1)
  cum_end_prop = round(t(apply(end_prop, 1, cumsum))*100, 2)
  color_plt = viridis_pal(option = "C")(10)
  
  plot_ly(x = rownames(cum_end_prop)) %>%
    add_lines(y = cum_end_prop[,8], name = colnames(cum_end_prop)[8] ,line = list(shape = "spline", color = color_plt[1]), fill = "tozeroy", fillcolor = color_plt[3], hovertext = percent(end_prop[,8])) %>%
    add_lines(y = cum_end_prop[,7], name = colnames(cum_end_prop)[7] ,line = list(shape = "spline", color = color_plt[2]), fill = "tozeroy", fillcolor = color_plt[4], hovertext = percent(end_prop[,7])) %>%
    add_lines(y = cum_end_prop[,6], name = colnames(cum_end_prop)[6] ,line = list(shape = "spline", color = color_plt[3]), fill = "tozeroy", fillcolor = color_plt[5], hovertext = percent(end_prop[,6])) %>%
    add_lines(y = cum_end_prop[,5], name = colnames(cum_end_prop)[5] ,line = list(shape = "spline", color = color_plt[4]), fill = "tozeroy", fillcolor = color_plt[6], hovertext = percent(end_prop[,5])) %>%
    add_lines(y = cum_end_prop[,4], name = colnames(cum_end_prop)[4] ,line = list(shape = "spline", color = color_plt[5]), fill = "tozeroy", fillcolor = color_plt[7], hovertext = percent(end_prop[,4])) %>%
    add_lines(y = cum_end_prop[,3], name = colnames(cum_end_prop)[3] ,line = list(shape = "spline", color = color_plt[6]), fill = "tozeroy", fillcolor = color_plt[8], hovertext = percent(end_prop[,3])) %>%
    add_lines(y = cum_end_prop[,2], name = colnames(cum_end_prop)[2] ,line = list(shape = "spline", color = color_plt[7]), fill = "tozeroy", fillcolor = color_plt[9], hovertext = percent(end_prop[,2])) %>%
    add_lines(y = cum_end_prop[,1], name = colnames(cum_end_prop)[1], line = list(shape = "spline", color = color_plt[8]), fill = "tozeroy", fillcolor = color_plt[10]) %>%
    layout(xaxis = list(title = "",
                        dtick = 2,
                        showgrid = FALSE),
           yaxis = list(title = "Proportion from the Loan Counts",
                        showgrid = FALSE,
                        ticksuffix = "%"),
           hovermode = "x") %>% 
    config(displayModeBar = F)
})