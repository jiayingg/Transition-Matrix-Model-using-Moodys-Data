m = unique(z_final$Model)

output$tmm1_plot <- renderPlotly({ plot_z(z_final[z_final$Model == m[1], 1:3], "Z Score") })
output$tmm2_plot <- renderPlotly({ plot_z(z_final[z_final$Model == m[2], 1:3], "Z Score") })
output$tmm3_plot <- renderPlotly({ plot_z(z_final[z_final$Model == m[3], 1:3], "Z Score") })
output$tmm4_plot <- renderPlotly({ plot_z(z_final[z_final$Model == m[4], 1:3], "Z Score") })

output$df1_plot <- renderPlotly({ plot_z(df_final[df_final$Model == m[1], 1:3], "Default Rate") })
output$df2_plot <- renderPlotly({ plot_z(df_final[df_final$Model == m[2], 1:3], "Default Rate") })
output$df3_plot <- renderPlotly({ plot_z(df_final[df_final$Model == m[3], 1:3], "Default Rate") })
output$df4_plot <- renderPlotly({ plot_z(df_final[df_final$Model == m[4], 1:3], "Default Rate") })

output$tmm1_fore <- renderDataTable({ fore_z(z_final[z_final$Model == m[1],], "Transition Matrix - Z Score") })
output$tmm2_fore <- renderDataTable({ fore_z(z_final[z_final$Model == m[2],], "Transition Matrix - Z Score") })
output$tmm3_fore <- renderDataTable({ fore_z(z_final[z_final$Model == m[3],], "Transition Matrix - Z Score") })
output$tmm4_fore <- renderDataTable({ fore_z(z_final[z_final$Model == m[4],], "Transition Matrix - Z Score") })

output$df1_fore <- renderDataTable({ fore_z(df_final[df_final$Model == m[1],], "Default Rate Prediction") })
output$df2_fore <- renderDataTable({ fore_z(df_final[df_final$Model == m[2],], "Default Rate Prediction") })
output$df3_fore <- renderDataTable({ fore_z(df_final[df_final$Model == m[3],], "Default Rate Prediction") })
output$df4_fore <- renderDataTable({ fore_z(df_final[df_final$Model == m[4],], "Default Rate Prediction") })

output$equa1 <- renderUI({ withMathJax(h4(v_(loose_2v[1,], 2)$equation)) })
output$equa2 <- renderUI({ withMathJax(h5(v_(loose_3v[1,], 3)$equation)) })
output$equa3 <- renderUI({ withMathJax(h4(v_(tight_2v[1,], 2)$equation)) })
output$equa4 <- renderUI({ withMathJax(h5(v_(tight_3v[1,], 3)$equation)) })

output$adjr1 <- renderUI({ withMathJax(h4(v_(loose_2v[1,], 2)$adjr)) })
output$adjr2 <- renderUI({ withMathJax(h5(v_(loose_3v[1,], 3)$adjr)) })
output$adjr3 <- renderUI({ withMathJax(h4(v_(tight_2v[1,], 2)$adjr)) })
output$adjr4 <- renderUI({ withMathJax(h5(v_(tight_3v[1,], 3)$adjr)) })

output$tmm1_migrate <- renderDataTable({
  if(length(input$tmm1_fore_cells_selected) == 0) return()
  if(input$tmm1_fore_cells_selected[2] == 0) return()
  mig.mtx(z_final, input$tmm1_fore_cells_selected, m[1])
})

output$tmm2_migrate <- renderDataTable({
  if(length(input$tmm2_fore_cells_selected) == 0) return()
  if(input$tmm2_fore_cells_selected[2] == 0) return()
  mig.mtx(z_final, input$tmm2_fore_cells_selected, m[2])
})

output$tmm3_migrate <- renderDataTable({
  if(length(input$tmm3_fore_cells_selected) == 0) return()
  if(input$tmm3_fore_cells_selected[2] == 0) return()
  mig.mtx(z_final, input$tmm3_fore_cells_selected, m[3])
})

output$tmm4_migrate <- renderDataTable({
  if(length(input$tmm4_fore_cells_selected) == 0) return()
  if(input$tmm4_fore_cells_selected[2] == 0) return()
  mig.mtx(z_final, input$tmm4_fore_cells_selected, m[4])
})