output$test_desc <- renderGvis({
  gvisTable(test_desc)
})

output$sign_desc <- renderGvis({
  gvisTable(sign_desc)
})

output$final_tb <- renderGvis({
  gvisTable(t(tight_2v))
})