function(input, output) {
  
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  
  output$table <- renderTable({
    head(cars, 4)
  })
   
  #render table PETERSON navtabb
  output$contents <- renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })  
  
}
