function(input, output) {

  ###Standard output
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  
  ###Standard output
  output$table <- renderTable({
    head(cars, 4)
  })
  
  ###Render table navtab PETERSON
  output$contents <- renderDT({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })  
  
  ###Render Map navtab PETERSON
  ###Define the color pallate for the magnitidue
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = v_data1$mag)
  
  ###Categorize depth
  v_data1$depth_type <- ifelse(v_data1$depth <= 70, "shallow", ifelse(v_data1$depth <= 300 | v_data1$depth >70, "intermediate", ifelse(v_data1$depth > 300, "deep", "other")))
  
  ###Map
  output$mymap <- renderLeaflet({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    v_data1 <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
    
    leaflet(v_data1) %>% 
      #setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = v_data1, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(mag)*25000, popup = ~as.character(mag), label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), color = ~pal(mag), fillOpacity = 0.5)   
  })
  
}
