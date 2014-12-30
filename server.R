shinyServer(function(input, output) {
  
  plotInput <- reactive({
    dataset <- switch(input$dataset,
           "Fastest" = serves[,c("Fastest","Year","Title")],
           "First" = serves[,c("AvgFirst","Year","Title")],
           "Second" = serves[,c("AvgSecond","Year","Title")],
           "Difference (First - Second)" = serves[,c("Difference","Year","Title")])
           
    names(dataset)[1] <- "Measure"
    
    dataset
  })
  
    output$view <- renderGvis({
  	
  	usopen <- subset(plotInput(), Title == "US Open")
  	
  	ylimits <- quantile(usopen$Measure, c(.25, .75), na.rm = T)
  	vaxis.text <- '{minValue: LOW, maxValue: UPPER, title: "Median (mph)"}'
  	vaxis.text <- sub("LOW",ylimits[1],vaxis.text)
  	vaxis.text <- sub("UPPER",ylimits[2],vaxis.text)
  	
  	usopen <- ddply(usopen, "Year", summarize,
  		Measure = round(median(Measure, na.rm = TRUE), 1)
  	)
 
  	wimbledon <- subset(plotInput(), Title == "Wimbledon")
  	wimbledon <- ddply(wimbledon, "Year", summarize,
  		Measure = round(median(Measure, na.rm = TRUE), 1)
  	)

  	french <- subset(plotInput(), Title == "Roland Garros")
  	french <- ddply(french, "Year", summarize,
  		Measure = round(median(Measure, na.rm = TRUE), 1)
  	)

  	aussie <- subset(plotInput(), Title == "Australian Open")
  	aussie <- ddply(aussie, "Year", summarize,
  		Measure = round(median(Measure, na.rm = TRUE), 1)
  	)


    p1 <- gvisScatterChart(usopen[,c("Year", "Measure")], 
    		options = list(curveType = 'function', lineWidth = 2, 
    		hAxis = '{format: "####"}',
    		vAxis= vaxis.text, pointSize= 10,
    		legend = 'none', title = "US Open", colors="['#3399CC']"))
    		
    p2 <- gvisScatterChart(wimbledon[,c("Year", "Measure")], 
    		options = list(curveType = 'function', lineWidth = 2, 
    		hAxis = '{format: "####"}',
    		vAxis= vaxis.text, pointSize= 10,
    		legend = 'none', title = "Wimbledon", colors="['#009966']"))
    		
    p3 <- gvisScatterChart(french[,c("Year", "Measure")], 
    		options = list(curveType = 'function', lineWidth = 2, 
    		hAxis = '{format: "####"}',
    		vAxis= vaxis.text, pointSize= 10,
    		legend = 'none', title = "Roland Garros", colors="['#CC99FF']"))   
  
    p4 <- gvisScatterChart(aussie[,c("Year", "Measure")], 
    		options = list(curveType = 'function', lineWidth = 2, 
    		hAxis = '{format: "####"}',
    		vAxis= vaxis.text, pointSize= 10,
    		legend = 'none', title = "Australian Open", colors="['#FF6666']"))
    		 
   gvisMerge(gvisMerge(p1, p2), gvisMerge(p3, p4),  horizontal = TRUE)
    
  })
  
  
  output$viewplot <- renderGvis({
    gvisBubbleChart(top_servers, idvar = "Player", xvar = "Difference", yvar = "First", sizevar = "Recorded Serves",
    	options = list(height = 800, width = 900, bubble="{textStyle:{color: 'none'}}",
    	sizeAxis = '{minValue: 0,  maxSize: 12}', 
    	title = "First Serve Speed Versus Difference Between First and Second Serve Speed", 
    	hAxis='{minValue: 0, maxValue: 35, title: "Difference, First - Second (mph)"}',
    	vAxis='{minValue: 95, maxValue: 135, title: "Average First Serve (mph)"}'))
  })
  
})