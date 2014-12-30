shinyUI(pageWithSidebar(
  headerPanel("Trends in ATP Service Speeds"),
  sidebarPanel(
    selectInput("dataset", "Choose a Serve Metric:", 
                choices = c("Fastest", "First", "Second", "Difference (First - Second)"))
  ),
  mainPanel(
   tabsetPanel(
      tabPanel("By Grand Slam", htmlOutput("view")), 
      tabPanel("By Player", htmlOutput("viewplot"))
    )
  )
 )
)