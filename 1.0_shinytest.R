install.packages("shinythemes")
install.packages("shinydashboard")
install.packages("DT")

library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
str(month)

twitterdf<- read.csv(file.choose(), header = T, sep="," )


DT
attach(twitterdf)

server = function(input, output, session){
  
  # table for the Data table tab
  
  output$tableDT <- DT::renderDataTable(DT::datatable(twitterdf) %>% 
                                          formatStyle("handle") %>%
                                          formatStyle("tops", color = "grey")
                                                      backgroundColor = "lightblue") 
  
  
  weighted.mydata = reactive(
    cbind(twitterdf, 
          points = input$w1 * `handle` + input$w2 * `month`)
  )
  
  
  output$scat = renderPlot({
    ggplot(weighted.mydata(), aes(rPerformance, ePerformance)) +
      geom_point() + geom_smooth(method = "lm") +
      xlab("Impressions") + ylab("Engagements")
  })
  
  
  
  mydata.new = reactive({
    
    user_brush <- input$user_brush
    mysel <- brushedPoints(weighted.mydata(), user_brush)
    return(mysel)
    
  })
  
  
  output$table = DT::renderDataTable(DT::datatable(mydata.new()))
  
  output$mydownload = downloadHandler(
    filename = "twitterselection.csv",
    content = function(file) {
      write.csv(mydata.new(), file)})
  
  
}

ui = navbarPage(theme = shinytheme("sandstone"), title = h3("Twitter Handle Performance"),
                tabPanel( ("Audience Interaction"),
                  wellPanel(
                    sliderTextInput(inputId = "Handle","Select Handle:",
                                choices = c("PewTrusts" , "PewStates" , "PewHealth", "PewEU" , "PewEnvironment" , "FixOurParks" ), "Twitter Handle",
                                Selected= c("PewTrusts" , "PewStates" , "PewHealth", "PewEU" , "PewEnvironment" , "FixOurParks" ),
                                animate = FALSE, grid = FALSE
                                hide_min_max = FALSE, from_fixed = FALSE,
                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                post = NULL, dragRange = TRUE
                  ),
                  plotOutput("scat", brush = "user_brush"),
                  DT::dataTableOutput("table"),
                  downloadButton(outputId = "mydownload", label = "Download Table")
                ),
                
                tabPanel("Documentation",
                         h4("Video documentation - Embedded from Youtube"),
                         tags$iframe(style="height:700px; width:100%",
                                     src="https://www.youtube.com/embed/vySGuusQI3Y")
                ), 
                
                tabPanel("Data Table with the underlying Data",
                         DT::dataTableOutput("tableDT"))
                
)

shinyApp(ui = ui, server = server