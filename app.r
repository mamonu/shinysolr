
library(solrium)
library(shiny)
library(rCharts)


solr_connect("localhost:8080/solr/select", errors = "complete", verbose = TRUE)

ui <- basicPage(
  textInput("text", "Description:", "Bread"),
 
  #submitButton("Submit"),
  plotOutput("myChart", click = "plot_click"),
  #showOutput("myChart","polycharts"),
  verbatimTextOutput("info")

  
  
  
  
  
  
  )

server <- function(input, output) {
  output$myChart <- renderPlot({
    
    
    solrdf<- solr_search(q=paste0('CodeDesc:',str_replace_all(input$text,"([[:punct:]])|\\s+","+")), rows=10000,wt='csv' )
    
    solrdf <- na.omit(solrdf)
    
    solrdf$mlQuantity <-  ifelse (solrdf$Units=="Litres",1000*solrdf$Quantity,solrdf$Quantity) 
    
    fit <- lm (solrdf$Paid1~ solrdf$mlQuantity)
    
    
    plot(solrdf$mlQuantity, solrdf$Paid1, pch = 16, cex = 1.3, col = "blue", main = "Weight PLOTTED AGAINST Price", xlab = "Weight (ml)", ylab = "Price (pence)")
    abline(lm (solrdf$Paid1~ solrdf$mlQuantity))
#     d1<- dPlot(
#       solrdf$Paid1~ solrdf$mlQuantity,
#      
#       data = solrdf,
#       type = "bubble",height=800,
#       width=1000,
#       bounds = list(x=60, y=25, width=400, height=350)
#     )
#     d1$xAxis( type = "addMeasureAxis" )
#     d1$yAxis( type = "addMeasureAxis" )
#     d1$legend(
#       x = 465,
#       y = 0,
#       width = 50,
#       height = 200,
#       horizontalAlign = "left"
#     )
#     
#     d1$addParams(dom = 'myChart')
#     
#     return(d1)
    
    
      })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })

  
  
  
  
  
  }

shinyApp(ui, server)






