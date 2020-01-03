#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {

  output$dotPlot <- renderPlot({
    output$pc_choice <- renderUI({
      if (input$pred) {
        numericInput('pcs','Number of principal components:',5,min=1,max=30)
      }
     })
    deca_plot(input$plot_type,
              input$country,
              shade_fraction=input$shade,
              overall_score=ifelse(input$score,'PC1','none'),
              show_pred=input$pred,
              num_pcs=input$pcs) +
      theme(axis.text.y = element_text(size=15),
            title=element_text(size=20))
    })
  
  output$scaledPlot <- renderUI({
    heightstr <- paste0(25*num_rows(input$plot_type,input$country) + 50,'px')
    plotOutput('dotPlot',width='auto',height=heightstr)
  })
  
  # filter drop-down options as explained in this blog post:
  # https://www.davidsolito.com/post/conditional-drop-down-in-shiny/
  country_choice <- reactive({
    available_countries(input$plot_type)
  })
  observe(
    updateSelectizeInput(session, "country", choices = country_choice())
  )
})
