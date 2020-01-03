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
  
  output$how_to <- renderText({
    str <- paste0("<b>How to read this plot:</b><br><br>",
                  "<b><i><font color='#BA0C2F'>Red circles</font></i></b>: indicator values for ",input$country,'.<br>',
                  "<b><i><font color='#002F6C'>Translucent blue circles</font></i></b>: indicator values for all other countries.<br>",
                  "<b><i><span style='background-color: #CFCDC9'>Gray shaded region</i></b></span>: indicates the middle ",input$shade*100,"% of countries.<br><br>")
    if (input$score) {
      str <- paste0(str,
                    "<b><i><font color='#BA0C2F'>Vertical red line</font></i></b>: Overall score for ",input$country,
                    " based on the data shown in this plot. The overall score is the ",
                    "first principal component (PC) of the data shown on this plot (with missing values for ",
                    "countries other than ",input$country," filled in by multiple imputation).<br><br>")
    }
    if (input$pred) {
      str <- paste0(str,
                    "<b><i><font color='#BA0C2F'>Short vertical red lines</font></i></b>: Predicted value of indicator based on linear model of first ",
                    input$pcs," principal components.<br><br>",
                    "Red circles are <b><i><font color='#BA0C2F'>filled</font></i></b> if the difference between the model prediction and the actual ",
                    "value is statistically-significant (p < 0.05). This can be taken as a sign that the ",
                    'actual value is "surprising", given other data about the country.<br><br>',
                    "Red circles are <b><i><font color='#BA0C2F'>hollow</font></i></b> if the difference is not statistically-significant. This sometimes ",
                    "happens even for relatively large errors; this is just a sign that the indicator cannot be modeled ",
                    "well based on the first ",input$pcs," principal components.<br><br>")
    }
    if (input$help_me) { str }
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
