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
  
  # filter drop-down options as explained in this blog post:
  # https://www.davidsolito.com/post/conditional-drop-down-in-shiny/
  # country_choice <- reactive({
  #   available_countries(input$plot_type)
  # })
  # observe(
  #   updateSelectizeInput(session, "country", choices = country_choice())
  # )
  
  output$plot_options <- renderUI({
    if (input$plot_category == 'Summary plot') {
      list(
        selectizeInput('plot_type','Select a plot:',choices=plot_list),
        uiOutput('country_choice'),
        checkboxInput('score','Show summary score?',TRUE),
        sliderInput('shade','Fraction to shade:',0,1,0.5),
        checkboxInput('pred','Show predictions?',FALSE),
        uiOutput('pc_choice'),
        hr(),
        checkboxInput('help_me','Show help text?',FALSE),
        checkboxInput('show_sources','Show sources?',FALSE),
        htmlOutput('how_to')
      )
    } else if (input$plot_category == 'Scatter plot') {
      list(
        selectizeInput('plot_type','Select a plot:',choices=scatter_list),
        selectizeInput('country_list','Highlight countries:',choices=all_countries,
                       multiple=TRUE)
      )
    }
  })
  
  output$country_choice <- renderUI({
    country_choices <- available_countries(input$plot_type)
    selectizeInput('country','Select a country:',choices=country_choices)
  })
  
  output$pc_choice <- renderUI({
    if (input$pred) {
      numericInput('pcs','Number of principal components:',5,min=1,max=30)
    }
  })
  
  output$dotPlot <- renderPlot({
    deca_plot(input$plot_type,
              input$country,
              shade_fraction=input$shade,
              overall_score=ifelse(input$score,'PC1','none'),
              show_pred=input$pred,
              num_pcs=input$pcs,
              show_sources=input$show_sources) +
      theme(axis.text.y = element_text(size=15),
            title=element_text(size=20))
    })
  
  output$scatterPlot <- renderPlot({
    deca_scatter(input$plot_type,input$country_list) +
      theme(title=element_text(size=15))
  })
  
  output$main_plot <- renderUI({
    if (input$plot_category == 'Summary plot') {
      heightstr <- paste0(25*num_rows(input$plot_type,input$country) + 50,'px')
      plotOutput('dotPlot',width='auto',height=heightstr)      
    } else if (input$plot_category == 'Scatter plot') {
      plotOutput('scatterPlot')
    }
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
  output$plot_sources <- renderText({
    # TODO: somehow these still aren't quite right; don't work properly for some plots
    sources <- get_sources(input$plot_type)
    # TODO: turn these into appropriate hyperlinks
    source_tbl <- tibble(short=c("A4AI","DTRI","EIU 3i","EIU Global Microscope","Estonia", 
                                 "Freedom House","GSMA MCI","GSMA MMRI","ITU","OKF", 
                                 "RSF","UNESCO","Universal Postal Union","V-Dem","WB Doing Business", 
                                 "WB Findex","WEF","WEF NRI","WJP","WomanStats"),
                         full=c("Alliance for Affordable Internet<br>",
                                "Digital Trade Restrictiveness Index<br>",
                                "Inclusive Internet Index<br>",
                                "EIU Global Microscope<br>",
                                "Estonia<br>", 
                                "Freedom House<br>",
                                "GSMA Mobile Connectivity Index<br>",
                                "GSMA Mobile Money Regulatory Index<br>",
                                "International Telecommunications Union<br>",
                                "Open Knowledge Foundation<br>", 
                                "Reporters Without Borders<br>",
                                "UNESCO<br>",
                                "Universal Postal Union<br>",
                                "V-Dem<br>",
                                "WB Doing Business<br>", 
                                "WB Findex<br>",
                                "WEF<br>",
                                "WEF Networkod Readiness Index<br>",
                                "World Justice Project<br>",
                                "WomanStats<br>"))
    
    source_str <- filter(source_tbl,short %in% sources)$full %>% paste(collapse='')
    paste0('<b>Sources:</b><br><br>',source_str)
  })
  
})
