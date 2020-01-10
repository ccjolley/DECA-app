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
        uiOutput('pred_choice'),
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
  
  output$pred_choice <- renderUI({
    if (input$country %in% all_pcs$country) {
      res[1] <- checkboxInput('pred','Show predictions?',FALSE)
    }
  })
  
  output$pc_choice <- renderUI({
    if (input$pred) {
      sliderInput('pcs','Number of principal components:',1,10,5,round=TRUE)
    }
  })
  
  cached_score <- reactive({
    cache_pc1(input$plot_type,input$country)
  })
  
  output$dotPlot <- renderPlot({
    # TODO: now that I can pass a double into the overall_score argument of j2sr_plot, I can 
    # cache the value of that calculation and only update it when input$plot_type or input$country changes
    deca_plot(input$plot_type,
              input$country,
              shade_fraction=input$shade,
              overall_score=ifelse(input$score,cached_score(),'none'),
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
    sources <- get_sources(input$plot_type,input$country)
    source_tbl <- tibble(short=c("A4AI","DTRI","EIU 3i","EIU Global Microscope","Estonia", 
                                 "Freedom House","GSMA MCI","GSMA MMRI","ITU","OKF", 
                                 "RSF","UNESCO","Universal Postal Union","V-Dem","WB Doing Business", 
                                 "WB Findex","WEF","WEF NRI","WJP","WomanStats"),
                         full=c('<a href="https://a4ai.org/affordability-report/data/?_year=2019&indicator=INDEX">Alliance for Affordable Internet</a><br>',
                                '<a href="https://ecipe.org/dte/dte-report/">Digital Trade Restrictiveness Index</a><br>',
                                '<a href="https://theinclusiveinternet.eiu.com/">Inclusive Internet Index</a><br>',
                                '<a href="https://www.eiu.com/public/topical_report.aspx?campaignid=microscope2019">EIU Global Microscope</a><br>',
                                '<a href="https://ncsi.ega.ee/">National Cyber Security Index</a><br>', 
                                '<a href="https://freedomhouse.org/report/freedom-net/freedom-net-2018/rise-digital-authoritarianism">Freedom House</a><br>',
                                '<a href="http://www.mobileconnectivityindex.com/">GSMA Mobile Connectivity Index</a><br>',
                                '<a href="https://www.gsma.com/mobilemoneymetrics/#regulatory-index">GSMA Mobile Money Regulatory Index</a><br>',
                                '<a href="https://www.itu.int/en/ITU-D/Statistics/Pages/stat/default.aspx">International Telecommunications Union</a><br>',
                                '<a href="https://index.okfn.org/place/">Open Knowledge Foundation</a><br>', 
                                '<a href="https://rsf.org/en/2019-world-press-freedom-index-cycle-fear">Reporters Without Borders</a><br>',
                                '<a href="http://tcg.uis.unesco.org/">UNESCO: SDG4 indicators</a><br>',
                                '<a href="http://www.upu.int/en/the-upu/strategy/2ipd.html">Universal Postal Union</a><br>',
                                '<a href="https://www.v-dem.net/en/">Varieties of Democracy</a><br>',
                                '<a href="https://www.doingbusiness.org/">World Bank: Doing Business</a><br>', 
                                '<a href="https://globalfindex.worldbank.org/">World Bank: Global Findex</a><br>',
                                '<a href="https://www.weforum.org/reports/the-global-information-technology-report-2016">WEF Global Information Technology Report</a><br>',
                                '<a href="https://reports.weforum.org/global-information-technology-report-2016/networked-readiness-index/">WEF Networked Readiness Index</a><br>',
                                '<a href="https://worldjusticeproject.org/our-work/research-and-data/wjp-rule-law-index-2019">World Justice Project</a><br>',
                                '<a href="http://www.womanstats.org/">WomanStats</a><br>'))
    
    source_str <- filter(source_tbl,short %in% sources)$full %>% paste(collapse='')
    paste0('<b>Sources:</b><br><br>',source_str)
  })
  
})
