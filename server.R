## Shiny Server component for dashboard

function(input, output, session){
  
  # Data table Output
  output$dataT = renderDataTable(
    
    mydata %>% subset(equipement==input$var99)
    
    )

  
  # Rendering the box header  
  output$head1 = renderText(
    paste("5 higher measurements of", input$var2)
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 lower measurements of ", input$var2)
  )
  
  
  # Rendering table with 5 states with high arrests for specific crime type
  output$top5 = renderTable({
    
    mydata %>% subset(equipement==input$var11) %>% 
      select(Date, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  
  # Rendering table with 5 states with low arrests for specific crime type
  output$low5 = renderTable({
    
    mydata %>% subset(equipement==input$var11) %>% 
      select(Date, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
    
    
  })
  
  
  # For Structure output
  output$structure = renderPrint({
    mydata %>% subset(equipement==input$var88) %>% skim()
  })
  
  
  # For Summary Output
  output$summary = renderPrint({
    mydata %>% 
      summary()
  })
  
  # For histogram - distribution charts
  output$histplot = renderPlotly({
    p1 = mydata %>% subset(equipement==input$var0) %>% plot_ly() %>% add_histogram(x=~get(input$var1)) %>% layout(xaxis = list(title = paste(input$var1)))
    

    
    p2 = mydata %>% subset(equipement==input$var0) %>%
      plot_ly() %>%
      add_boxplot(x=~get(input$var1)) %>% 
      layout(yaxis = list(showticklabels = F))
    
  # stacking the plots on top of each other
    subplot(p2, p1, nrows = 2, shareX = TRUE) %>%
      hide_legend() %>% 
      layout(title = "Distribution chart - Histogram and Boxplot",
             yaxis = list(title="Frequency"))
  })
  
  
  ### Bar Charts - State wise trend
  output$bar = renderPlotly({
    
    m=mydata %>% subset(equipement==input$var11) %>% ggplot(aes(x=DateC, y=get(input$var2))) + geom_line(color = "#FFC433", size = 1) + stat_smooth(
      color = "#00AFBB", fill = "#00AFBB",
      method = "loess"
      ) + labs(title = paste("Distribution chart of ", input$var2),
               x = "DateTime",
               y = input$var2) +
      theme(  plot.title = element_textbox_simple(size=10,halign=0.5))
    
    ggplotly(m) 
    
    
    
    #+ labs(title = paste("Evolution de ", input$var2),
          # x = input$var3,
         #  y = input$var4) +
      #theme(  plot.title = element_textbox_simple(size=10,halign=0.5))
    
    
    #plot(x=baro$Date, y=~get(input$var22)) 
    #baro %>% amTimeSeries("Date", c("Temperator","Ventilator_throughput"),groupToPeriods=c('DD'),main = "Baie_0003")
       
    #%>% 
     # layout(title = paste("variation for", input$var2),
             #xaxis = list(title = "Date"),
             #yaxis = list(title = paste(input$var2, "mesure per equipement") ))
  })
  
  ### Scatter Charts 
  output$scatter <- renderPlotly({
    p = mydata %>% subset(equipement==input$var44) %>% ggplot(aes(x=get(input$var3), y=get(input$var4))) +
      geom_point() +
      geom_smooth(method=get(input$fit)) +
      labs(title = paste("Relation b/w", input$var3 , "and" , input$var4),
           x = input$var3,
           y = input$var4) +
      theme(  plot.title = element_textbox_simple(size=10,halign=0.5))
      
    
    
    
    # applied ggplot to make it interactive
    ggplotly(p)
    
  })
  
  
  ## Correlation plot
 
  output$cor <- renderPlotly({
      
    p3 <- mydata  %>% subset(equipement==input$var33) %>% select(input$var2,input$var22)
    
    
    #select(input$var2,input$var22)
    
    # Compute a correlation matrix
    corr <- round(cor(p3), 1)
    
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(p3)
    
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE, 
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
  })
  

    # Choropleth map
  #output$map_plot <- renderPlot({
      #new_join %>% 
      #ggplot(aes(x=long, y=lat,fill=get(input$crimetype) , group = group)) +
     # geom_polygon(color="black", size=0.4) +
      #scale_fill_gradient(low="#73A5C6", high="#001B3A", name = paste(input$crimetype, "Arrest rate")) +
      #theme_void() +
      #labs(title = paste("Choropleth map of", input$crimetype , " Arrests per 100,000 residents by state in 1973")) +
      #theme(
       # plot.title = element_textbox_simple(face="bold", 
                                           # size=18,
                                          #  halign=0.5),
        
       # legend.position = c(0.2, 0.1),
       # legend.direction = "horizontal"
        
     # ) +
     # geom_text(aes(x=x, y=y, label=abb), size = 4, color="white")
    
    
 
 # })
  
# Prediction Plot
  
  output$predchart = renderPlotly({
    
    
    
    seri33 = mydata %>% subset(equipement==input$var55) %>% select(c("Temperator","Date")) %>% drop_na()
    seri33$Date = ymd(seri33$Date)
    
    df_split1 = initial_time_split(seri33, prop = 0.7)
    model_fit_prophet = prophet_reg() %>% set_engine(engine = "prophet") %>% fit(Temperator ~ Date, data = training(df_split1))
    calibration_prophet=model_fit_prophet %>% modeltime_calibrate(new_data = testing(df_split1))
    p5=calibration_prophet %>% modeltime_forecast(new_data = testing(df_split1), actual_data = seri33) %>% plot_modeltime_forecast(.legend_max_width = 20, .interactive = TRUE)
    
    ggplotly(p5)
    
    
  })
  
  
  
  
  output$predchartfuture= renderPlotly({
    
    
    seri33 = mydata %>% subset(equipement==input$var77) %>% select(c("Temperator","Date")) %>% drop_na()
    seri33$Date = ymd(seri33$Date)
    df_split1 = initial_time_split(seri33, prop = 0.7)
    
    model_fit_prophet = prophet_reg() %>% set_engine(engine = "prophet") %>% fit(Temperator ~ Date, data = training(df_split1))
    calibtotal= model_fit_prophet %>% modeltime_calibrate(new_data = testing(df_split1))
    
    refit_tbl = calibtotal %>% modeltime_refit(data = seri33)
    refit_tbl %>% modeltime_forecast(h = paste(input$var7," months"), actual_data = seri33) %>% plot_modeltime_forecast(.legend_max_width = 25, .interactive = TRUE
      )
    
  })
}



