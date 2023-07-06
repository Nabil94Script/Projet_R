## Shiny UI component for the Dashboard

dashboardPage(skin = "blue",
  
  dashboardHeader(title="preventive maintenance solution", titleWidth = 650,
                  #tags$li(class="dropdown",tags$a(href="https://www.youtube.com/playlist?list=PL6wLL_RojB5xNOhe2OTSd-DPkMLVY9DfB", icon("youtube"), "My Channel", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://fr.linkedin.com/company/amanze" ,icon("linkedin"), "Profile linkedin", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="", icon("github"), "Source Code", target="_blank"))
                  ),
  
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Data observation", tabName = "viz", icon=icon("chart-line")),
      menuItem("Data prediction", tabName = "predict", icon=icon("chart-line")),
      
      # Conditional Panel for conditional widget appearance
      # Filter should appear only for the visualization menu and selected tabs within it
      
      conditionalPanel("input.sidebar == 'data' && input.t1 == 'dataeq'", selectInput(inputId = "var99" , label ="Select the equipement" , choices = c0)),
      
      conditionalPanel("input.sidebar == 'data' && input.t1 == 'datastr'", selectInput(inputId = "var88" , label ="Select the equipement" , choices = c0)),
      
      
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var0" , label ="Select the equipement" , choices = c0)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'", selectInput(inputId = "var1" , label ="Select the Variable" , choices = c1)),
      
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends'", selectInput(inputId = "var11" , label ="Select the equipement" , choices = c0)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ", selectInput(inputId = "var2" , label ="Select the Mesure type" , choices = c1)),
      
      
      
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'correl'", selectInput(inputId = "var33" , label ="Select the equipement" , choices = c0)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'cor' ", selectInput(inputId = "var22" , label ="Select the Mesure type" , choices = c3)),
    

     
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'", selectInput(inputId = "var44" , label ="Select the equipement" , choices = c0)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var3" , label ="Select the X variable" , choices = c1)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1)),
      
      
      conditionalPanel("input.sidebar == 'predict' && input.t3 == 'pred'", selectInput(inputId = "var55" , label ="Select the equipement" , choices = c0)),
      
      
      conditionalPanel("input.sidebar == 'predict' && input.t3 == 'pred1'", selectInput(inputId = "var77" , label ="Select the equipement" , choices = c0)),
      conditionalPanel("input.sidebar == 'predict' && input.t3 == 'pred1'", selectInput(inputId = "var7" , label ="Select period" , choices = c4))
      
      
      
      
      
      #menuItem("Choropleth Map", tabName = "map", icon=icon("map"))
      
    )
  ),
  
  
  
  dashboardBody(
    
    tabItems(

      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12,
                     tabPanel("About", icon=icon("address-card"),

    fluidRow(
      
  column(width = 8, tags$img(src="https://media.licdn.com/dms/image/D4D12AQH1v5pJm7IMQw/article-cover_image-shrink_600_2000/0/1672673932702?e=2147483647&v=beta&t=asup5jwHpFNF6ic6dv4xo9-0xqhpfVYVHllYjmvr5RY", width =600 , height = 300),
         tags$br() , 
         tags$a("Amanze"), align = "center"),
  column(width = 4, tags$br() ,
         tags$p("This machine learning solution detects temperature jumps or predicts temperature levels over the next 6 months, with the aim of limiting temperature-related breakdowns.")
              )
            )
         ), 
                     tabPanel("Data", dataTableOutput("dataT"), icon = icon("table"),value='dataeq'), 
                     tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted"),value='datastr'),
                     #tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
         )

     ),  
  
  
  
    
    # Second Tab Item
    tabItem(tabName = "viz", 
            tabBox(id="t2",  width=12, 
                   tabPanel("Data observation", value="trends",
                            fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                     tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                            ),  withSpinner(plotlyOutput("bar"))
                   ),
            tabPanel("Distribution", value="distro", withSpinner(plotlyOutput("histplot", height = "350px"))),
                     # selectInput("var", "Select the variable", choices=c("Rape", "Assault")),
            tabPanel("Correlation Matrix", id="corr" , value="correl", withSpinner(plotlyOutput("cor"))),
            tabPanel("Relationship among temperator & Ventilation Troughput", 
                     radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                     withSpinner(plotlyOutput("scatter")), value="relation"),
            side = "left"
                   )
    ),

    # prediction Tab item


    tabItem(tabName = "predict", tabBox(id="t3",  width=12, 
                                        
                                        tabPanel("Adjustment of the prediction model", value="pred", withSpinner(plotlyOutput("predchart"))),
                                        tabPanel("future forecasts", value="pred1", withSpinner(plotlyOutput("predchartfuture")))
          
                                        ))

  )
)
)
  
  
