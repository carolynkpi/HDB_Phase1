library(shiny)
library(shinydashboard)

source('eda.R')

con <- file("intro.txt", "rt") 
intro = readLines(con, -1)


### UI Code ####
ui = dashboardPage(
  
  ## Dashboard settings ####
  skin = 'black',
  
  dashboardHeader(title = 'Singapore HDB Resale Prices', titleWidth = 400),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro'),
      menuItem('Exploratory Data Analysis', tabName = 'eda', startExpanded = TRUE,
               menuSubItem('Summaries', tabName = 'summary'),
               menuSubItem('Trends Over Time', tabName = 'trend'),
               menuSubItem('Distributions', tabName = 'dist')
      )
    )
  ), #end of sidebar
  
  ## Dashboad body ####
  dashboardBody(fluidPage(tabItems(
    # Intro tab ####
    tabItem(tabName = 'intro',fluidPage(
      fluidRow(
        h2("Introduction"),
        br(),
        box(width = 5,height = 450, htmlOutput('myintro')),
        box(width = 7,height = 450, imageOutput('project'))
      ) 
    )), #end of tab 1
    
    # Summary Tab ####
    tabItem(tabName = 'summary',fluidPage(
      fluidRow(
        h2('Summaries'),
        br(),
        box(width = 3, height = 500,
            column(12, selectInput(inputId = 'agg_func', label = 'Choose a summary statistic:', choice = c('Mean', 'Median'))),
            column(12, selectInput(inputId = 'agg_num1', label = 'Which numerical variable do you want to summarize?', choice = num_var_d, selected = 'Resale Price')),
            column(12, strong('How do you want to group the data?')),
            column(12, selectInput(inputId = 'agg_cat1', label = 'on x-axis:', choice = fac_var_d, selected = 'Town')),
            column(12, uiOutput('agg_cat2vars')),
            column(12, checkboxInput(inputId = 'agg_desc', label = 'Descending order. ', value = TRUE))
        ),
        
        box(width = 2, height = 500,
            column(12, uiOutput('agg_cat2vals')),
            column(12, strong ('X-axis filter'),
                   checkboxInput(inputId = 'agg_cat1allvals', label = 'Select all x. ', value = TRUE),
                   uiOutput('agg_cat1vals'))
        ),
        
        box(width = 7, height = 500,
            plotOutput(outputId = 'agg')
        )
      )
    )), #end of tab2
    
    # Trend Tab ####
    tabItem(tabName = 'trend', fluidPage(
      fluidRow(
        h2("Trend Over Time"),
        br(),
        box(width = 3, height = 400,
            column(12, selectInput(inputId = 'trend_func', label = 'Choose a summary statistic:', choice = c('Mean', 'Median'))),
            column(12, selectInput(inputId = 'trend_num1', label = "Which numerical variable do you want to see its trend?", choice = num_var_d, selected = 'Resale Price')),
            column(12, selectInput(inputId = 'trend_cat1', label = "Do you want to split the trend? by: ", choice = c('Not Required',fac_var_d)))
        ),
        
        box(width = 2, height = 400,
            column(12, 
                   strong('Split filter:'),
                   checkboxInput(inputId = 'trend_cat1allvals', label = 'Select all values. ', value = TRUE),
                   uiOutput('trend_cat1vals'))
        ),
        
        box(width = 7, height = 400,
            plotOutput(outputId = 'trend')
        )
        
      )
    )), #end of tab3
    
    # Distribution Tab ####
    tabItem(tabName = 'dist', fluidPage(
      fluidRow(
        h2('Distributions'),
        br(),
        tabsetPanel(type = 'tabs',
          tabPanel('Single Variable',
            br(),
            box(width = 3, height = 500,
                column(12, selectInput(inputId = 'unidist_var1', label = "Choose a variable to see its distribution", choice = all_var_d, selected = 'Resale Price')),
                column(12, uiOutput('unidist_cat1')),
                column(2),
                checkboxInput(inputId = 'unidist_cat1allvals', label = 'Select all values. ', value = TRUE),
                column(12, uiOutput('unidist_cat1vals'))
            ),
            
            box(width = 9, height = 500, 
                strong("Summary"),
                verbatimTextOutput('unidist_summary'),
                plotOutput('unidist', height = '350px'))
                   
          ),
          
          tabPanel('Multiple Variables',
            br(),
            
            box(width =3, height = 500,
                column(12, selectInput(inputId = 'multidist_x', label = 'Choose a variable for x-axis.', choices = all_var_d, selected = 'Floor Area Sqft')),
                column(12, selectInput(inputId = 'multidist_y', label = 'Choose a variable for y-axis.', choices = all_var_d, selected = 'Resale Price')),
                column(12, selectInput(inputId = 'multidist_panel', label = 'Choose a variable for panels.', choices = c('Not Required', fac_var_d))),
                column(12, selectInput(inputId = 'multidist_color', label = 'Choose a variable for colors.', choices = c('Not Required', fac_var_d)))
            ),
            
            box(width = 3, height = 500,
                column(12, strong('Filter panels:')),
                column(12, checkboxInput('multidist_panelallvals', label = 'Select all values.', value = TRUE)),
                column(12, uiOutput('multidist_panelvals')),
                
                column(12, strong('Filter colors:')),
                column(12, checkboxInput('multidist_colorallvals', label = 'Select all values.', value = TRUE)),
                column(12, uiOutput('multidist_colorvals'))
            ),
            
            box(width = 6, height = 500, plotOutput('multidist'))
          )
        )
      )
    
    
    )) #end of tab4
    
    
  )))
)

    
    
### Server Code ####
server = function(input, output){
  
  ## Intro ####
  output$project = renderImage({
    list(src = 'HDBproject.jpg',
         contentType = 'image/jpg',
         width = 580,
         height = 300,
         alt = "Project Plan")
  },deleteFile = FALSE)
  
  output$myintro = renderUI({
    text = ''
    for(i in intro){
      text = paste(text, i, "<br/>")
    }
    HTML(text)
  })
    
  ## Summaries ####
  output$agg_cat2vars = renderUI({
    agg_cat2vars = fac_var_d [fac_var_d != input$agg_cat1]
    selectInput(inputId = 'agg_cat2', label = 'on panels:', choice = c('Not Required', agg_cat2vars))
  })
  
  output$agg_cat1vals = renderUI({
    agg_cat1valuelist = levels(data[[getOldName(input$agg_cat1)]])
    selectInput(inputId = 'agg_cat1vals', label = NULL, choices = agg_cat1valuelist, selected = agg_cat1valuelist[1], multiple = TRUE)
  })
  
  output$agg_cat2vals = renderUI({
    if(input$agg_cat2 == 'Not Required'){
      agg_cat2valuelist = 'Not Required'
    }else{agg_cat2valuelist = levels(data[[getOldName(input$agg_cat2)]])}
    selectInput(inputId = 'agg_cat2vals', label = 'Panel filter', choices = agg_cat2valuelist, selected = agg_cat2valuelist[1], multiple = TRUE)
  })
  
  output$agg = renderPlot({
    if (input$agg_cat1allvals == TRUE){
      agg_cat1vals = levels(data[[getOldName(input$agg_cat1)]])
    }
    else{
      agg_cat1vals = input$agg_cat1vals
    }
    if(input$agg_cat2vals == 'Not Required'){
      agg_bi(data,getOldName(input$agg_cat1), getOldName(input$agg_num1), agg_cat1vals, input$agg_func, desc= input$agg_desc)
    }else{
      agg_tri(data, getOldName(input$agg_cat1), getOldName(input$agg_num1), getOldName(input$agg_cat2), agg_cat1vals, input$agg_cat2vals, input$agg_func, desc= input$agg_desc)
    }
  })
  
  

  
  ## Trend ####
  output$trend_cat1vals = renderUI({
    if(input$trend_cat1 == 'Not Required'){
      trend_cat1valuelist = 'Not Required'
    }else{trend_cat1valuelist = levels(data[[getOldName(input$trend_cat1)]])}
    selectInput(inputId = 'trend_cat1vals', label = NULL, choices = trend_cat1valuelist, selected = trend_cat1valuelist[1], multiple = TRUE)
    
  })
  output$trend = renderPlot({
    if(input$trend_cat1 == 'Not Required'){
      num_trend_gen(data, getOldName(input$trend_num1),input$trend_func)
    }else{
      if(input$trend_cat1allvals == TRUE) {
        trend_cat1vals = levels(data[[getOldName(input$trend_cat1)]])
      }else{
        trend_cat1vals = input$trend_cat1vals
      }
      num_trend_bycat(data, getOldName(input$trend_num1), getOldName(input$trend_cat1), trend_cat1vals,input$trend_func)
    }
  })
  
  ## Distributions ####
  output$unidist_cat1 = renderUI({
    if (input$unidist_var1 %in% num_var_d){
      unidist_cat1list = fac_var_d
    }
    if (input$unidist_var1 %in% fac_var_d){
      unidist_cat1list = fac_var_d[fac_var_d!=input$unidist_var1]
    }
    selectInput(inputId = 'unidist_cat1', label = 'Filter by:', choice = unidist_cat1list, selected = 'region')
  })
  
  output$unidist_cat1vals = renderUI({
    unidist_cat1valuelist = levels(data[[getOldName(input$unidist_cat1)]])
    selectInput(inputId = 'unidist_cat1vals', label = NULL, choice = unidist_cat1valuelist, multiple = TRUE)
  })
  
  output$unidist_summary = renderPrint({
    if (input$unidist_cat1allvals == TRUE){
      unidist_cat1vals = levels(data[[getOldName(input$unidist_cat1)]])
    }else{
      unidist_cat1vals = input$unidist_cat1vals
    }    
    unidist_summary(data,getOldName(input$unidist_var1),getOldName(input$unidist_cat1), unidist_cat1vals)
  })
  
  output$unidist = renderPlot({
    
    if (input$unidist_cat1allvals == TRUE){
      unidist_cat1vals = levels(data[[getOldName(input$unidist_cat1)]])
    }else{
      unidist_cat1vals = input$unidist_cat1vals
    }
    
    if (input$unidist_var1 %in% num_var_d){
      unidist_num(data, getOldName(input$unidist_var1),getOldName(input$unidist_cat1), unidist_cat1vals)
    }
    if (input$unidist_var1 %in% fac_var_d){
      unidist_cat(data,getOldName(input$unidist_var1),getOldName(input$unidist_cat1), unidist_cat1vals)
    }
  })
  
  output$multidist_panelvals = renderUI({
    if(input$multidist_panel=='Not Required'){
      multidist_panelvaluelist = 'Not Required'
    }else{
      multidist_panelvaluelist = levels(data[[getOldName(input$multidist_panel)]])
    }
    selectInput(inputId = 'multidist_panelvals', label = NULL, choices = multidist_panelvaluelist, multiple = TRUE)
  })
  
  output$multidist_colorvals =renderUI({
    if(input$multidist_color=='Not Required'){
      multidist_colorvaluelist = 'Not Required'
    }else{
      multidist_colorvaluelist = levels(data[[getOldName(input$multidist_color)]])
    }
    selectInput(inputId = 'multidist_colorvals', label = NULL, choices = multidist_colorvaluelist, multiple = TRUE)
  })
  
  output$multidist = renderPlot({
    if(input$multidist_panelallvals){
      multidist_panelvals = levels(data[[getOldName(input$multidist_panel)]])
    }else{
      multidist_panelvals = input$multidist_panelvals
    }
    if(input$multidist_colorallvals){
      multidist_colorvals = levels(data[[getOldName(input$multidist_color)]])
    }else{
      multidist_colorvals = input$multidist_colorvals
    }
    
    dist(data,x= getOldName(input$multidist_x), y = getOldName(input$multidist_y),
         pan = getOldName(input$multidist_panel), pan_vals = multidist_panelvals, 
         col = getOldName(input$multidist_color), col_vals = multidist_colorvals)
  })
  
  
}

### ShinyApps ####
shinyApp(ui = ui, server = server)