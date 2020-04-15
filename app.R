## Load required packages:
library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(wesanderson)
library(RColorBrewer)

### Ui code begins below:
ui <- navbarPage(#shinythemes::themeSelector(),
    theme = shinytheme("spacelab"), 
    title = h3("Demo App: Kshitij Kolas"),
    
    ## First Tab:
    tabPanel(h4("Data of free standing component"),
             sidebarLayout(
               ## Upload the data for free standing component
               sidebarPanel(h3("Upload File"),
                            fileInput("file1", "Upload .txt/.csv File/s for the data of Free Standing Component", multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values, text/plain",
                                                 ".csv")),
                            checkboxInput(inputId = 'header1', label = 'Header', value =  T),
                            checkboxInput(inputId = "stringAsFactors1", "stringAsFactors", FALSE),
                            selectInput(inputId = 'sep1', label =  'Separator', choices =   c(Comma =',',Semicolon =';', Tab='\t', Space=' ', Period ='.'), selected  =  ';'),
                            wellPanel(uiOutput("selectfile1")),
                            wellPanel(h3("Select The X and Y Axes Data"),
                                      selectInput('xcol1', 'X Variable', "", selected = ""),
                                      selectInput('ycol1', 'Y Variable', "", selected = "")),
                            width = 3),
                            mainPanel(
                              ## Original Data Sets:
                              wellPanel(h3("Raw Data"), uiOutput("tb1")),
                              wellPanel(h3("Plot Output"),
                                        ## Plotly plot display:
                                            plotOutput("plot1", brush = "User_brush_1")),
                              ## Updated Data Sets:
                              wellPanel(h3("Filtered Data"),
                                            DT::dataTableOutput("table_1"),
                                            downloadButton(outputId = "mydownload1", label = "Download Table"))
                            ))),
    ## Second Tab
    tabPanel(h4("Data of Pure PCB without component"),
             sidebarLayout(
               ## Upload the Data of Pure PCB without component
               sidebarPanel(h3("Upload File"),
                            fileInput("file2", "Upload .txt/.csv File/s for the data of Pure PCB without component", multiple = T,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values, text/plain",
                                                 ".csv")),
                            checkboxInput(inputId = 'header2', label = 'Header', value =  T),
                            checkboxInput(inputId = "stringAsFactors2", "stringAsFactors", FALSE),
                            selectInput(inputId = 'sep2', label =  'Separator', choices =   c(Comma =',',Semicolon =';', Tab='\t', Space=' ', Period ='.'), selected  =  ';'),
                            wellPanel(uiOutput("selectfile2")),
                            wellPanel(h3("Select The X and Y Axes Data"),
                                      selectInput('xcol2', 'X Variable', "", selected = ""),
                                      selectInput('ycol2', 'Y Variable', "", selected = "")),
                            width = 3),
               mainPanel(
                 ## Original Data Sets:
                 wellPanel(h3("Raw Data"), uiOutput("tb2")),
                 wellPanel(h3("Plot Output"),
                           ## Plotly plot display:
                           plotOutput("plot2", brush = "User_brush_2")),
                 ## Updated Data Sets:
                 wellPanel(h3("Filtered Data"),
                           DT::dataTableOutput("table_2"),
                           downloadButton(outputId = "mydownload2", label = "Download Table"))
               ))),
    
    ## Third Tab:
    tabPanel(title = h4("Unmounted State")),
    
    ## Fourth Tab:
    tabPanel(title = h4("Mounted State")),
    
    ## Fifth Tab:
    tabPanel(title = h4("Interaction"))
    
)

### Server side code begins below:
server <- function(input, output, session){

  ### Tab 1:
  output$filedf1 <- renderTable({
    if(is.null(input$file1)){return ()}
    input$file1 
  })
  
  # Extract the file path for file
  output$filedf2 <- renderTable({
    if(is.null(input$file1)){return ()}
    input$file1$datapath
  })
  
  ## Below code to display the structure of the input file object
  output$fileob1 <- renderPrint({
    if(is.null(input$file1)){return ()}
    str(input$file1)
  })
  
  # Following code displays the select input widget with the list of file loaded by the user
  output$selectfile1 <- renderUI({
    if(is.null(input$file1)) {return()}
    list(hr(), 
         helpText("Select the files for which you need to see data and summary stats"),
         selectInput("Select1", "Select", choices=input$file1$name))
  })
  
  ## Summary Stats code
  output$summ1 <- renderPrint({
    if(is.null(input$file1)){return()}
    summary(read.table(file=input$file1$datapath[input$file1$name==input$Select1], 
                       sep=input$sep1, 
                       header = input$header1, 
                       stringsAsFactors = input$stringAsFactors1))})
  
  ## Reading the uploaded .CSV file: 
  data1 <- reactive({
    req(input$file1)
    df1 <- read.table(file=input$file1$datapath[input$file1$name==input$Select1], sep=input$sep1, header = input$header1, stringsAsFactors = input$stringAsFactors1)
    updateSelectInput(session, inputId = 'xcol1', label = 'X Variable',
                      choices = names(df1), selected = names(df1)[1])
    updateSelectInput(session, inputId = 'ycol1', label = 'Y Variable',
                      choices = names(df1), selected = names(df1))
    return(df1)
  })
  
  ## Data Table 
  output$table1 <- DT::renderDataTable({
    if(is.null(input$file1)){return()}
    else
    data1()
      })
  
  ## Rendering ggplotly scatter plot: 
  output$plot1 <- renderPlot({
    x1 <- data1()[, c(input$xcol1, input$ycol1)]
    ggplot(data =  x1) + 
      geom_line(aes_string(x = input$xcol1, y = input$ycol1, color = input$ycol1), size = 1) + 
      # geom_point(aes_string(x = input$xcol1, y = input$ycol1, color = input$ycol1), size = 1) +
      scale_colour_gradientn(colours=rainbow(6))
      # stat_smooth(aes_string(x = input$xcol1, y = input$ycol1, color = input$ycol1))
  })
  
  
  ## Using Brush on the Plotted graph:
  data1.new <- reactive({
    User_brush_1 <- input$User_brush_1
    mysel1 <- brushedPoints(data1(), User_brush_1)
    return(mysel1)
  })
  
  ## Rendering the filered data:
  output$table_1 <- DT::renderDataTable(DT::datatable(data1.new()))
  
  ## Download the filtered data:
  output$mydownload1 <- downloadHandler(
    filename = "Filtered_Data_Default_000.csv",
    content = function(file){
      write.csv(data1.new(), file)})
  
  ## MainPanel tabset renderUI code
  output$tb1 <- renderUI({
    if(is.null(input$file1)) {return()}
    else
      tabsetPanel(
        tabPanel("Dataset", DT::dataTableOutput("table1")),
        tabPanel("Input File Object DF ", tableOutput("filedf1"), tableOutput("filedf2")),
        tabPanel("Input File Object Structure", verbatimTextOutput("fileob1")),
        tabPanel("Summary Stats", verbatimTextOutput("summ1")))
  })
  
  ### Tab 2:
  
  output$filedf3 <- renderTable({
    if(is.null(input$file2)){return ()}
    input$file2 
  })
  
  # Extract the file path for file
  output$filedf4 <- renderTable({
    if(is.null(input$file2)){return ()}
    input$file2$datapath
  })
  
  ## Below code to display the structure of the input file object
  output$fileob2<- renderPrint({
    if(is.null(input$file2)){return ()}
    str(input$file2)
  })
  
  # Following code displays the select input widget with the list of file loaded by the user
  output$selectfile2 <- renderUI({
    if(is.null(input$file2)) {return()}
    list(hr(), 
         helpText("Select the files for which you need to see data and summary stats"),
         selectInput("Select2", "Select", choices=input$file2$name))
  })
  
  ## Summary Stats code
  output$summ2 <- renderPrint({
    if(is.null(input$file2)){return()}
    summary(read.table(file=input$file2$datapath[input$file2$name==input$Select2], 
                       sep=input$sep2, 
                       header = input$header2, 
                       stringsAsFactors = input$stringAsFactors2))})
  
  ## Reading the uploaded .CSV file: 
  data2 <- reactive({
    req(input$file2)
    df2 <- read.table(file=input$file2$datapath[input$file2$name==input$Select2], sep=input$sep2, header = input$header2, stringsAsFactors = input$stringAsFactors2)
    updateSelectInput(session, inputId = 'xcol2', label = 'X Variable',
                      choices = names(df2), selected = names(df2)[1])
    updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                      choices = names(df2), selected = names(df2))
    return(df2)
  })
  
  ## Data Table 
  output$table2 <- DT::renderDataTable({
    if(is.null(input$file2)){return()}
    else
      data2()
  })
  
  ## Rendering ggplotly scatter plot: 
  output$plot2 <- renderPlot({
    x2 <- data2()[, c(input$xcol2, input$ycol2)]
    ggplot(data =  x2) + 
      geom_line(aes_string(x = input$xcol2, y = input$ycol2, color = input$ycol2), size = 1) + 
      # geom_point(aes_string(x = input$xcol2, y = input$ycol2, color = input$ycol2), size = 1) +
      scale_colour_gradientn(colours=rainbow(6))
      #stat_smooth(aes_string(x = input$xcol2, y = input$ycol2, color = input$ycol2))
  })
  
  ## Using Brush on the Plotted graph:
  data2.new <- reactive({
    User_brush_2 <- input$User_brush_2
    mysel2 <- brushedPoints(data2(), User_brush_2)
    return(mysel2)
  })
  
  ## Rendering the filered data:
  output$table_2 <- DT::renderDataTable(DT::datatable(data2.new()))
  
  ## Download the filtered data:
  output$mydownload2 <- downloadHandler(
    filename = "Filtered_Data_Default_000.csv",
    content = function(file){
      write.csv(data2.new(), file)})
  
  ## MainPanel tabset renderUI code
  output$tb2 <- renderUI({
    if(is.null(input$file2)) {return()}
    else
      tabsetPanel(
        tabPanel("Dataset", DT::dataTableOutput("table2")),
        tabPanel("Input File Object DF ", tableOutput("filedf3"), tableOutput("filedf4")),
        tabPanel("Input File Object Structure", verbatimTextOutput("fileob2")),
        tabPanel("Summary Stats", verbatimTextOutput("summ2")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)