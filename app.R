#--------notes
# need to add .dat to filename in name files sheet and spell the file name EXACTLY as it is named including spaces and symbols
# in the name file Sample_Name must be spelled this exact way
#-----------------------------
library(tidyverse)
library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  tags$style(type = 'text/css',
             "header{position: fixed; top:20px; right: 50px; padding:1px; z-index: 1000;}"),
  tags$style(type = 'text/css',
             "div.b {
  text-decoration-line: overline;
  text-decoration-style: wavy;
  text-decoration-color: #8300b5;
  font-weight: bold;
  color: #696969;
}"),
    navbarPage(HTML("<div class ='fa fa-seedling'style='color:#006cff'></i></div>","<div class ='fa fa-seedling'style='color:#00ff17'></i></div>","<div class ='fa fa-seedling'style='color:#fff200'></i></div>","<div class ='fa fa-seedling'style='color:red'></i></div>", "<div class = 'b'>FluoroLogger</div>"),  windowTitle = "FluoroLogger",header = HTML("<header><i> App by Jared Brabazon </i></header>"),
                   
               # multi-page user-interface that includes a navigation bar.
#Data tab--------------------------------------------------------------------------------------------------
               tabPanel(h5(icon("table"), "Dataset"),
                        sidebarPanel(
                          h4("Step 1"),
                          fileInput(inputId = "nam", label = ".csv File Names", accept = c('text/csv','text/comma-separated-values,text/plain','.csv')),
                          h4("Step 2"),
                          fileInput(inputId = "dat", label = ".dat Files", multiple = TRUE, accept = c('.dat')),
                          h4("Step 3"),
                          actionButton(inputId = "tableit", label = "Make Table", class = "btn-primary"),
                          br(),
                          br(),
                          downloadButton("downloadDatawide", "Download Wide"),
                          br(),
                          downloadButton("downloadDatalong", "Download Long")
                          
                          ),
                          
                        
               
                        mainPanel(
                          tabsetPanel(
                            # Data 
                            tabPanel(p(icon("table"), "Dataset Long"),
                                     DTOutput(outputId="table")
                            ),
                            tabPanel(p(icon("table"), "Dataset Wide"),
                                     DTOutput(outputId="table2")
                            )
                          )
                         
                        )     
               ),
#Normalization tab--------------------------------------------------------------------------------------------------               
    tabPanel(h5(icon("line-chart"), "Normalization"),
             sidebarPanel(
               h4("Step 1"),
               uiOutput(outputId = "blanks"),
               h4("Step 2"),
               uiOutput(outputId = "samples"),
               h4("Step 3"),
               actionButton(inputId = "plotorig", label = "Plot Raw", class = "btn-primary"),
               h4("Step 4"),
               numericInput(inputId = "norm", label = "Normalization Value", value = NULL),
               h4("Step 5"),
               textInput(inputId = "color", label = tags$a(href="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", "Choose color options", target="_blank"), value = "chartreuse"),
               h4("Step 6"),
               uiOutput(outputId = "leftnm"),
               uiOutput(outputId = "rightnm"),
               h4("Step 7"),
               actionButton(inputId = "plotnorm", label = "Plot Normalized", class = "btn-primary"),
               br(),
               br(),
               downloadButton("downloadraw", "Download Not Normalized"),
               br(),
               downloadButton("downloadnorm", "Download Normalized"),
               br(),
               h4("Step 8"),
               textInput(inputId = "namefinal", label = "Sample Name", value = NULL),
               actionButton(inputId = "sendtofinal", label = "Send to final dataset", class = "btn-primary")
               
               
             ),
             
   
    
    mainPanel(
      tabsetPanel(
        # Data 
        tabPanel(
          p(icon("line-chart"), "Plots"),
                 plotlyOutput(outputId = "plotraw"),
                 br(),
                 br(),
                 plotlyOutput(outputId = "plotnormalized")
                 ),
        tabPanel(title = p(icon("table"), "Dataset Not-Normalized"),
                 DTOutput(outputId="table3")
        ),
        tabPanel(title = p(icon("table"), "Dataset Normalized"),
                 DTOutput(outputId="table4")
        )
        ) 
        
      )
    ),
#Data merger tab--------------------------------------------------------------------------------------------------
    tabPanel(h5(icon("copy"), "Data Merger"),
             sidebarPanel(
               uiOutput("choices"),
               br(),
               downloadButton("downloadfinallong", "Download Merged Long"),
               br(),
               downloadButton("downloadfinalwide", "Download Merged Wide")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Data 
                 tabPanel(p(icon("table"), "Final Long"),
                          DTOutput(outputId="table5")
                 ),
                 tabPanel(p(icon("table"), "Final Wide"),
                          DTOutput(outputId="table6")
                 )
               )
               
             )     
    )
)
)

server <- function(input, output){
#Data tab--------------------------------------------------------------------------------------------------
  
#make data sheet
df <- eventReactive(input$tableit,{
  req(input$nam)
  req(input$dat)
  
  mutit <- function(x, y){
    #get the sample name from input$nam
    upload <- read_csv(file = input$nam$datapath)
    
    if(isTRUE(length(unique(upload$Sample_Name)) != (nrow(upload)))){
      showModal(modalDialog(title = "Duplicate 'Sample_Name' detected in uploaded .csv file", "Please check format and try again", easyClose = TRUE, fade = FALSE))
    } else NULL
    if(isTRUE(length(unique(upload$File_Name)) != (nrow(upload)))){
      showModal(modalDialog(title = "Duplicate 'File_Name' detected in uploaded .csv file", "Please check format and try again", easyClose = TRUE, fade = FALSE))
    } else NULL
    validate(need(length(unique(upload$Sample_Name)) == (nrow(upload)), "Please check format of input files and try again"))
    validate(need(length(unique(upload$File_Name)) == (nrow(upload)), "Please check format of input files and try again"))
    
    namit <- upload %>%filter(File_Name == y)
    #read as csv and get new file name
    dat_file <- read_tsv(file = x, skip = 1)
    dat_file %>%
      mutate(File_Name = y)%>%
      mutate(Sample_Name = namit[["Sample_Name"]])
  }
  
datit <- pmap(.l = list(input$dat$datapath, input$dat$name), .f = mutit)
finaldat <- as_tibble(datit %>% reduce(full_join))

})
    
# make table long
  output$table <- renderDT({
    req(input$nam)
    req(input$dat)
    req(input$tableit)
    
    DT::datatable(df(),
                  options = list(lengthMenu = c(20, 50, 100, 150, 200), searching = TRUE))
  })
  
  wide_df <- reactive({
    wider <- df()
    wider %>%
    select(-File_Name)%>%
    spread(key = Sample_Name, value = CPS)
      
  })
  # make table long
  output$table2 <- renderDT({
    req(input$nam)
    req(input$dat)
    req(input$tableit)
    
    DT::datatable(wide_df(),
                  options = list(lengthMenu = c(20, 50, 100, 150, 200), searching = TRUE))
  })
  
  output$downloadDatawide <- downloadHandler(
    filename = function() {
      paste("datawide", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(x = wide_df(), file = file, row.names = FALSE)
    }
  )
  
  output$downloadDatalong <- downloadHandler(
    filename = function() {
      paste("datalong", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(x = df(), file = file, row.names = FALSE)
    }
  )
#Normalization tab--------------------------------------------------------------------------------------------------
  output$blanks <- renderUI({
    req(df())
    selectizeInput(inputId = "blankchoice", label = "Choose Negative Controls", choices = sort(df()$Sample_Name), multiple = TRUE)
  })
  
  output$samples <- renderUI({
    req(df())
    selectizeInput(inputId = "samplechoice", label = "Choose Replicates", choices = sort(df()$Sample_Name), multiple = TRUE)
  })

  output$leftnm <- renderUI({
    req(raw_df())
    textInput(inputId = "left", label = , "Choose left nm", value = min(raw_df()$nm))
  })
  output$rightnm <- renderUI({
    req(raw_df())
    textInput(inputId = "right", label = , "Choose right nm", value = max(raw_df()$nm))
  })
  
raw_df <- eventReactive(input$plotorig, {
  req(input$blankchoice)
  req(input$samplechoice)
  
  average_df <- df()
  
  blank_df <- average_df[average_df$Sample_Name %in% input$blankchoice,]
  sample_df <- average_df[average_df$Sample_Name %in% input$samplechoice,]
  
  avg_blank <- blank_df %>%
    group_by(nm)%>%
    summarise(avg_blank = mean(CPS), stdev_blank = sd(CPS))
  avg_sample <- sample_df %>%
    group_by(nm)%>%
    summarise(avg_sample = mean(CPS), stdev_sample = sd(CPS))
  
plot_df <- data.frame(nm = avg_sample$nm, avg_neg_controls = avg_blank$avg_blank, stdev_neg_controls = avg_blank$stdev_blank,  avg_replicates = avg_sample$avg_sample, stdev_replicates = avg_sample$stdev_sample)%>%mutate(difference = avg_neg_controls - avg_replicates)
plot_df
})

output$table3 <- renderDT({
  req(input$plotorig)
  
  DT::datatable(raw_df(),
                options = list(lengthMenu = c(20, 50, 100, 150, 200), searching = TRUE))
})

norm_df <- eventReactive(input$plotnorm, {
  req(raw_df())
  req(input$norm)
  validate(need(input$left <= input$right, "'Left nm' value must be smaller than 'Right nm' value"))
  
  average_df <- df()
  
  blank_df <- average_df[average_df$Sample_Name %in% input$blankchoice,]
  sample_df <- average_df[average_df$Sample_Name %in% input$samplechoice,]
  
  avg_blank <- blank_df %>%
    group_by(nm)%>%
    summarise(avg_blank = mean(CPS))
  avg_sample <- sample_df %>%
    group_by(nm)%>%
    summarise(avg_sample = mean(CPS))
  
  norm_plot_df <- data.frame(nm = avg_sample$nm, avg_neg_controls = avg_blank$avg_blank, avg_replicates = avg_sample$avg_sample)%>%
    mutate(normalized = avg_replicates + input$norm)
  if(!is.null(input$left)){
    norm_plot_df <- norm_plot_df %>%
      filter(nm >= input$left)
  } else NULL
  if(!is.null(input$right)){
    norm_plot_df <- norm_plot_df %>%
      filter(nm <= input$right)
  } else NULL
    
  norm_plot_df
})

output$table4 <- renderDT({
  req(input$plotnorm)
  
  DT::datatable(norm_df(),
                options = list(lengthMenu = c(20, 50, 100, 150, 200), searching = TRUE))
})


output$plotraw <- renderPlotly({
  req(raw_df())
  plots <- ggplot()+
    ggtitle("Averaged Data - Not normalized")+
    geom_line(data = raw_df(), aes_string(x = "nm", y = "avg_replicates", diff = "difference", sd = "stdev_replicates"), size = 1.5, color = "chartreuse")+
    geom_line(data = raw_df(), aes_string(x = "nm", y = "avg_neg_controls", diff = "difference", sd = "stdev_neg_controls"), size = 1.5, color = "#444444")+
    theme_light(base_size = 14)+
    theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    xlab("nm")+
    ylab("CPS")
  
  suppressMessages(ggplotly(plots, tooltip = c('x', 'y', 'diff', 'sd'), dynamicTicks = TRUE))
})

output$plotnormalized <- renderPlotly({
  req(norm_df())
  plots <- ggplot()+
    ggtitle("Averaged Data - normalized")+
    geom_line(data = norm_df(), aes_string(x = "nm", y = "normalized"), size = 1.5, color = input$color)+
    geom_line(data = norm_df(), aes_string(x = "nm", y = "avg_neg_controls"), size = 1.5, color = "#444444")+
    theme_light(base_size = 14)+
    theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                             panel.background = element_blank())+
    xlab("nm")+
    ylab("CPS")
  
  suppressMessages(ggplotly(plots, tooltip = c('x', 'y'), dynamicTicks = TRUE))
})  

output$downloadraw <- downloadHandler(
  filename = function() {
    paste("datanotnorm", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(x = raw_df(), file = file, row.names = FALSE)
  }
)  

output$downloadnorm <- downloadHandler(
    filename = function() {
      paste("datanorm", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(x = norm_df(), file = file, row.names = FALSE)
    }
  )
#Data merger--------------------------------------------------------------------------------------------------

RV <- reactiveValues(sends = list(),
                     name = NULL)
observeEvent(input$sendtofinal, {
  RV$name <- c(RV$name, input$namefinal)
  namer <- function(x){
    finalforlist <- norm_df() %>%
      select(-avg_replicates)%>%
      mutate(sample_name = x)
    
    RV$sends[[paste(x)]]<- finalforlist
  }
  namer(input$namefinal)
showNotification(paste(input$namefinal,"sent to 'Data Merger' tab"), type = "message")
})

output$choices <- renderUI({
  req(input$sendtofinal)
  req(RV$name)
  req(RV$sends)
  checkboxGroupInput(inputId = "boxnames", label = "Select data to be merged:", choices = RV$name)
})

finale_long <- reactive({
  req(input$namefinal)
  req(RV$sends)
  req(input$boxnames)

  finaltable <- RV$sends[input$boxnames]
  if(length(input$boxnames) > 1){
    finaltable <- finaltable%>%reduce(full_join)
  }else finaltable
})

finale_wide <- reactive({
  req(input$namefinal)
  req(input$boxnames)
  req(RV$sends)
  validate(need(length(input$boxnames) >=2, "Please select at least two replicates to merge data"))
  
  #list of named dataframes selected in checkboxes
  final_table_wide <- RV$sends[input$boxnames]
  #function to make each table wide
  spreadr<-function(x){spread(data = x, key = sample_name, value = normalized)}
  spreaded <- map(.x = final_table_wide, .f = spreadr)
  
  #for loop to combine tables and to get proper name for each column
  sfx <- names(spreaded)
  res <- spreaded[[1]]
  for(i in head(seq_along(spreaded), -1)) {
    
    res <- merge(res, spreaded[[i+1]], all = TRUE, 
                 suffixes = sfx[i:(i+1)], by = c("nm"))
  }
  
  res
})

output$table5 <- renderDT({
  req(finale_long())
  validate(need(length(input$boxnames) >=2, "Please select at least two samples to merge data"))
   DT::datatable(finale_long(),
                 options = list(lengthMenu = c(20, 50, 100, 150, 200), searching = TRUE))
})

output$table6 <- renderDT({
  req(finale_wide())
  validate(need(length(input$boxnames) >=2, "Please select at least two replicates to merge data"))
  DT::datatable(finale_wide(),
                options = list(lengthMenu = c(20, 50, 100, 150, 200), searching = TRUE))
})

output$downloadfinallong <- downloadHandler(
  filename = function() {
    paste("finallong", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(x = finale_long(), file = file, row.names = FALSE)
  }
)  

output$downloadfinalwide <- downloadHandler(
  filename = function() {
    paste("finalwide", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(x = finale_wide(), file = file, row.names = FALSE)
  }
)

}
shinyApp(ui = ui, server = server)