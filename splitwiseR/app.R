# -------------- Set-up -------------- #

# libraries
library(readxl)         # import export
library(kableExtra)     # table formatting
library(data.table)     # setnames
library(shiny)          # shiny
library(shinyWidgets)   # toggle materialSwitch
library(colourpicker)   # user selected color
library(tidyverse)      # general wrangling


# -------------- Define UI -------------- #

ui <- fluidPage(

    # Application title
    titlePanel("splitwiseR: visualize household spending data with Splitwise"),

    # Sidebar with application options
    sidebarPanel(
        fileInput("upload", "Upload Exported csv from Splitwise",
                  accept = c(".csv")
        ),
        
        fluidRow(
            column(6,
                   dateInput("start", "Start Date:", value = "2021-08-01")),
            column(6,
                   dateInput("end", "End Date:", value = "2021-08-31"))
        ),
        
        fluidRow(
            column(6,
                   colourInput("highcolor", "above high", 
                               value = "yellow"),
                   colourInput("avgminuscolor", "above low", 
                               value = "green")),
            column(6,
                   colourInput("avgpluscolor", "above average", 
                               value = "red"),
                   colourInput("lowcolor", "below low", 
                               value = "brown"))
        )
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot", height = "700px"),
                             hover = "plot_hover"),
                    tabPanel("Data",
                             downloadButton("downloadData", "Download data"),
                             dataTableOutput("summary"),
                             dataTableOutput("data")),
                    tabPanel("Description"
                             #, includeHTML("manual.html")
                             )
        )
    )
)

# -------------- Server Logic -------------- #

server <- function(input, output) {
    
    # data file
    data <- reactive({
        
        # initial clean uploaded data file
        data <- read.csv(input$upload$datapath) %>% 
            
            # format variables 
            mutate(Date = as.Date(Date),
                   Category = as.factor(Category)) %>% 
            
            # delete "total balance" row and "currency" column
            filter(Description != "Total balance") %>% 
            select(-Currency) %>% 
            
            # selected date from uploaded data file
            filter(Date >= input$start & Date <= input$end) %>% 
            
            # 0/1 dummy code for each person
            # 1 = spender
            mutate(Linh.Nguyen = as.numeric(Linh.Nguyen > 0),
                   Justin.Cho  = as.numeric(Justin.Cho > 0))
            
                # TODO: add budgets for benchmarks

        data
    })
    
    # bar chart data
    data_bar <- reactive({
        aggregate(data()$Cost, by = list(data()$Category), 
                  FUN = function(x) sum(x, na.rm = T)) %>% 
            as.data.frame() %>% 
            rename(Category = Group.1,
                   Cost     = x)
    })
    
    # bar chart
    p_bar <- reactive({
        p_bar <- ggplot(data = data_bar(),
                        aes(x = reorder(Category, Cost), 
                            y = Cost)) +
            geom_col() +
            geom_text(aes(label = Cost), vjust = -0.2) +
            theme_classic() +
            theme(plot.title   = element_text(size = 18, face = "bold"),
                  axis.title.x = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 16, face = "bold"),
                  axis.text.x  = element_text(size = 14),
                  axis.text.y  = element_text(size = 14)) +
            labs(title = paste0("Expense reports for the selected date range, total = $",
                                sum(data_bar()$Cost)),
                 x     = "Category")
        
        p_bar
    })
    
    # outputs
    output$plot <- renderPlot(p_bar())
    output$summary <- renderDataTable(data_bar())
    output$data <- renderDataTable(data())

}

# Run the application 
shinyApp(ui = ui, server = server)
