# -------------- Set-up -------------- #

# libraries
library(readxl)         # import export
library(kableExtra)     # table formatting
library(data.table)     # setnames
library(shiny)          # shiny
library(shinyWidgets)   # toggle materialSwitch
library(colourpicker)   # user selected color
library(tidyverse)      # general wrangling

# current dates
this_date  <- unlist(str_split(Sys.Date(), pattern = "-"))
this_year  <- this_date[1]
this_month <- this_date[2]

# -------------- Define UI -------------- #

ui <- fluidPage(

    # Application title
    titlePanel("splitwiseR: visualize household spending data with Splitwise"),

    # Sidebar with application options
    sidebarPanel(
        fileInput("upload", "Upload Exported csv from Splitwise",
                  accept = c(".csv")
        ),
        
        selectInput("date", 
                    strong("Date range"),
                    choices = c("Month to date", "Custom date"),
                    selected = "Month to date"),
        tabsetPanel(
            id   = "date_selector",
            type = "hidden",
            tabPanelBody(
                "Month to date",
                strong(paste0("Month to Date: From ", this_year, "-", this_month, "-01 ",
                       "to ", Sys.Date(), "\n"))), 
            tabPanelBody(
                "Custom date",
                fluidRow(
                    column(6,
                           dateInput("start", "Start Date:", value = "2021-08-01")),
                    column(6,
                           dateInput("end", "End Date:", value = "2021-08-31"))))
        ),
        
        hr(),
        
        pickerInput("cat", 
                    "Which category to plot?",
                    multiple = TRUE,
                    options  = list(`actions-box` = TRUE),
                    choices  = c("Groceries", "Household supplies", "Home"),
                    selected = c("Groceries", "Household supplies", "Home")
        ),

        fluidRow(
            column(6,
                   colourInput("budget_green", "Below budget", 
                               value = "green")),
            column(6,
                   colourInput("budget_red", "Above budget", 
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

server <- function(input, output, session) {
    
    # observe custom_date option
    observeEvent(input$date, {
        updateTabsetPanel(session,
                          "date_selector",
                          selected = input$date)
    })

    # extract date range
    dates <- reactive({
        if(input$date == "Custom date") {
            
            # custom date range
            start <- input$start
            end   <- input$end
            
        } else {
            # month to date
            start <- as.Date(paste0(this_year, "-", this_month, "-01"))
            end   <- Sys.Date()
        }
        return(list(end   = end,
                    start = start))
    })

    # extract information from data files
    data_list <- reactive({
        
        # default test_data.csv if no uploaded files
        if(!is.null(input$upload)) {
            data <- read.csv(input$upload$datapath)
        } else {
            data <- read.csv(file = "test_data.csv")
        }
        
        # initial clean of data file
        data  <- data %>% 
            
            # format variables 
            mutate(Date = as.Date(Date, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
                   Category = as.factor(Category)) %>% 

            # delete "total balance" row and "currency" column
            filter(Description != "Total balance") %>% 
            select(-Currency) %>% 

            # selected date from uploaded data file
            filter(Date >= dates()$start & Date <= dates()$end)
        
        # pull out people's names
        names <- colnames(data)[5:ncol(data)]
        
        # 0/1 dummy code for each person
        # 1 = spender
        data  <- data %>% 
            mutate(across(all_of(names),
                          ~as.numeric(. > 0)))
        
        # pull out categories
        cat   <- sort(as.character(unique(data$Category)))
        
        return(list(data  = data,
                    names = names,
                    cat   = cat))
    })
    
    # pull out reactive variables
    data  <- reactive(data_list()$data)
    names <- reactive(data_list()$names)
    cat   <- reactive(data_list()$cat)
    
    # observe category options
    observeEvent(cat(), {
        updatePickerInput(session,
                          "cat",
                          choices = cat(),
                          selected = cat())
    })

    # TODO: add budgets for benchmarks
            
    # bar chart data
    data_bar <- reactive({
        aggregate(data()$Cost, by = list(data()$Category), 
                  FUN = function(x) sum(x, na.rm = T)) %>% 
            as.data.frame() %>% 
            rename(Category = Group.1,
                   Cost     = x) %>% 
            filter(Category %in% input$cat)
    })
    
    # bar chart
    p_bar <- reactive({
        p_bar <- ggplot(data = data_bar(),
                        aes(x = reorder(Category, Cost), 
                            y = Cost)) +
            geom_col() +
            theme_classic() +
            theme(plot.title   = element_text(size = 18, face = "bold"),
                  axis.title.x = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 16, face = "bold"),
                  axis.text.x  = element_text(size = 14),
                  axis.text.y  = element_text(size = 14)) +
            labs(title = paste0("Expense reports from ", dates()$start, " to ", dates()$end, ", total = $",
                                sum(data_bar()$Cost)),
                 x     = "Category") +
            coord_flip() +
            geom_text(aes(label = Cost), hjust = -0.2)

        
        p_bar
    })
    
    # outputs
    output$plot    <- renderPlot(p_bar())
    output$summary <- renderDataTable(data_bar())
    output$data    <- renderDataTable(data())

}

# Run the application 
shinyApp(ui = ui, server = server)

