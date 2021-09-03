# -------------- Set-up -------------- #

# libraries
library(readxl)         # import data
library(openxlsx)       # export excel sheets
library(reshape2)       # long wide
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

# date for previous month
prev_month <- ifelse(as.numeric(this_month) != 1, as.numeric(this_month) - 1, 12)
prev_year  <- ifelse(as.numeric(this_month) != 1, this_year, as.numeric(this_year) - 1)
prev_date  <- ifelse(prev_month %in% c(1, 3, 5, 7, 8, 10, 12), 31,
              ifelse(prev_month %in% c(4, 6, 9, 11), 30,
              ifelse(prev_year %% 4 != 0, 28, 29)))

# -------------- Define UI -------------- #

ui <- fluidPage(

    # Application title
    titlePanel("splitwiseR: visualize household spending data with Splitwise"),

    # Sidebar with application options
    sidebarPanel(
        
        # data files
        fileInput("upload", "Upload Exported csv from Splitwise",
                  accept = c(".csv")
        ),
        
        # date selection
        selectInput("date", 
                    strong("Date range"),
                    choices = c("Month to date", "Year to date", "Custom date", "Custom month"),
                    selected = "Month to date"),
        tabsetPanel(
            id   = "date_selector",
            type = "hidden",
            tabPanelBody(
                "Month to date",
                strong(paste0("Month to Date: From ", this_year, "-", this_month, "-01 ",
                       "to ", Sys.Date(), "\n"))), 
            tabPanelBody(
                "Year to date",
                strong(paste0("Year to date: From ", this_year, "-01-01 ",
                       "to ", Sys.Date(), "\n"))), 
            tabPanelBody(
                "Custom date",
                fluidRow(
                    column(6,
                           dateInput("start", "Start Date:", 
                                     value = paste0(prev_year, "-", prev_month, "-01"))),
                    column(6,
                           dateInput("end", "End Date:", 
                                     value = paste0(prev_year, "-", prev_month, "-", prev_date))))),
            tabPanelBody(
                "Custom month",
                fluidRow(
                    column(6,
                           selectInput("month", "Month:", 
                                       choices = c("January" = "01", "February" = "02", "March" = "03",
                                                   "April" = "04", "May" = "05", "June" = "06", 
                                                   "July" = "07", "August" = "08", "September" = "09",
                                                   "October" = "10", "November" = "11", "December" = "12"),
                                       selected = this_month)),
                    column(6,
                           selectInput("year", "Year:", 
                                       choices = 2000:2100,
                                       selected = as.numeric(prev_year)))))
        ),
        
        hr(),
        
        # category selection
        pickerInput("cat", 
                    "Which category to plot?",
                    multiple = TRUE,
                    options  = list(`actions-box` = TRUE),
                    choices  = c("Groceries", "Household supplies", "Home"),
                    selected = c("Groceries", "Household supplies", "Home")
        ),
        
        # budget setting
        selectInput("budget",
                    strong("Set a budget?"),
                    choices = c("No", "Custom budget"),
                    selected = "No"),
        tabsetPanel(
            id = "budget_selector",
            type = "hidden",
            tabPanelBody(
                "No"
            ),
            tabPanelBody(
                "Custom budget",
                uiOutput("custom_budget")
            )
        ),
        
        # color selection
        fluidRow(
            column(6,
                   colourInput("budget_green", "Below budget", 
                               value = "#057a05")),
            column(6,
                   colourInput("budget_red", "Above budget", 
                               value = "#a52a2a"))
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
            
        } else if(input$date == "Month to date") {
            # month to date
            start <- as.Date(paste0(this_year, "-", this_month, "-01"))
            end   <- Sys.Date()
        
        } else if(input$date == "Year to date") {
            # year to date
            start <- as.Date(paste0(this_year, "-01-01"))
            end   <- Sys.Date()
            
        } else if(input$date == "Custom month") {
            #custom month
            start    <- as.Date(paste0(input$year, "-", input$month, "-01"))
            end_date <- ifelse(as.numeric(input$month) %in% c(1, 3, 5, 7, 8, 10, 12), 31,
                               ifelse(as.numeric(input$month) %in% c(4, 6, 9, 11), 30,
                                      ifelse(as.numeric(input$year) %% 4 != 0, 28, 29)))
            end      <- as.Date(paste0(input$year, "-", input$month, "-", end_date))
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

    # render budget options
    output$custom_budget <- renderUI({
        lapply(1:length(input$cat), function(i) {
            numericInput(paste0(input$cat[i]), label = paste0(input$cat[i]), value = 200, step = 10)
        })
    })
    
    # observe budget options
    observeEvent(input$budget, {
        updateTabsetPanel(session,
                          "budget_selector",
                          selected = input$budget)
    })
    
    # add budget
    budget <- reactive({
        if (input$budget == "Custom budget") {
            budget <- data.frame(lapply(1:length(input$cat), function(i) {
                input[[paste0(input$cat[i])]]
                }))
            names(budget) <- input$cat
            
            # to long
            budget <- reshape2::melt(budget)
            names(budget) <- c("Category", "Budget")
            budget
        }
    })

    # bar chart data
    data_bar <- reactive({
        
        # summary by category
        data_bar <- aggregate(data()$Cost, by = list(data()$Category), 
                              FUN = function(x) sum(x, na.rm = T)) %>% 
            as.data.frame() %>% 
            rename(Category = Group.1,
                   Cost     = x)
        
        # summary by person and category
        for(i in seq_len(length(names()))) {
            data_person <- data()[which(data()[, names()[i]] == 1), ]
            data_person  <- aggregate(data_person$Cost, 
                                      by = list(data_person$Category),
                                      FUN = function(x) sum(x, na.rm = T)) %>% 
                as.data.frame()
            
            # rename
            names(data_person) <- c("Category", names()[i])
            
            # merge with data_bar
            data_bar <- merge(data_bar, data_person, all = TRUE)
            data_bar[is.na(data_bar)] <- 0
        }

        # add budget
        if (input$budget == "Custom budget") {
            
            # merge budget to data_bar
            data_bar <- merge(data_bar, budget()) %>% 
            
            # add comparison with budget
            mutate(Compare = ifelse(Cost > Budget,  "above budget",
                             ifelse(Cost <= Budget, "within budget", NA))) %>% 
                
            # change to factor
            mutate(Compare = factor(Compare, levels = c("above budget",
                                                        "within budget")))
        }

        data_bar
    })
    

    # bar chart
    p_bar <- reactive({
        
        # fill for budget
        if(input$budget == "Custom budget") {
            p_bar <- ggplot(data = data_bar() %>% 
                                # selected categories
                                filter(Category %in% input$cat),
                            aes(x = reorder(Category, Cost), 
                                y = Cost,
                                fill = Compare)) +
                geom_col() +
                theme_classic() + 
                scale_fill_manual(values = c(
                    "above budget"    = input$budget_red,
                    "within budget"   = input$budget_green))
        } else {
            p_bar <- ggplot(data = data_bar() %>% 
                                # selected categories
                                filter(Category %in% input$cat),
                            aes(x = reorder(Category, Cost), 
                                y = Cost)) +
                geom_col() +
                theme_classic()
        } # END if else STATEMENT
        
        p_bar <- p_bar + 
            theme(plot.title   = element_text(size = 18, face = "bold"),
                  axis.title.x = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 16, face = "bold"),
                  axis.text.x  = element_text(size = 14),
                  axis.text.y  = element_text(size = 14)) +
            labs(title = paste0("Expense reports from ", dates()$start, " to ", dates()$end, ", total = $",
                                sum(data_bar()$Cost)),
                 x     = "Category") +
            geom_text(aes(label = Cost), vjust = -0.5, size = 5)
        
        p_bar
    })
    
    # outputs
    output$plot    <- renderPlot(p_bar())
    output$summary <- renderDataTable(data_bar())
    output$data    <- renderDataTable(data() %>% 
                                          # selected categories
                                          filter(Category %in% input$cat))
    output$downloadData <- downloadHandler(
        filename = "splitwiseR.xlsx",
        content  = function(file) {
            wb <- createWorkbook()
            ws <- addWorksheet(wb, "Graph")
            plot(p_bar())
            insertPlot(wb, sheet = ws, width = 12, height = 7)
            addWorksheet(wb, "Grouped data")
            writeData(wb, "Grouped data", data_bar())
            addWorksheet(wb, "Itemized data")
            writeData(wb, "Itemized data", data())
            saveWorkbook(wb, file = file, overwrite = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

