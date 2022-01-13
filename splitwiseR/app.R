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

# custom budget
group_list  <- c("Entertainment", "Food", "Home", "Pets", "Expenses", "Utilities")
budget_list <- c(50, 400, 100, 100, 200, 1100)


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
        
        # more category selection
        selectInput("cat2", 
                    "More general category?",
                    choices  = c("No", "Yes"),
                    selected = c("No")
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
                             dataTableOutput("group"),
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
    # and specify date range for test_data
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
        # specific date range for test_data
        if(is.null(input$upload)){
            start <- "2021-08-01"
            end   <- "2021-09-01"
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
        
        # add larger categories
        if(input$cat2 == "Yes") {
            data1 <- data %>% 
                mutate(Group = ifelse(
                    Category %in% cat2()$Entertainment, "Entertainment",
                    ifelse(
                        Category %in% cat2()$Food, "Food",
                        ifelse(
                            Category %in% cat2()$Home, "Home",
                            ifelse(
                                Category %in% cat2()$Pets, "Pets",
                                ifelse(
                                    Category %in% cat2()$Expenses, "Expenses",
                                    ifelse(
                                        Category %in% cat2()$Health, "Health",
                                        ifelse(
                                            Category %in% cat2()$Transportation, "Transportation",
                                            ifelse(
                                                Category %in% cat2()$Utilities, "Utilities", NA
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
            ))
            data <- add_column(data, data1$Group, .after = "Category")
            data <- rename(data, Group = `data1$Group`)
        }
        
        # pull out people's names
        if(input$cat2 == "Yes") {
            names <- colnames(data)[6:ncol(data)]
        } else {
            names <- colnames(data)[5:ncol(data)]
        }
        
        # 0/1 dummy code for each person
        # 1 = spender
        # if only one person -> all = 1
        if (length(names) == 1) {
            data <- data %>% 
                mutate(across(all_of(names),
                              ~as.numeric(. == 0)))
        } else {
            data  <- data %>% 
                mutate(across(all_of(names),
                              ~as.numeric(. > 0)))
        }
        
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
    
    # additional category options
    cat2 <- reactive({
        if (input$cat2 == "Yes") {
            cat2 <- list(Entertainment = c("Games", "Movies", "Music", "Entertainment - Other", "Sports"),
                         Food = c("Dining out", "Groceries", "Liquor", "Food and drink - Other"),
                         Home = c("Electronics", "Furniture", "Household supplies", "Maintenance", "Home - Other", "Services"),
                         Pets = c("Pets"),
                         Expenses = c("Clothing", "Gifts", "Education", "Life - Other",
                                      "Insurance", "Medical expenses", "Bicycle", "Bus/train",
                                      "Car", "Gas/fuel", "Hotel", "Transportation - Other",
                                      "Parking", "Plane", "Taxi"),
                         Utilities = c("Rent", "Cleaning", "Electricity", "Heat/gas", "Utilities - Other", 
                                       "Trash", "TV/Phone/Internet", "Water", "Mortgage")
            )
        }
        cat2
    })
    
    # render budget options
    output$custom_budget <- renderUI({
        if (input$cat2 == "Yes") {
            lapply(1:length(group_list), function(i) {
                numericInput(paste0(group_list[i]), label = paste0(group_list[i]), value = budget_list[i], step = 10)    
            })
        }
    })
    
    # observe budget options
    observeEvent(input$budget, {
        updateTabsetPanel(session,
                          "budget_selector",
                          selected = input$budget)
    })
    

    ###### DATA ###### 
    
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
        data_bar
    })

    # grouped chart data 
    data_group <- reactive({
        if(input$cat2 == "Yes"){    
            
            # add a larger category in original data frame
            data_group <- data() %>% 
                mutate(Group = ifelse(
                    Category %in% cat2()$Entertainment, "Entertainment",
                    ifelse(
                        Category %in% cat2()$Food, "Food",
                        ifelse(
                            Category %in% cat2()$Home, "Home",
                            ifelse(
                                Category %in% cat2()$Pets, "Pets",
                                ifelse(
                                    Category %in% cat2()$Expenses, "Expenses",
                                    ifelse(
                                        Category %in% cat2()$Utilities, "Utilities", NA
                                    )
                                )
                            )
                        )
                    )
                ))
            
            # aggregate by group
            data_group <- aggregate(data_group$Cost, by = list(data_group$Group), 
                                    FUN = function(x) sum(x, na.rm = T)) %>% 
                as.data.frame() %>% 
                rename(Group = Group.1,
                       Cost     = x)
            
            # add budget
            if (input$budget == "Custom budget") {
                group_list <- group_list[which(group_list %in% data_group$Group)]
                
                budget <- data.frame(lapply(1:length(group_list), function(i) {
                    input[[paste0(group_list[i])]]
                }))
                names(budget) <- group_list

                # to long
                budget <- reshape2::melt(budget)
                names(budget) <- c("Group", "Budget")
                
                # merge budget to data_bar
                data_group <- merge(data_group, budget) %>% 
                    
                    # add comparison with budget
                    mutate(Compare = ifelse(Cost > Budget,  "above budget",
                                            ifelse(Cost <= Budget, "within budget", NA))) %>% 
                    # change to factor
                    mutate(Compare = factor(Compare, levels = c("above budget",
                                                                "within budget")))
            }
        data_group
        } # END if input$cat2 == "Yes" STATEMENT
    }) # END data_group REACTIVE
     
    ###### VISUALIZATION ###### 
    
    # bar chart for category
    p_bar <- reactive({
        
        p_bar <- ggplot(data = data_bar() %>% 
                            # selected categories
                            filter(Category %in% input$cat),
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
            geom_text(aes(label = Cost), vjust = -0.5, size = 5)
        
        p_bar
    })
    
    # bar chart for larger group
    p_group <- reactive({
        if(input$cat2 == "Yes") {
            
            # fill budget color
            if(input$budget == "Custom budget") {
                p_group <- ggplot(data = data_group(),
                                  aes(x = reorder(Group, Cost), 
                                      y = Cost,
                                      fill = Compare)) +
                    geom_col() +
                    theme_classic() + 
                    scale_fill_manual(values = c(
                        "above budget"    = input$budget_red,
                        "within budget"   = input$budget_green))
                } else {
                    p_group <- ggplot(data = data_group(),
                                      aes(x = reorder(Group, Cost), 
                                          y = Cost)) +
                        geom_col() +
                        theme_classic()
                }
            p_group <- p_group + 
                theme(plot.title   = element_text(size = 18, face = "bold"),
                      axis.title.x = element_text(size = 16, face = "bold"),
                      axis.title.y = element_text(size = 16, face = "bold"),
                      axis.text.x  = element_text(size = 14),
                      axis.text.y  = element_text(size = 14)) +
                labs(title = paste0("Expense reports from ", dates()$start, " to ", dates()$end, ", total = $",
                                    sum(data_group()$Cost)),
                     x     = "Group") +
                geom_text(aes(label = Cost), vjust = -0.5, size = 5)
            p_group
        }
    })
    
    # outputs
    p <- reactive({
        if(input$cat2 == "Yes") {
            p <- p_group()
        } else {
            p <- p_bar()
        }
        p
    })
    
    output$plot <- renderPlot(p())
    output$group   <- renderDataTable(data_group())
    output$summary <- renderDataTable(data_bar() %>% 
                                          # selected categories
                                          filter(Category %in% input$cat))
    output$data    <- renderDataTable(data() %>% 
                                          # selected categories
                                          filter(Category %in% input$cat))
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("splitwiseR-", Sys.Date(),".xlsx")
        },
        content  = function(file) {
            wb <- createWorkbook()
            ws <- addWorksheet(wb, "Graph")
            plot(p())
            insertPlot(wb, sheet = ws, width = 12, height = 7)
            addWorksheet(wb, "Grouped data")
            writeData(wb, "Grouped data", data_group())
            addWorksheet(wb, "Splitwise data")
            writeData(wb, "Splitwise data", data_bar())
            addWorksheet(wb, "Itemized data")
            writeData(wb, "Itemized data", data())
            saveWorkbook(wb, file = file, overwrite = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

