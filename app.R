library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

json <- httr::GET("https://api.covid19api.com/country/canada/status/confirmed")
covid_canada <- rawToChar(json$content) 
df <- fromJSON(covid_canada)
df$month<- strftime(df$Date,"%m")
df$year<- strftime(df$Date,"%y")
df$year <- as.numeric(df$year)
df$year <- df$year + 2000
df$month <- as.factor(df$month)

df$month_abr <-  month.abb[df$month]
df$month_abr <- as.factor(df$month_abr)
df$month_abr <- factor(df$month_abr, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                "Aug", "Sep", "Oct", "Nov", "Dec"))



df <- df %>%
    filter(Province != "") %>%
    filter(Province != "Grand Princess") %>%
    select(-CountryCode, -City, -CityCode, -Status, -month)

#-----------------------------------

ui <- fluidPage(
    titlePanel("Shiny Dashboard of Covid Cases in Canada"),
    mainPanel(
      p("This shiny app depicts total cases of covid in Canada. Data is requested from the API: (covid19api.com).
      The data and corresponding data visualization will update dynamically. 
      On this dashboard, you can filter data by province, month, and year. 
      There are graphs that visualize covid cases in Canada throughout 2020 and 2021. Graphs show covid cases by month and
      can be filtered by province.")),
    
    
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(5,
               selectInput("Province",
                           "Province:",
                           c("All",
                             unique(as.character(df$Province))))
        ),
        column(5,
               selectInput("month_abr",
                           "Month:",
                           c("All",
                             unique(as.character(df$month_abr))))
        ),
        column(5,
               selectInput("year",
                           "Year:",
                           c("All",
                             unique(as.character(df$year))))
        )
               
  
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table"),
    downloadButton('downloadData', 'Download data'),
    
    plotOutput("plot1", hover = "plot_hover1"),
    verbatimTextOutput("info1"),
    plotOutput("plot2", hover = "plot_hover2"),
    verbatimTextOutput("info2")
    


)


server <- function(input, output) {   # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- df
        if (input$Province != "All") {
            data <- data[data$Province == input$Province,]
        }
        if (input$month_abr != "All") {
            data <- data[data$month_abr == input$month_abr,]
        }
        if (input$year != "All") {
            data <- data[data$year == input$year,]
        }
        data
    })) 
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("dataset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(df, file)
        })
    
    output$plot1 <- renderPlot({
      
      if (input$Province != "All") {
        df <- df[df$Province == input$Province,]
      }
      
      
      df %>%
        filter(year == 2020) %>% 
        group_by(Province, month_abr) %>%
        filter(Cases == max(Cases)) %>%
        ggplot(aes(x = month_abr, y = Cases)) + 
        geom_line(aes(group = Province, color = Province)) + 
        xlab("Month") + ylab("Total Cases") + 
        ggtitle("Total Cases by Province in 2020") })
 
    output$info1 <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
      }
      
      paste0("hover: ", xy_str(input$plot_hover1)
      )
    })   

    output$plot2 <- renderPlot({
      
      if (input$Province != "All") {
        df <- df[df$Province == input$Province,]
      }
      
      
      df %>%
        filter(year == 2021) %>% 
        group_by(Province, month_abr) %>%
        filter(Cases == max(Cases)) %>%
        ggplot(aes(x = month_abr, y = Cases)) + 
        geom_point(aes(group = Province, color = Province)) + 
        geom_line(aes(group = Province)) +
        xlab("Month") + ylab("Total Cases") + 
        ggtitle("Total Cases by Province in 2021")  })
    
    
    
    output$info2 <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
      }
      
      paste0("hover: ", xy_str(input$plot_hover2)
      )
    })
    
      
}



# Run the application 
shinyApp(ui = ui, server = server)
