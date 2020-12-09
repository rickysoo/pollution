source('utils.R')

library(shiny)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme('slate'),
    
    titlePanel('Exploring Air Pollution - Sulfate and Nitrate'),
    p('The levels of sulfate and nitrate were reported in 332 locations across the United States. Use the sliders below to explore and visualize the data.'),
    p('GitHub - ', a('https://github.com/rickysoo/pollution', href = 'https://github.com/rickysoo/pollution')),
    hr(),
    
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = 'Number',
                label = paste0('Choose the number of locations (1 to ', max, '):'),
                min = min,
                max = max,
                value = default,
                width = '100%'
            ),
        
            sliderInput(
                inputId = 'Threshold',
                label = 'Use only locations with minimum number of reports:',
                min = 0,
                max = 1000,
                value = 0,
                width = '100%'
            )
        ),
    
        mainPanel(    
            tabsetPanel(
                tabPanel('Locations', tableOutput(outputId = 'locations')),
                tabPanel('Pollutants', plotOutput(outputId = 'pollutants')),
                tabPanel('Distributions', plotOutput(outputId = 'distributions')),
                tabPanel('Correlation', plotOutput(outputId = 'correlation'))
            )
        )
    )
)

server<- function(input, output, session) {
    load_data <- reactive({
        df <- get_data() %>%
            rename(`Location ID` = ID) %>%
            group_by(`Location ID`) %>%
            summarise(
                Reports = n(),
                `Sulfate` = mean(sulfate),
                `Nitrate` = mean(nitrate)
            ) %>%
            arrange(`Location ID`)

        return(df)
    })
    
    get_locations <- function(size, threshold = 100) {
        df <- load_data() %>%
            filter(`Location ID` <= size & Reports >= threshold)
        return (df)
    }
    
    output$locations <- renderTable(
        get_locations(input$Number, input$Threshold),
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$pollutants <- renderPlot({
        df <- get_locations(input$Number, input$Threshold)

        par(mfrow = c(1, 2))
        
        hist(df$Sulfate, breaks = 'sturges',
             main = 'Sulfate Mean Levels',
             xlab = 'Means of Sulfate Level',
             ylab = 'Number of Locations'
        )
    
        hist(df$Nitrate, breaks = 'sturges',
             main = 'Nitrate Mean Levels',
             xlab = 'Means of Nitrate Level',
             ylab = 'Number of Locations'
        )
    })
    
    output$distributions <- renderPlot({
        df <- get_locations(input$Number, input$Threshold)
        x <- df$Sulfate
        y <- df$Nitrate
        
        boxplot(y, x,
            horizontal = TRUE,
            names = c('Nitrate', 'Sulfate'),
            main = 'Comparison of Sulfate and Nitrate Mean Levels',
            xlab = 'Means of Pollutant Level'
        )
    })
    
    output$correlation <- renderPlot({
        df <- get_locations(input$Number, input$Threshold)
        x <- df$Sulfate
        y <- df$Nitrate

        plot(x, y,
             main = 'Sulfate and Nitrate Mean Levels',
             xlab = 'Means of Sulfate Level',
             ylab = 'Means of Nitrate Level'
        )
        
        lines(lowess(x, y), col = 'blue')
    })
}

shinyApp(ui = ui, server = server)
