source('utils.R')

library(shiny)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme('slate'),
    
    titlePanel('Exploring Air Pollution - Sulfate and Nitrate'),
    p('The air pollution levels of sulfate and nitrate were monitored in 332 locations across the United States. Please use the slider to choose the number of locations, then click on tabs below to explore and visualize the data.'),
    em('Data source - R Programming assignment by John Hopkins University on Coursera'),
    hr(),
    
    sliderInput(
        inputId = 'Number',
        label = paste0('Choose the number of locations (1 to ', max, '):'),
        min = min,
        max = max,
        value = default,
        width = '100%'
    ),
    
    tabsetPanel(
        tabPanel('Locations', tableOutput(outputId = 'locations')),
        tabPanel('Sulfate', plotOutput(outputId = 'sulfate')),
        tabPanel('Nitrate', plotOutput(outputId = 'nitrate')),
        tabPanel('Correlation', plotOutput(outputId = 'correlation'))
    )
)

server<- function(input, output) {
    load_data <- reactive({
        df <- get_data()
        return(df)
    })
    
    load_df <- function(size) {
        df <- load_data()
        # samples <- sample(1:max, size = size, replace = FALSE)
        # df <- (subset(df, df$ID %in% samples))
        df <- (subset(df, df$ID %in% 1:size))
        return (df)
    }

    group_df <- function(size) {
        df <- load_df(input$Number) %>%
            rename(`Location ID` = ID) %>%
            group_by(`Location ID`) %>%
            summarise(
                Observations = n(),
                `Sulfate (Mean)` = mean(sulfate),
                `Sulfate (SD)` = sd(sulfate),
                `Nitrate (Mean)` = mean(nitrate),
                `Nitrate (SD)` = sd(nitrate)
            ) %>%
            arrange(`Location ID`)
        
        return (df)
    }
    
    output$locations <- renderTable(
        group_df(input$Number),
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$sulfate <- renderPlot({
        df <- group_df(input$Number)
        x <- df[['Sulfate (Mean)']]

        hist(x, breaks = 'sturges',
            main = 'Sulfate Mean Levels',
            xlab = 'Means of Sulfate Level',
            ylab = 'Number of Locations'
        )
    })

    output$nitrate <- renderPlot({
        df <- group_df(input$Number)
        x <- df[['Nitrate (Mean)']]
        
        hist(x, breaks = 'sturges',
             main = 'Nitrate Mean Levels',
             xlab = 'Means of Nitrate Level',
             ylab = 'Number of Locations'
        )
    })
    
    output$correlation <- renderPlot({
        df <- group_df(input$Number)
        x <- df[['Sulfate (Mean)']]
        y <- df[['Nitrate (Mean)']]

        plot(x, y,
             main = 'Sulfate and Nitrate Mean Levels',
             xlab = 'Means of Sulfate Level',
             ylab = 'Means of Nitrate Level'
        )
        
        # abline(lm(y ~ x, data = df), col = 'blue')
        lines(lowess(x, y), col = 'blue')
    })
}

shinyApp(ui = ui, server = server)
