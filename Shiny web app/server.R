library(shiny)
library(datasets)

Info <- mtcars
Info$am <- factor(Info$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
    
    newone <- reactive({
        paste("mpg ~", input$variable)
    })
    
    newonePoint <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
    sit <- reactive({
        lm(as.formula(newonePoint()), data=Info)
    })
    
    output$caption <- renderText({
        newone()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(newone()), 
                data = Info,
                outline = input$outliers)
    })
    
    output$sit <- renderPrint({
        summary(sit())
    })
    
    output$mpgPlot <- renderPlot({
        with(Info, {
            plot(as.formula(newonePoint()))
            abline(sit(), col=2)
        })
    })
    
})