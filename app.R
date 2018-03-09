#
# This is a shiny web application. You can run this application and get an
# empty line to type in. The shiny app will give you suggestions for the
# next words you want to type
#

library(shiny)
source("predict2.R")

# define a simple UI with a textfield and buttons for predicted next words
ui <- fluidPage(
    tags$head(tags$script(
        'Shiny.addCustomMessageHandler("refocus",
        function(NULL) {
        document.getElementById("text").focus();
        });')),
        
    # Application title
    titlePanel("Predictive keyboard"),
    textInput(inputId = "text", label = "Start typing your sentence in english", value = "", width = 609),
    actionButton(inputId = "b1", label = "?", width = 200),
    actionButton(inputId = "b2", label = "?", width = 200),
    actionButton(inputId = "b3", label = "?", width = 200)
)

# define the server part for predicting words
server <- function(input, output, session) {
    updateText <- function(oldText, newWord, replace) {
        if (replace) {
            s <- oldText
            while(tolower(substr(s, nchar(s), nchar(s))) %in% c(letters, "'", "-"))
            { s <- substr(s, 1, nchar(s)-1) }
            if (substr(s, nchar(s), nchar(s)) != " " & s != "") {
                s <- paste0(s, " ")
            }
            if (newWord == ".") {
                s <- substr(s, 1, nchar(s)-1)
            }
            s <- paste0(s, newWord, " ")
        } else {
            s <- oldText
            if (substr(s, nchar(s), nchar(s)) != " " & s != "") {
                s <- paste0(s, " ")
            }
            if (newWord == ".") {
                s <- substr(s, 1, nchar(s)-1)
            }
            s <- paste0(s, newWord, " ")
        }
        s
    }
    
    updatePrediction <- function() {
        pre <- predict_words2(input$text)
        updateActionButton(session, "b1", label = pre$words[1])
        updateActionButton(session, "b2", label = pre$words[2])
        updateActionButton(session, "b3", label = pre$words[3])
        
        session$userData$b1 <- pre$words[1]
        session$userData$b2 <- pre$words[2]
        session$userData$b3 <- pre$words[3]
        session$userData$replace <- pre$replace
        session$sendCustomMessage(type="refocus",message=list(NULL))
    }

    observeEvent(input$text, {
        updatePrediction()
    })
    
    observeEvent(input$b1, {
        updateTextInput(session, "text", value = updateText(input$text,
                                                            session$userData$b1,
                                                            session$userData$replace))
        updatePrediction()
        session$sendCustomMessage(type="refocus",message=list(NULL))
    })
    
    observeEvent(input$b2, {
        updateTextInput(session, "text", value = updateText(input$text,
                                                            session$userData$b2,
                                                            session$userData$replace))
        updatePrediction()
        session$sendCustomMessage(type="refocus",message=list(NULL))
    })
    
    observeEvent(input$b3, {
        updateTextInput(session, "text", value = updateText(input$text,
                                                            session$userData$b3,
                                                            session$userData$replace))
        updatePrediction()
        session$sendCustomMessage(type="refocus",message=list(NULL))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
