#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gbm)
source("analysis.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Music Genre Classification App"),

    # Sidebar with a slider input for number of bins

    sidebarLayout(
        sidebarPanel(
            sliderInput("key",
                        "Key",
                        min = 0,
                        max = 11,
                        value = 0),
            column(4,
            textInput("dance", h5("Danceability"),
                             value = 0)),
            column(4,
            textInput("energy", h5("Energy"),
                      value = 0)),
            column(4,
            textInput("loudness", h5("Loudness"),
                      value= 0)),
            hr(),

            column(4,
                   textInput("speech", h5("Speechness"),
                             value= 0)),

            column(4,
                   textInput("instrument", h5("Instrumentalness"),
                             value= 0)),

            column(4,
                   textInput("acoustic", h5("Acousticness"),
                             value=0)),

            hr(),

            column(12, align ='center',
                   radioButtons("mode", label = h5("Mode"),
                                choices = list("0" = 0, "1" = 1),
                                selected = 1)),
            hr(),

                   textInput("valence", h5("Valence"),
                             value=0),
            hr(),

            sliderInput("time_signature",
                        "Time Signature",
                        min = 0,
                        max = 5,
                        value = 4),

            sliderInput("tempo",
                        "Tempo",
                        min = 0,
                        max = 250,
                        value = 80),

            sliderInput("duration",
                        "Duration (Sec)",
                        min = 0,
                        max = 995,
                        value = 450),

            column(6, align="center", offset = 3,
                   actionButton("submit","Submit")

                   )

        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("Display"),
            tags$head(tags$style("#Display{color: purple;
                                 font-size: 25px;
                                 font-style: bold;
                                 top: 500px;

                                 }")),
            hr(),
            hr(),

           textOutput("calculate"),
           tags$head(tags$style("#calculate{color: green;
                                 font-size: 50px;
                                 font-style: bold;
                                 align:center;
                                 text-align: center;
                             border: 1px solid green;
                                 top: 500px;

                                 }"
           )),
           hr(),
           textOutput("Close"),
           tags$head(tags$style("#Close{color: purple;
                                 font-size: 25px;
                                 font-style: bold;
                                 top: 500px;

                                 }")),
           hr(),
           column(12,align='center',
           tableOutput("closest"),
           tags$head(tags$style("#closest{
                                 font-size: 18px;
                                  align:center;
                                 font-style: bold;

                                 }"))),

           hr(),
           hr(),
           column(12,align='center',
           uiOutput("tab")),
           tags$head(tags$style("#tab{color: black;
                                 font-size: 18px;
                                 font-style: bold;
                                 top: 500px;

                                 }")),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$Display<-renderText({

        input$submit
        paste("The Predicted Genre:")
    })

    output$calculate<-renderText({
        input$submit

        id<-isolate({as.numeric(input$dance)})
        ie<-isolate({as.numeric(input$energy)})
        ii<-isolate({as.numeric(input$instrument)})
        im<-isolate({as.integer(input$mode)})
        idu<-isolate({as.integer(input$duration)})
        is<-isolate({as.numeric(input$speech)})
        il<-isolate({as.numeric(input$loudness)})
        ik<-isolate({as.integer(input$key)})
        ia<-isolate({as.numeric(input$acoustic)})
        iv<-isolate({as.numeric(input$valence)})
        it<-isolate({as.numeric(input$tempo)})
        itime<-isolate({as.numeric(input$time_signature)})


        new_data<-data.frame(
            Danceability = c (id),
            Energy = c (ie),
            Loudness = c (il),
            Acousticness = c (ia),
            Instrumentalness= c(ii),
            Speechness = c (is),
            Valence = c (iv),
            Tempo = c (it),
            Duration_ms = c (idu),
            Key = c (ik),
            Mode = c (im),
            time_signature=c(itime))
        print(new_data)
        print(str(new_data))




        rf_model <- readRDS("/Users/pranavmanjunath/Desktop/Duke/702/Final Project_Git/final-project-PranavM98/Code/App/final_model_boost_train.rds")
        probs<-predict(rf_model,new_data)
        labels = colnames(probs)[apply(probs, 1, which.max)]
        print("GENRE")
        print(labels)

        rec_data<-data_123(new_data,labels)
        write.csv(rec_data,"Reccomendation.csv")

        paste(labels)

    })

    output$Close<-renderText({

        paste("Similar Songs:")

    })

    output$closest<-renderTable({
        input$submit

        isolate({read.csv('Reccomendation.csv')})

},
align='c', striped=TRUE, digits=5)

    url <- a("Spotify", href="https://open.spotify.com/search")
    output$tab <- renderUI({
        tagList("Access Spotify Here:", url)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
