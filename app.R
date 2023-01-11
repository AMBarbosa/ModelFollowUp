library(shiny)
library(shinyjs)
library(DT)
library(fuzzySim)
library(modEvA)

#source("global.R")
#column_names <- names(dat)

source("functions.R")

left_width <- 4  # left column for inputs
right_width <- 8  # right column for outputs

#jsResetCode <- "shinyjs.reset = function() {history.go(0)}"  # https://stackoverflow.com/questions/25062422/restart-shiny-session


ui <- fluidPage(

    useShinyjs(),  # https://www.rdocumentation.org/packages/shinyjs/versions/2.0.0/topics/reset
    #extendShinyjs(text = jsResetCode),  # https://stackoverflow.com/questions/25062422/restart-shiny-session

    h1("Following up on species distribution models"),


    # Row 1: Load data ----

    fluidRow(
        column(left_width,

               actionButton("reset", "Reset data", icon = icon("power-off")),

               # Input: Data upload ----
               wellPanel(#style="background:lightblue",


                   fileInput(inputId = "file", label = "Upload data file",
                             multiple = FALSE,
                             #accept = c("text/csv",
                             #            "text/comma-separated-values,text/plain",
                             ##            "text/space-separated-values,text/plain",
                             #            ".csv")
                   ),

                   # Input: Checkbox if file has header ----
                   checkboxInput(inputId = "header", label = "Column names in first row", value = TRUE),

                   # Input: Select data separator ----
                   selectInput(inputId = "sep", label = "Column separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Space = " ",
                                           Tab = "\t"),
                               selected = ","),
                   selectInput(inputId = "dec", label = "Decimal separator",
                               choices = c(Comma = ",",
                                           Point = "."),
                               selected = "."),

               )
        ),

        # Output: Data head ----
        column(right_width,
               #wellPanel(#style="background:lightyellow",
               textOutput("data_table_text"),
               dataTableOutput("data_table")
               #)
        )
    ),  # end fluidRow 1


    # Row 2: Choose train and test columns ----

    fluidRow(

        # input: Train and test columns ----
        column(left_width,
               wellPanel(#style="background:lightyellow",
                   #selectInput('train', 'Column with model training data', choices = c("", column_names)),
                   #selectInput('test', 'Column with test ("future") data', choices = c("", column_names)),
                   uiOutput("ui_selectTrain"),
                   uiOutput("ui_selectTest"),
                   actionButton("plotRangeChangeButton", "Plot range change", icon = icon("chart-column"))
               )
        ),

        # Output: Range change plot ----
        column(right_width,
               textOutput("rangechange_text"),
               plotOutput("rangechange_plot"),
        )

    ),  # end fluidRow 2


    # Row 3: Choose pred column and threshold ----

    fluidRow(

        # Input: Pred column and threshold ----
        column(left_width,
               wellPanel(#style="background:lightgrey",
                   #selectInput('pred', 'Column with predicted values', choices = c("", column_names)),
                   uiOutput("ui_selectPred"),
                   selectInput('thresh', 'Classification threshold', choices = c("", modEvAmethods("getThreshold"), seq(0.1, 0.9, 0.1))),
                   actionButton("plotTrainTestEvalButton", "Plot model evaluation", icon = icon("chart-line"))
               )
        ),

        # Output: Model evaluation plots ----
        column(right_width,
               plotOutput("modEvalPlots")
        )
    ),  # end fluidRow 3


    # Row 4: Choose spatial parameters ----

    fluidRow(

        # Input: Coordinate columns and distance ----
        column(left_width,
               wellPanel(#style="background:lightred",
                   #selectInput('x', 'Column with X (longitude) coordinates', choices = c("", column_names)),
                   #selectInput('y', 'Column with Y (latitude) coordinates', choices = c("", column_names)),
                   uiOutput("ui_selectLon"),
                   uiOutput("ui_selectLat"),
                   numericInput('dist', 'Neighbour distance in map units', 1),
                   actionButton("plotMapButton", "Plot spatial coordinates", icon = icon("globe"))
               )
        ),

        # Output: Map and boxplots ----
        column(right_width,
               plotOutput("map"),
               plotOutput("boxplots"))
    )  # end fluidrow 4

)  # end fluidPage


server <- function(input, output, session) {

    # observeEvent(input$reset, {
    #   dat() <- NULL
    # })


    #observeEvent(input$file, {js$reset()})  # https://stackoverflow.com/questions/25062422/restart-shiny-session


    # create 'dat' object from 'file' input

    dat <- eventReactive(input$file, {
        reset("selectTrain")  # https://www.rdocumentation.org/packages/shinyjs/versions/2.0.0/topics/reset
        reset( "selectTest")
        reset("selectPred")
        reset("selectLon")
        reset("selectLat")
        reset("plotRangeChangeButton")
        reset("plotTrainTestEvalButton")
        reset("plotMapButton")

        file <- input$file
        file_ext <- tools::file_ext(input$file)
        validate(need(file_ext %in% c("csv", "txt"), "You can upload your data in .txt or .csv format."))
        read.csv(input$file$datapath,
                 header = input$header,
                 sep = input$sep)
    })

    output$ui_selectTrain <- renderUI({
        selectInput("selectTrain",
                    "Choose column with training data",
                    choices = c("", names(dat())))
    })

    output$ui_selectTest <- renderUI({
        selectInput("selectTest",
                    "Choose column with test ('future') data",
                    choices = c("", names(dat())))
    })

    output$ui_selectPred <- renderUI({
        selectInput("selectPred",
                    "Choose column with predicted values",
                    choices = c("", names(dat())))
    })

    output$ui_selectLon <- renderUI({
        selectInput("selectLon",
                    "Choose column with X (longitude) coordinates",
                    choices = c("", names(dat())))
    })

    output$ui_selectLat <- renderUI({
        selectInput("selectLat",
                    "Choose column with Y (latitude) coordinates",
                    choices = c("", names(dat())))
    })


    # OUTPUT: Show data ----
    output$data_table_text <- renderText({
        req(input$file)
        "This is what your data look like:"
    })

    #output$column_names <- names(dat)

    output$data_table <- renderDataTable({
        # file_ext <- tools::file_ext(input$file)
        # validate(need(file_ext %in% c("csv", "txt"), "You can upload your data in .txt or .csv format."))
        # dat <- read.csv(input$file$datapath,
        #                 header = input$header,
        #                 sep = input$sep)
        dat()
    })


    # # OUTPUT: Limit choices for input columns ----
    # output$ui_test_column <- renderUI({
    #   choices_full <- names(dat)
    #   choices_left <- subset(choices_full, !choices_full %in% input$selectTrain)
    #   selectInput("input_test", "Column with test ('future') data", choices = c("", choices_left))
    # })
    #
    # output$ui_pred_column <- renderUI({
    #   choices_full <- names(dat)
    #   choices_left <- subset(choices_full, !choices_full %in% c(input$selectTrain, input$input_test))
    #   selectInput("input_pred", "Column with test ('future') data", choices = c("", choices_left))
    # })


    # OUTPUT: Plot range change ----

    observeEvent(input$plotRangeChangeButton, {

        req(input$plotRangeChangeButton)

        output$rangechange_text <- renderText({
            req(input$selectTrain != "", input$selectTest != "")
            "Range change from training to test data:"
        })


        output$rangechange_plot <- renderPlot({

            req(input$plotRangeChangeButton,
                input$selectTrain,
                input$selectTest)

            #validate(need(isolate(input$selectTrain) != "" & isolate(input$selectTest) != "", "Your range change plot will appear here if you specify the columns containing the training and test data."))

            #observe({
            #  updateSelectInput(session, inputId = "train", choices = colnames(dat))
            #  updateSelectInput(session, inputId = "test", choices = colnames(dat))
            #})

            #req(input$selectTrain, input$selectTest)
            fuzzyRangeChange(dat()[ , isolate(input$selectTrain)], dat()[ , isolate(input$selectTest)])
        })
    })


    # OUTPUT: Plot model evaluation metrics ----
    observeEvent(input$plotTrainTestEvalButton, {

        req(input$plotTrainTestEvalButton)

        output$modEvalPlots <- renderPlot({

            #validate(need((isolate(input$selectTrain) != "" | isolate(input$selectTest) != "") & isolate(input$selectPred) != "", "Your model evaluation plots will appear here if you specify the columns containing train and/or test data and predicted values."))

            #AUC(obs = dat()[ , isolate(input$selectTest)], pred = dat()[ , isolate(input$selectPred)], simplif = TRUE, plot = TRUE)

            print(head(dat()))
            print(dat()[ ,input$selectTrain])
            print(input$selectTrain)

            trainTestEval(dat(), isolate(input$selectTrain), isolate(input$selectTest), isolate(input$selectPred), thresh = isolate(input$thresh))
        })
    })


    # OUTPUT: Map ----
    observeEvent(input$plotMapButton, {

        observeEvent(input$plotMapButton, {
            output$map <- renderPlot({

                #validate(need(isolate(input$selectLon) != "" & isolate(input$selectLat) != "", "Your data will be mapped here if you specify X and Y coordinate columns."))

                req(isolate(input$selectLon), isolate(input$selectLat))

                plot(dat()[ , isolate(input$selectLon)], dat()[ , isolate(input$selectLat)], pch = 20, xlab = "X coordinate", ylab = "Y coordinate")
            })
        })
    })
}

# Create Shiny app ----
shinyApp(ui, server)

