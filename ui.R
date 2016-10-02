library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Image compression via PCA"),

    mainPanel(
        h2("Original Image:"),
        textOutput("origSize"),
        plotOutput("defaultPlot"),
        numericInput("n",
                     "Number of singular vectors to include:",
                     min = 1, max = 128, value = 1, step = 1),
        textOutput("compressedSize"),
        plotOutput("compressedPlot")
    )
))
