# Load Packages----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(dplyr)
library(seewave)
library(tuneR)
library(howler)
library(torch)
library(torchvision)
library(tibble)


# Load additional dependencies and setup functions ----
source("neuralNet.R")
# Load in confusion matrices
load("cmtibble.RData")

transform <- function(x) x %>% 
  torch_tensor()
dir <- "./mnist"

mnistTrain <- mnist_dataset(
  root = dir,
  train = TRUE,
  download = TRUE,
  transform = transform
)


trainNNData <- read.csv(file = "trainNNData.csv", header = TRUE)
trainNNData <- na.omit(trainNNData)
testNNData <- read.csv(file = "testNNData.csv", header = TRUE)
binaryTestData <- testNNData %>%
  filter(digit == 0 | digit == 1) 
speakers <- c("george", "jackson", "lucas", "nicolas", "theo", "yweweler")


generateNumbersPlot <- function() {
  par(mfrow = c(3, 3))
  for (iter in 1:9) {
    i <- sample(1:length(mnistTrain), 1)
    x <- mnistTrain$data[i, , ] %>% t
    image(
      x[1:28, 28:1],
      useRaster = TRUE,
      axes = FALSE,
      col = gray.colors(1000),
      main = mnistTrain$targets[i] - 1
    )
  }
}

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "green",
    ### Create the app header ----
    dashboardHeader(
      title = "Neural Networks", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Classification_Using_Neural_Networks")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Examples", tabName = "example", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Neural Network Intro"), 
          p("This application will help show you the structure of a neural network,
            the various nodes involved and different activation functions for 
            different situations. In the app there is an example dealing with a 
            hospital, showcasing binary and multinomial classification. Look to 
            the instructions to learn more."),
          h2("Instructions"),
          p("Review the instructions below."),
          tags$ol(
            tags$li("Review the prerequiste information and ideas using the
                    Prerequistes tab."),
            tags$li("Explore situations in Exploration tab."),
            tags$li("Head to the Examples tab to see a neural networks in action!
                    Dealing with examples of classification in a post office.
                    See how layers and epochs affect the accuracy.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Robert Chappell,
            special thanks to Neil Hatfield for help throughout.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/30/2023 by RWC.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Hidden Layers -- Hidden layers play a crucial role as
                    intermediary layers between input and output layers. They
                    enable the model to capture complex patterns and relationships
                    within the data, improving its ability to make accurate
                    predictions."),
            tags$li("Epochs -- Epochs refer to the number of times a model
                    processes the entire training dataset. While more epochs
                    allow the model to refine its learning. Too many can lead to
                    overfitting.")
          ),
          tags$strong("Neural Network Structure"),
          p(
            "In the diagram below you can see the general structure of a neural
            network. Click on nodes to learn more about what their purpose is."),
            div(
              id = "centeredBox",
              style = "text-align: center;",
              width = "200%",
              neuralNet
              ,
              tags$script(HTML(
                "document.getElementById('neuralNet').focus();
      $('#objects').on('click', '.highlightNode', (ev) => {
        Shiny.setInputValue('clickedElement', ev.target.id);
      })"
              )),
      tags$script(HTML(
        "document.getElementById('neuralNet').focus();
      $('#objects').on('click', '.highlightArrow', (ev) => {
        Shiny.setInputValue('clickedElement', ev.target.parentNode.parentNode.id);
      })"
      )),
          ),
      textOutput("displayText"),
      br(),
      strong("Overfitting"),
            p(
            "Overfitting in machine learning occurs when a model learns the
            training data too closely, including its noise and random variations.
            This can lead to poor performance on new, unseen data because the
            model becomes too specialized. To avoid overfitting, it's essential
            to strike a balance in the complexity of the model. A model that is 
            too complex may perform well on the training data but struggle to
            generalize to real-world situations.")
          
        ),
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          tabsetPanel(
            tabPanel(
              title = "Writing Recognition",
              withMathJax(),
              br(),
              h2("MNIST Dataset"),
              p("An example of uses for Neural Network is with recognizing writing,
                you can use them in scanners at the post office, or copying words from an image.
                In this dataset we have diferent handwriting styles for each digit (0-9).
                Look at the similarities and explore the data!"),
                         actionButton(
                           inputId = "makenum", 
                           label = "Show Data!"
                         ),
              plotOutput("numbersPlot")
            ),
        tabPanel(
          title = "Speech Recognition",
          withMathJax(),
          br(),
          h2("Voice Recording Dataset"),
          p("Another great real-world application is speech recognition, we see 
            this everywhere in life today. From a hospital elevator with no buttons,
            or even with any digital assistant on your phone! Look for similarites
            between different takes in this dataset."),
          br(),
          p("This dataset being used contains 3,000 recordings (50 of each digit
        per speaker). The digits are 0-9, and all speakers are male, with 
        English pronunciation."),
        p("Explore the dataset below looking at the waveforms for the various
        audio files, and take a listen!"),
        fluidRow(
          column(width = 6,
                 wellPanel(
                   selectInput(
                     inputId = "selectedDigit",
                     label = "Select Digit:",
                     choices = c("All", 0:9),
                     selected = "All"
                   ),
                   selectInput(
                     inputId = "selectedSpeaker",
                     label = "Select Speaker:",
                     choices = c("All", speakers),
                     selected = "All"
                   ),
                   actionButton(
                     inputId = "select", 
                     label = "Select Random Wave File"
                   )
                 ),
                 tags$strong("To listen to audio click the play button!"),
                 br(),
                 howler(elementId = "sound", tracks = trainNNData$filedir),
                 howlerPlayPauseButton("sound")
          ),
          column(width = 6,
                 plotOutput("waveform")
          )
        )
        )
          )
        
        ),
        #### Set up the Examples Page ----
        tabItem(
          tabName = "example",
          withMathJax(),
          h2("Classification Example"),
              br(),
              p("Let's look at a practical use of a neural network performing
                multinomial classification. You are working at your post office,
                you go through thousands of envelopes a day and need a quicker
                way to read the zip codes, lets look at how a neural network
                performs in this situation."),
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = "nLayer",
                      label = "Number of Hidden Layers in Neural Network:",
                      min = 0, 
                      max = 3,
                      value = 1, 
                      step = 1
                    ),
                    sliderTextInput(
                      inputId = "nEpochs",
                      label = "Number of Epochs in Neural Network:", 
                      choices = c(1, 5, 10),
                      selected = 5,
                      grid = TRUE
                    
                    ),
                    br(),
                    actionButton(
                      inputId = "simulateMulti",
                      label = "Simulate with test data"
                    )
                  )
                ),
                column(
                  width = 8,
                  verbatimTextOutput(
                    outputId = "cmTable"
                  )
                )
              ),
              br(),
          tags$strong("Effect of Epochs:"),
          p("How does adjusting the number of epochs influence the performance
            of the neural network in the classification task, and what insights
            can be gained about the trade-off between underfitting and overfitting?"),
          
          tags$strong("Number of Layers Impact:"),
          p("Explore the impact of varying the number of hidden layers on the 
            confusion matrix. What patterns emerge, and how does the complexity
            introduced by additional layers affect the model's ability to
            generalize?"),
          
          tags$strong("Overfitting Concerns:"),
          p("Consider the possibility of overfitting in the context of the 
            confusion matrices. What signs of overfitting can be observed, and
            how does this influence the neural network's ability to handle
            unseen data?"),
          
          tags$strong("Identifying Network Struggles:"),
          p("Examine specific entries in the confusion matrix where the neural
            network tends to struggle. What can be inferred about the
            the data that pose challenges to the model, are there identifiable
            patterns in these struggles?")
          )
        ,
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Baldry, A. and Simpson, J. (2022). howler: 'Shiny' Extension of 'howler.js'.
            (version 0.2.1). [R package] Available from
            <https://CRAN.R-project.org/package=howler>."
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield., N. J. (2023). boastUtils: BOAST utilities.
            (v0.1.11.2). [R Package]. Available from
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create dashboards
            with 'Shiny'. (v0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Sueur, J., Aubin, T., and Simonis, C. (2022). seewave: A free modular
            tool for sound analysis and synthesis. (v2.2.0). [R Package]. Available
            from https://www.tandfonline.com/doi/abs/10.1080/09524622.2008.9753600"
          ),
          p(
            class = "hangingindent",
            "Uwe, L., Sebastian, K., Olaf, M., and Sarah, S. (2023). tuneR:
            Analysis of Music and Speech. (v1.4.4). [R Package].
            Available from https://CRAN.R-project.org/package=tuneR"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            (v3.4.2). [R Package]. New York:Springer-Verlag. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. François, R. Henry, L. Müller, K. and Vaughan, D. (2023).
            dplyr: A Grammar of Data Manipulation. (v1.1.2). [R package].
            Available from <https://CRAN.R-project.org/package=dplyr>."
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  #### Set up SVG ----
  displayedText <- reactiveVal("")
  
  # Observe click events on nodes or arrows
  observeEvent(
    eventExpr = input$clickedElement, 
    handlerExpr = {
    # Get the ID of the clicked element
    clickedId <- input$clickedElement
    
    if (substr(clickedId, 1, 5) == "input") {
      displayedText("The input layer nodes receive the raw input data.
                    Each node corresponds to a specific feature/variable,
                    and the entire input layer serves as the initial data
                    representation for the neural network.")
    }
    else if (substr(clickedId, 1, 5) == "node1") {
      displayedText("Each node in the first hidden layer performs a weighted
                    summation of its inputs received from the input layer. It
                    then applies an activation function to produce an output
                    value, introducing non-linearity to the neural network.")
    } 
    else if (substr(clickedId, 1, 5) == "node2") {
      displayedText("Similar to the first hidden layer, each node in the second
                    hidden layer performs a weighted summation of its inputs and
                    applies an activation function to generate an output")
    } 
    else if (clickedId == "output") {
      displayedText("The output node receives the weighted inputs from the
                    second hidden layer and performs its own weighted summation.
                    Then, an appropriate activation function is applied to
                    produce the final output of the neural network, representing
                    the predicted class or category for the given input data in
                    the classification example")
    }
    else if (clickedId == "connections1") {
      displayedText("The input nodes are connected to the nodes in the first
                    hidden layer through weighted connections. These weights
                    determine the importance of each input feature in 
                    influencing the activations of the first hidden layer nodes.")
    }
    else if (clickedId == "connections2") {
      displayedText("The outputs of the first hidden layer nodes are connected
                    to the nodes in the second hidden layer with their
                    respective weights. This allows the second hidden layer to
                    receive and process information from the first hidden layer.")
    }
    else if (clickedId == "connections3") {
      displayedText("The outputs from the second hidden layer nodes are
                    connected to the output node through weighted connections.
                    These weights determine the final influence of the second 
                    hidden layer's activations on the output.")
    }
  }
  )
  
  # Output the displayed text to the UI
  output$displayText <- renderText(
    expr = {
    displayedText()
  })
  
  
  ## Set up Prereq button ----
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Head to the Explore page to see Neural Networks in action"
      )
    }
  )
  
  ## Set up Explore Page ----
  
  observeEvent(input$makenum, {
    output$numbersPlot <- renderPlot({
      generateNumbersPlot()
    })
  })
  
  # Load the training data (audio files for selected digits and speakers)
  trainingData <- reactive(
    {
    selectedDigit <- input$selectedDigit
    selectedSpeaker <- input$selectedSpeaker
    
    if (selectedDigit == "All" & selectedSpeaker == "All") {
      # If both "All" are selected, use all data
      fileNamesSubset <- trainNNData$filedir
    } else {
      # Filter based on both selected digit and speaker
      fileNamesSubset <- trainNNData %>%
        filter(digit == selectedDigit | selectedDigit == "All") %>%
        filter(speaker == selectedSpeaker | selectedSpeaker == "All") %>%
        pull(filedir)
    }
    
    fileNamesSubset
  })
  
  # Randomly select a file from the training data when the "Select Random Wave File" button is clicked
  randomFile <- reactiveVal()
  
  observeEvent(
    eventExpr = input$select, 
    handlerExpr = {
    if (length(trainingData()) > 0) {
      randomFile(sample(trainingData(), 1))
      output$fileInfo <- renderText(paste("Selected File:", randomFile()))
    } else {
      output$fileInfo <- renderText("No files found for the selected digit and speaker.")
    }
      changeTrack(
        id = "sound",
        track = paste0("www/", randomFile())
      )
  }
  )
  
  # Render the waveform plot
  output$waveform <- renderPlot(
    expr = {
    if (!is.null(randomFile())) {
      audio <- readWave(paste0("www/", randomFile()))
      audio_df <- data.frame(time = 1:length(audio@left) / audio@samp.rate, amplitude = audio@left)
      firstDigit <- unlist(strsplit(randomFile(), "_"))[1]
      speakerName <- unlist(strsplit(randomFile(), "_"))[2]
      ggplot(audio_df, aes(x = time, y = amplitude)) +
        geom_line() +
        labs(title = paste("Waveform of", speakerName, "saying", firstDigit),
             x = "Time (s)", y = "Amplitude")+
        theme(text = element_text(size = 18),
              plot.title = element_text(size = 18))+
        theme_bw() +
        scale_x_continuous(
          limits = c(0, 0.6)
        )
    }
  },
  alt = "Waveform of audio")
  
  #### Set up Example Page----
  observeEvent(
    eventExpr = input$simulateMulti,
    handlerExpr = {
      selected_row <- cmtibble[cmtibble$layer == input$nLayer & cmtibble$epochs
                               == as.numeric(input$nEpochs), ]
      output$cmTable <- renderPrint({
        if (!is.null(selected_row)) {
          selected_row$confusionmatrix[[1]]
        }
      })
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)