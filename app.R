# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(seewave)
library(tuneR)
library(ggplot2)



# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "green",
    ### Create the app header ----
    dashboardHeader(
      title = "Classification using Neural Networks", # You may use a shortened form of the title here
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
            tags$li("Review the prerequiste iformation and ideas using the
                    Prerequistes tab."),
            tags$li("Explore the dataset Exploration Tab."),
            tags$li("Head to the *tab* to see two neural networks in action!
                    Dealing with examples of binary and multinomial
                    classification in a hospital.")
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
            "This version of the app was developed and coded by Robert Chappell",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 07/07/2022 by RWC.")
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
            tags$li("Binary Classification--Technical/Conceptual Prerequisites are ideas that
                    users need to have in order to engage with your app fully."),
            tags$li("Multinomial Classification--Contextual Prerequisites refer to any information
                    about a context in your app that will enrich a user's
                    understandings.")
          ),
          box(
            title = strong("Neural Network"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "In the diagram below you can see the general structure of a neural
            network. Click on nodes to learn more about what their purpose is.
            [INSERT IMAGE]"
          ),
          box(
            title = strong("Activation Functions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Activation functions are vital components in neural networks,
            especially when it comes to distinguishing between binary and
            multinomial classifications. In binary classification, where we
            have to categorize data into two classes, we commonly employ the
            sigmoid function. The sigmoid function maps input values to a range
            between 0 and 1, giving us a convenient probability interpretation
            for the output. Multinomial classification, however, deals with
            categorizing data into more than two classes. In such cases, we turn
            to the softmax activation function. Softmax transforms a vector of real
            values into a probability distribution across multiple classes,
            ensuring that the probabilities sum up to 1. This allows us to select
            the class with the highest probability as the predicted class."
          )
        ),
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Dataset"),
          p("This dataset being used contains 3,000 recordings (50 of each digit
            per speaker). The digits are 0-9, and all are with english pronunciation."),
          p("Explore the dataset below looking at the waveforms for the various
            audio files, and take a listen!"),
          ##### Waveform Player -----
          fluidRow(
            column(width = 6,
                   actionButton("select", "Select Random Wave File"),
                   verbatimTextOutput("fileInfo"),
                   actionButton("play", "Play", icon("headphones"))
            ),
            column(width = 6,
                   plotOutput("waveform")
            )
          )
        ),
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
  waveFiles <- list.files("recordings", pattern = "\\.wav$", full.names = TRUE)
  randomFile <- reactiveVal()
  
  observeEvent(input$select, {
    randomFile(sample(waveFiles, 1))
    output$fileInfo <- renderText(paste("Selected File:", randomFile()))
  })
  
  output$waveform <- renderPlot({
    if (!is.null(randomFile())) {
      audio <- readWave(randomFile())
      audio_df <- data.frame(time = 1:length(audio@left)/audio@samp.rate, amplitude = audio@left)
      
      first_digit <- as.integer(substring(basename(randomFile()), 1, 1))
      
      ggplot(audio_df, aes(x = time, y = amplitude)) +
        geom_line() +
        labs(title = paste("Waveform of", first_digit), x = "Time (s)", y = "Amplitude")
    }
  },
  alt = "Waveform of audio"
  )
  
  observeEvent(input$play, {
    if (!is.null(randomFile())) {
      audio <- readWave(randomFile())
      play(audio)
    }
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)