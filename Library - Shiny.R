shinyApp(ui, server)

#### How to create shiny begin ----
library(shiny)

ui <- fluidPage(
)

server <- function(input, output, session){
  
}

#########################################################################################################
############################################### Widgets examples ########################################
#########################################################################################################

# actionButton	Action Button
# checkboxGroupInput	A group of check boxes
# checkboxInput	A single check box
# dateInput	A calendar to aid date selection
# dateRangeInput	A pair of calendars for selecting a date range
# fileInput	A file upload control wizard
# helpText	Help text that can be added to an input form
# numericInput	A field to enter numbers
# radioButtons	A set of radio buttons
# selectInput	A box with choices to select from
# sliderInput	A slider bar
# submitButton	A submit button
# textInput	A field to enter text




#########################################################################################################
############################################# Function Refrence #########################################
#########################################################################################################


################################### UI Layout ##############################
# Functions for laying out the user interface for your application.

# absolutePanel() fixedPanel(): Panel with absolute positioning
# bootstrapPage() basicPage(): Create a Bootstrap page
# column(): Create a column within a UI definition
# conditionalPanel(): Conditional Panel
# fillPage(): Create a page that fills the window
# fillRow() fillCol(): Flex Box-based row/column layouts
# fixedPage() fixedRow(): Create a page with a fixed layout
# fluidPage() fluidRow(): Create a page with fluid layout
# helpText(): Create a help text element
# icon(): Create an icon
# navbarPage() navbarMenu(): Create a page with a top level navigation bar
# navlistPanel(): Create a navigation list panel
# sidebarLayout() sidebarPanel() mainPanel(): Layout a sidebar and main area
# tabPanel() tabPanelBody(): Create a tab panel
# tabsetPanel(): Create a tabset panel
# titlePanel(): Create a panel containing an application title.
# inputPanel(): Input panel
# flowLayout(): Flow layout
# splitLayout(): Split layout
# verticalLayout(): Lay out UI elements vertically
# wellPanel(): Create a well panel
# withMathJax(): Load the MathJax library and typeset math expressions


################################### UI Inputs ##############################
# Functions for creating user interface elements that prompt the user for input values or interaction.

# actionButton() actionLink(): Action button/link
# checkboxGroupInput(): Checkbox Group Input Control
# checkboxInput(): Checkbox Input Control
# dateInput(): Create date input
# dateRangeInput(): Create date range input
# fileInput(): File Upload Control
# numericInput(): Create a numeric input control
# radioButtons(): Create radio buttons
# selectInput() selectizeInput(): Create a select list input control
# varSelectInput() varSelectizeInput(): Select variables from a data frame
# sliderInput() animationOptions(): Slider Input Widget
# submitButton(): Create a submit button
# textInput(): Create a text input control
# textAreaInput(): Create a textarea input control
# passwordInput(): Create a password input control
# modalButton(): Create a button for a modal dialog
# updateActionButton() updateActionLink(): Change the label or icon of an action button on the client
# updateCheckboxGroupInput(): Change the value of a checkbox group input on the client
# updateCheckboxInput(): Change the value of a checkbox input on the client
# updateDateInput(): Change the value of a date input on the client
# updateDateRangeInput(): Change the start and end values of a date range input on the client
# updateNumericInput(): Change the value of a number input on the client
# updateRadioButtons(): Change the value of a radio input on the client
# updateSelectInput() updateSelectizeInput() updateVarSelectInput() updateVarSelectizeInput(): Change the value of a select input on the client
# updateSliderInput(): Update Slider Input Widget
# updateTabsetPanel() updateNavbarPage() updateNavlistPanel(): Change the selected tab on the client
# insertTab() prependTab() appendTab() removeTab(): Dynamically insert/remove a tabPanel
# showTab() hideTab(): Dynamically hide/show a tabPanel
# updateTextInput(): Change the value of a text input on the client
# updateTextAreaInput(): Change the value of a textarea input on the client
# updateQueryString(): Update URL in browser's location bar
# getQueryString() getUrlHash(): Get the query string / hash component from the URL



################################### UI Outputs ##############################
# Functions for creating user interface elements that, in conjunction with rendering functions, display different kinds of output from your application.

# htmlOutput() uiOutput(): Create an HTML output element
# imageOutput() plotOutput(): Create an plot or image output element
# outputOptions(): Set options for an output object.
# tableOutput() dataTableOutput(): Create a table output element
# textOutput() verbatimTextOutput(): Create a text output element
# downloadButton() downloadLink(): Create a download button or link

# Progress: Reporting progress (object-oriented API)
# withProgress() setProgress() incProgress(): Reporting progress (functional API)
# modalDialog(): Create a modal dialog UI
# urlModal(): Generate a modal dialog that displays a URL
# showModal() removeModal(): Show or remove a modal dialog
# showNotification() removeNotification(): Show or remove a notification



################################### Interface builder functions ##############################
# A sub-library for writing HTML using R functions. These functions form the foundation on which the higher level user interface functions are built, and can also be used in your Shiny UI to provide custom HTML, CSS, and JavaScript.
 
# tags p() h1() h2() h3() h4() h5() h6() a() br() div() span() pre() code() img() strong() em() hr(): HTML Builder Functions
# HTML(): Mark Characters as HTML
# includeHTML() includeText() includeMarkdown() includeCSS() includeScript(): Include Content From a File
# singleton() is.singleton(): Include content only once
# tagList() tagAppendAttributes() tagHasAttribute() tagGetAttribute() tagAppendChild() tagAppendChildren() tagSetChildren() tag(): HTML Tag Object
# validateCssUnit(): Validate proper CSS formatting of a unit
# withTags(): Evaluate an expression using tags
# htmlTemplate(): Process an HTML template
# bootstrapLib(): Bootstrap libraries
# suppressDependencies(): Suppress web dependencies
# insertUI() removeUI(): Insert and remove UI objects
# markdown(): Insert inline Markdown


################################### Rendering functions ##############################
# Functions that you use in your application’s server side code, assigning them to outputs that appear in your user interface

# renderPlot(): Plot Output
# renderCachedPlot(): Plot output with cached images
# renderText(): Text Output
# renderPrint(): Printable Output
# renderDataTable(): Table output with the JavaScript library DataTables
# renderImage(): Image file output
# renderTable(): Table Output
# renderUI(): UI Output
# downloadHandler(): File Downloads
# createRenderFunction(): Implement render functions


################################### Reactive Programming ##############################
# A sub-library that provides reactive programming facilities for R.

# reactive() is.reactive(): Create a reactive expression
# observe(): Create a reactive observer
# observeEvent() eventReactive(): Event handler
# reactiveVal(): Create a (single) reactive value
# reactiveValues(): Create an object for storing reactive values
# reactiveValuesToList(): Convert a reactivevalues object to a list
# is.reactivevalues(): Checks whether an object is a reactivevalues object
# isolate(): Create a non-reactive scope for an expression
# invalidateLater(): Scheduled Invalidation
# debounce() throttle(): Slow down a reactive expression with debounce/throttle
# reactlog() reactlogShow() showReactLog() reactlogReset(): Reactive Log Visualizer
# makeReactiveBinding(): Make a reactive variable
# reactiveFileReader(): Reactive file reader
# reactivePoll(): Reactive polling
# reactiveTimer(): Timer
# getDefaultReactiveDomain() withReactiveDomain() onReactiveDomainEnded(): Reactive domains
# freezeReactiveVal() freezeReactiveValue(): Freeze a reactive value


####################################### Boilerplate ###################################
# Functions that are required boilerplate in ui.R and server.R.

# shinyUI(): Create a Shiny UI handler
# shinyServer(): Define Server Functionality


########################################## Running ####################################
# Functions that are used to run or stop Shiny applications.

# runApp(): Run Shiny Application
# runGadget(): Run a gadget
# runExample(): Run Shiny Example Applications
# runUrl() runGist() runGitHub(): Run a Shiny application from a URL
# stopApp(): Stop the currently running Shiny app
# paneViewer() dialogViewer() browserViewer(): Viewer options
# isRunning(): Check whether a Shiny application is running
# loadSupport(): Load an app's supporting R files


#################################### Bookmarking state #################################
# Functions that are used for bookmarking and restoring state.

# bookmarkButton(): Create a button for bookmarking/sharing
# enableBookmarking(): Enable bookmarking for a Shiny application
# setBookmarkExclude(): Exclude inputs from bookmarking
# showBookmarkUrlModal(): Display a modal dialog for bookmarking
# onBookmark() onBookmarked() onRestore() onRestored(): Add callbacks for Shiny session bookmarking events

##################################### Extending Shiny ###################################
# Functions that are intended to be called by third-party packages that extend Shiny.

# createWebDependency(): Create a web dependency
# addResourcePath() resourcePaths() removeResourcePath(): Resource Publishing
# registerInputHandler(): Register an Input Handler
# removeInputHandler(): Deregister an Input Handler
# markRenderFunction(): Mark a function as a render function

#################################### Utility functions ###################################
# Miscellaneous utilities that may be useful to advanced users or when extending Shiny.

# shinyAppTemplate(): Generate a Shiny application from a template
# req() isTruthy(): Check for required values
# validate() need(): Validate input values and other conditions
# session: Session object
# getShinyOption() shinyOptions(): Get or set Shiny options
# safeError(): Declare an error safe for the user to see
# onFlush() onFlushed() onSessionEnded(): Add callbacks for Shiny session events
# restoreInput(): Restore an input value
# applyInputHandlers(): Apply input handlers to raw input values
# exprToFunction(): Convert an expression to a function
# installExprFunction(): Install an expression as a function
# parseQueryString(): Parse a GET query string from a URL
# getCurrentOutputInfo(): Get output information
# plotPNG(): Run a plotting function and save the output as a PNG
# sizeGrowthRatio(): Create a sizing function that grows at a given ratio
# exportTestValues(): Register expressions for export in test mode
# setSerializer(): Add a function for serializing an input before bookmarking application state
# snapshotExclude(): Mark an output to be excluded from test snapshots
# snapshotPreprocessInput(): Add a function for preprocessing an input before taking a test snapshot
# snapshotPreprocessOutput(): Add a function for preprocessing an output before taking a test snapshot
# markOutputAttrs(): Mark a render function with attributes that will be used by the output
# repeatable(): Make a random number generator repeatable
# shinyDeprecated(): Print message for deprecated functions in Shiny
# serverInfo(): Collect information about the Shiny Server environment
# onStop(): Run code after an application or session ends
# diskCache(): Create a disk cache object
# memoryCache(): Create a memory cache object
# key_missing() is.key_missing(): A missing key object

############################################### Plot interaction ##################################
# Functions related to interactive plots

# brushedPoints() nearPoints(): Find rows of data selected on an interactive plot.
# brushOpts(): Create an object representing brushing options
# clickOpts(): Create an object representing click options
# dblclickOpts(): Create an object representing double-click options
# hoverOpts(): Create an object representing hover options

########################################### Modules ############################################
# Functions for modularizing Shiny apps
 
# NS() ns.sep: Namespaced IDs for inputs/outputs
# moduleServer(): Shiny modules
# callModule(): Invoke a Shiny module

########################################## Embedding ###########################################
# Functions that are intended for third-party packages that embed Shiny applications.

# shinyApp() shinyAppDir() shinyAppFile(): Create a Shiny app object
# maskReactiveContext(): Evaluate an expression without a reactive context

########################################## Testing #############################################
# Functions intended for testing of Shiny components

# runTests(): Runs the tests associated with this Shiny app
# testServer(): Reactive testing for Shiny server functions and modules
# MockShinySession: Mock Shiny Session








#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

###############################################    UI    ################################################
ui <- fluidPage(
  title = "Data Explorer",
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),  
  # up here, change your dataset (doesn't have to be list(ls))
  verbatimTextOutput("summary"),
  tableOutput("table")
) 







################################################### Input ###################################################
# First "" arguments are your naming to match with your output

# selectInput() # Using dataset list
selectInput("dataset", label = "Dataset", choices = ls("package:datasets"))

# selectInput()  # state.name is provided by R
selectInput("state", "what's your favorite state?", state.name)

## Free text Inputs
# textInput()
textInput("name", "What's your name?")

# passwordInput()
passwordInput("password", "What's your password?")

# textAreaInput()
textAreaInput("story", "Tell me about yourself", rows = 3)

## Numeric Inputs 
# numericInput()
numericInput("num", "Number one", value = 0, min = 0, max = 100)

# sliderInput()
sliderInput("num2", "Number two", value = 50, min = 0, max = 100)
sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100) # value is default setup

# dateInput()
dateInput("dob", "When were you born?")

# dateRangeInput()
dateRangeInput("holiday", "When do you want to go on a vacation next?")

# radioButtons()
radioButtons("animal", "What's your favorite animal?", animals)


# radioButtons() advanced
radioButtons("rb", "Choose one:", 
             choiceNames = list(
               icon("angry"),
               icon("smile"),
               icon("sad-tear")
             ),
             choiceValues = list("angry", "happy", "sad"))


# checkboxGroupInput
checkboxGroupInput("animal", "What animals do you like?", animals)

# checkboxInput
checkboxInput("cleanup", "Clean up?", value = TRUE)

checkboxInput("shutdown", "Shutdown?")


# fileInput  - it requires special handling on the server side.
fileInput("upload", NULL)

# Action  Buttons #
# actionButton
actionButton("click", "Click me!", class = "btn-danger")
actionButton("drink", "Drink me!", icon = icon("cocktail"), class = "btn-lg btn-success")
actionButton("eat", "Eat me!", class = "btn-block")



############################################# Output examples ################################################

# dataTableOutput	DataTable
# htmlOutput	raw HTML
# imageOutput	image
# plotOutput	plot
# tableOutput	table
# textOutput	text
# uiOutput	raw HTML
# verbatimTextOutput	text


################################################### Output ###################################################
############################################ match with server ###############################################

# OUTPUT #    (verbatimTextOutput pair with renderPrint), (tableOutput pair with rednerTable)
verbatimTextOutput("summary")
tableOutput("table")

# textOutput  (textOutput pair with renderText), (verbatimTextOutput pair with renderPrint)
textOutput("text")
verbatimTextOutput("code")

# dataTableOutput (pair with renderDataTable)
dataTableOutput("dynamic")

# tableOutput (pair with renderTable)
tableOutput("static")

# dataTableOutput (pair with renderDataTable)
dataTableOutput("dynamic_2")


# plotOutput (pair with renderPlot)
plotOutput("plot", width = "400px")



#################################### Rendering examples ##############################################

# renderDataTable	DataTable
# renderImage	images (saved as a link to a source file)
# renderPlot	plots
# renderPrint	any printed output
# renderTable	data frame, matrix, other table like structures
# renderText	character strings
# renderUI	a Shiny tag object or HTML


######################################## server #######################################

server <- function(input, output, session){
  # Create a reactive expression 
  
  dataset <- reactive(get(input$dataset, "package:datasets"))
  
  output$summary <- renderPrint(
    # Use a reactive expression by calling it like a function
    summary(dataset())
  )
  
  output$table <- renderTable(dataset())
  
  output$text <- renderText("Hello Friend!")
  output$code <- renderPrint(summary(1:10))
  
  output$dynamic <- renderDataTable(mtcars)
  
  # renderTable for tableOutput
  output$static <- renderTable(head(mtcars))
  
  # renderDataTable for dataTableOutput
  output$dynamic_2 <- renderDataTable(mtcars, options = list(pageLength = 5))
  
  # renderPolt for plotOutput
  output$plot <- renderPlot(plot(1:5), res = 96)   # recommend always setting as res = 96
}





###############################################################################################################
########################################### shiny with esquisse ###############################################
###############################################################################################################

# all you need to is to change the data

library(esquisse)
library(modeldata)
library(shiny)

data("drinks")
data("mpg")

drinks
mpg

ui <- fluidPage(
  
  titlePanel("Use esquisse as a Shiny module"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "data",
        label = "Data to use:",
        choices = c("drinks", "mpg"),
        inline = TRUE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "esquisse",
          esquisserUI(
            id = "esquisse",
            header = FALSE, # dont display gadget title
            choose_data = FALSE # dont display button to change data
          )
        ),
        tabPanel(
          title = "output",
          verbatimTextOutput("module_out")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  data_r <- reactiveValues(data = drinks, name = "drinks")
  
  observeEvent(input$data, {
    if (input$data == "drinks") {
      data_r$data <- drinks
      data_r$name <- "drinks"
    } else {
      data_r$data <- mpg
      data_r$name <- "mpg"
    }
  })
  
  result <- callModule(
    module = esquisserServer,
    id = "esquisse",
    data = data_r
  )
  
  output$module_out <- renderPrint({
    str(reactiveValuesToList(result))
  })
  
}

shinyApp(ui, server)




###############################################################################################################
##################################### Shiny with dplyr, ggplot2 ###############################################
###############################################################################################################

### 1

# you need to have the data variable in your enviornment. 

# Define UI for application that draws a figure
ui <- fluidPage(
  titlePanel("Ideology in Congress"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("my_cong",
                  "Congress:",
                  min = 93,
                  max = 114,
                  value = 93)
    ),
    mainPanel(
      plotOutput("congress_distplot")
    )
  )
)

server <- function(input, output) {
  
  output$congress_distplot <- renderPlot({
    ggplot2::ggplot(data = dplyr::filter(data = dat, Congress == input$my_cong),  # here: filter used accordingly in fluidPage
                    mapping = aes(x = Ideology, color = Party, fill = Party)) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::xlim(-1.5, 1.5) +
      ggplot2::labs(x = "Ideology - Nominate Score", y = "Density") +
      ggplot2::scale_fill_manual(values = c("blue", "red")) +
      ggplot2::scale_color_manual(values = c("blue", "red"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



### 2

# sample data in Data vizualization JHU, course 4 - week 1
dat <- read_csv("Practice data for publishing.csv")

# Define UI for application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "checked_groups",
        label = "Which groups do you want to display?",
        choices = c("a", "b", "c"),
        selected = c("a", "b", "c")
      )
    ),
    mainPanel(
      plotOutput("scatter")
    )
  )
)


# Define server logic 
server <- function(input, output) {
  
  output$scatter<-renderPlot({
    
    plot_dat <- dplyr::filter(dat, Group %in% input$checked_groups)
    
    ggplot2::ggplot(dat = plot_dat, mapping = aes(x = varX, y = varY, color = Group)) + 
      geom_point()
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)




###############################################################################################################
################################################ Text formatting ##############################################
###############################################################################################################

# examples
# shiny function	HTML5 equivalent	creates
# p	<p>	A paragraph of text
# h1	<h1>	A first level header
# h2	<h2>	A second level header
# h3	<h3>	A third level header
# h4	<h4>	A fourth level header
# h5	<h5>	A fifth level header
# h6	<h6>	A sixth level header
# a	<a>	A hyper link
# br	<br>	A line break (e.g. a blank line)
# div	<div>	A division of text with a uniform style
# span	<span>	An in-line division of text with a uniform style
# pre	<pre>	Text ‘as is’ in a fixed width font
# code	<code>	A formatted block of code
# img	<img>	An image
# strong	<strong>	Bold text
# em	<em>	Italicized text
# HTML	 	Directly passes a character string as HTML code

# example using h
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title")
    )
  )
)


# example using formatting texting
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph.")
    )
  )
)






###############################################################################################################
################################################ Inserting Image ##############################################
###############################################################################################################

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src = "rstudio.png", height = 140, width = 400)
    )
  )
)





