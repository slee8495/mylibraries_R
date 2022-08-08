shinyApp(ui, server)

#### How to create shiny begin ----
library(shiny)

ui <- fluidPage(
)

server <- function(input, output, session){
  
}


############################################### Widgets examples ########################################

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





