library(keras)
library(shiny)
library(pixels)
library(ggplot2)
library(shinycssloaders)
library(htmlwidgets)


# load pickled cnn
cnn <- load_model_hdf5("www/cnn")
#cnn <- load_model_hdf5("/srv/shiny-server/mnist/www/cnn")

# define function to convert pixel board to matrix and then to array, while normalizing data
pixels_array <- function(pixels, max_val){
  matrix <- matrix(pixels,28, 28, byrow = TRUE)
  matrix <- ifelse(matrix < max_val, matrix, max_val)
  #matrix <- apply(t(matrix),2,rev)
  return(array_reshape(matrix,c(1, 28, 28, 1)))
}

# define function to predict probabilities from pixel board
pixels_predict <- function(model, pixels, max_val = 1){
  array <- pixels_array(pixels, max_val = max_val)
  probabilities <- predict_proba(model, array)
  return(as.vector(probabilities))
}

# define function to return a specified length of randomn uniform, softmaxed probabilities
softmax <- function(length){
  vector <- runif(length)
  vector <- vector/sum(vector)
  return(vector)
}

# create discrete probability distribution from vector
pixels_barplot <- function(probabilities = softmax(10)){
  # create dataframe with discrete outcomes and predicted probabilities
  df <- data.frame(num = as.factor(0:(length(probabilities)-1)), prob = as.vector(probabilities))
  # create additional variable to indicate whether or not a row holds the maximum probabilities
  df$max <- (max(df$prob) == df$prob)
  # call ggplot
  ggplot(df, aes(num, y = prob, fill = max)) + 
    geom_col() + 
    geom_text(aes(label = scales::percent(df$prob), y = prob), vjust = -.5) +
    theme_minimal() + 
    labs(title ="Probability Distribution", 
         y = NULL, 
         x = NULL) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position="none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(size=16)) +
    scale_fill_manual(values = c("#ccb1b1", "#ae8383"))
}

# preemptively define random pixel board parameters
brush <- matrix(c(0.25, 0.5, 0.25,   0.25, 1.0, 0.25,   0.25, 0.5, 0.25),    3, 3)
params = list(fill = list(color = "#ae8383"))

# define random probabilities for later graphical render
probabilities <- softmax(10)

# define shiny user interface
ui <- shinyUI(
  fluidPage(
    column(12, titlePanel("Pattern Recognition with Convolutional Neural Network"), align = "center"),
    # first column
    column(5, offset = 1,
      # pixel board
      h3("Input"),
      withSpinner(
        shiny_pixels_output("pixels", width = "100%")
        ),
      align = "center"
    ),
    #second column
    column(5,
      # place barplot
      h3("Output"),
      withSpinner(
        plotOutput("barplot")
        ),
      br(),
      # set of buttons
      actionButton("reset", "Reset", width = "45%"),
      actionButton("submit", "Submit", width = "45%"),
      align = "center"
    )
  )
)


# define shiny server
server <- shinyServer(function(input, output, session) {
  
  # call barplot
  output$barplot <- renderPlot({ pixels_barplot(probabilities) })
  # call pixel board
  output$pixels <- shiny_render_pixels(show_pixels(brush = brush, params = params, size = c(450, 450)))

  # upon submit button
  observeEvent(input$submit, {
    # predict via cnn
    probabilities <- pixels_predict(model = cnn, pixels = input$pixels)
    # update plot
    output$barplot <- renderPlot({ pixels_barplot(probabilities) }, height = 400)
  })
  
  # upon submit button
  observeEvent(input$reset, {
    # reset pixel board
    output$pixels <- shiny_render_pixels( show_pixels(brush = brush, params = params, size = c(450, 450)) )
  })
  
})

# call application
shinyApp(ui, server)
