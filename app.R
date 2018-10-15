#setwd("~/Documents/projects/digits/digits/")

fileName <- './www/src.txt'
text_src <- readChar(fileName, file.info(fileName)$size)
digits <- data.frame(num = 0:9,txt = c("zero","one","two","three","four","five","six","seven","eight","nine"))
lapply(c("shiny", "keras", "shinyjs", "V8", "rvest", "magick", "png", "plotly", "colorspace", "XML", "openssl"), require, character.only = TRUE)

max_inverse <- function(matrix){
  matrix <- abs(matrix - 1)
  return(matrix)
}

max_one <- function(matrix){
  max = max(matrix)
  matrix = matrix / max
  return(matrix)
}

max_limit <- function(matrix, limit){
  matrix[matrix > limit] <- limit
  return(matrix)
}

transfer_src <- function(src = input$source){
  kern <- matrix(0, ncol = 3, nrow = 3)
  kern[c(1,3), c(1,3)] <- 0.1
  kern[2, c(1,3)] <- 0.12
  kern[c(1,3), 2] <- 0.12
  kern[2,2] <- 0.15
  img <- gsub("data:image/png;base64,", replacement = "", x = src)
  img <- image_read(base64_decode(img)) %>%
    image_convert(colorspace = "gray") %>%
    image_sample(c(28,28)) %>%
    image_convolve(kern) %>%
    image_write() %>%
    readPNG() %>%
    max_inverse %>%
    max_one() %>%
    max_limit(limit = 0.8) %>%
    max_one()
  return(img)
}

placeholder <- runif(10)
placeholder <- placeholder/sum(placeholder)

model <- load_model_hdf5("./www/cnn")

ui <- fluidPage(
  
  includeCSS("./www/style.css"),
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/signature_pad@2.3.2/dist/signature_pad.min.js")),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script = "./www/pad.js"),
  
  theme = "anti-drag.css", 
  column(12, titlePanel("Pattern Recognition with Convolutional Neural Network"), align = "center", br()),
  column(width = 5, offset = 1, align = "center",
         h3("Draw Digit Between Zero and Nine"),
         br(),
         div(class="wrapper",
             HTML("<canvas id='signature-pad' class='signature-pad' width=400 height=400></canvas>")),
         br(),
         div(class="wrapper", style="height:70px; display: none;",
             HTML("<div> <button class='button' 
                  id='save'>Submit</button> 
                  <button class='button' 
                  id='clear'>Clear</button></div>"))),
  column(width = 5, align = "center", 
         h3("Softmax Probability of Most Likely Digit"),
         br(),
         #plotOutput("image"),
         plotlyOutput("probabilities",  width="400px", height="400px")
         ),
  column(width = 12, align = "center", 
         h4("Due to pre-processed pixelation of image prior to input, smaller scale figures underperform")
  )
  )



server <- function(input, output, session) {
  
  boolean <- reactiveValues(active = FALSE)
  
  
 # onclick("save",{
#    boolean$active <- TRUE
 # })
  
  onevent("mouseleave","signature-pad",{
     
     delay(10,  boolean$active <- TRUE)
   
     click("save")
  
     print(boolean$active)
   
     click("clear")
  })
  
  
  
 # onevent("mouseleave","signature-pad", expr = { 
#    boolean$active <- TRUE 
 # })
  
 
  output$probabilities <- renderPlotly({
    
    source <- ifelse(boolean$active == FALSE, text_src, input$source)
    
    img <- transfer_src(source) %>%
      array_reshape(c(1, 28, 28, 1))
    
    probabilities <- as.vector(predict_proba(model, img))
    probabilities <- probabilities + (runif(10)/1000)
    probabilities <- probabilities/sum(probabilities)
    
    
    #print probability scores

    print(round(ifelse(is.na(probabilities),0,probabilities) * 100))
    
      data.frame(h = c(as.character(which.max(probabilities)-1), "other"), 
                 p = c(max(probabilities),
                       sum(probabilities[-(which.max(probabilities))]))) %>%
        plot_ly(hoverinfo = "none") %>%
        add_pie(hole = 0.7, marker = list(colors = c("#337AB7", "lightgray")), 
                rotation = runif(1)*360, labels = ~h, values = ~p, textinfo = "none") %>%
        layout(title = "",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange=TRUE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange=TRUE)) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE, dragmode = "none", bgcolor= 'rgba(0,0,0,0)') %>%
        add_text(text = paste0(digits[which.max(probabilities),"txt"],"\n",round(max(probabilities)*100),"%"),
                 y = 0, x = 0, textfont = list(size = 48))
  })
  
  # output$image <- renderPlot({
  #   source <- ifelse(boolean$active == FALSE, text_src, input$source)
  #   transfer_src(source) %>%
  #     image()
  # })
  # 
}

shinyApp(ui, server)