# digits

How to use this app?

* Fork/Clone this repo

* Install all the packages required (if not available). Easiest way is to use `pacman`:


```r
#install.packages("pacman")
pacman::p_install(c("shiny", "keras", "shinyjs", "V8", "rvest", "magick", "png", "plotly", "colorspace", "XML", "openssl"))
```

* If you are on Mac and running Python 3.7, Please install development version of keras using the below code:

```r
devtools::install_github("rstudio/keras") 
keras::install_keras()
```

* Run the app by executing the code or click Run icon in your RStudio and Enjoy your Deep Learning App!
