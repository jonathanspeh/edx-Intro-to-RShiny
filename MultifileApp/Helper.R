# Dependencies
library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(tidyverse)

# "create" data ####
#readr::write_csv(mpg, here::here("MultifileApp", "data", "mtcars.csv"))

# Prepare data ####
myData=readr::read_csv(here::here("MultifileApp", "data", "mpg.csv"),
                       show_col_types = FALSE)

numVars=names(myData |> select(where(is.numeric)))
textVars = myData |> select(where(is.character))
textVars = names(textVars[,-4])
classVars = unique(myData$class)



# shiny::runApp("MultifileApp")
