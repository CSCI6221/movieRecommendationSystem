library(shiny)
library(shinyWidgets)
library(shinydashboard)


genres <- c("Action", "Adventure", "Animation", "Children", 
            "Comedy", "Crime","Documentary", "Drama", "Fantasy",
            "Film_Noir", "Horror", "Musical", "Mystery","Romance",
            "Sci_Fi", "Thriller", "War", "Western")

age <- c("under18","18-24","25-34","35-44","45-49","50-55","56+")


shinyUI(fluidPage(
  setBackgroundImage(
    src = "https://wallpapers.com/images/hd/white-stripes-3d-background-hqdgpew0pemnyfaa-hqdgpew0pemnyfaa.jpg"
  ),
  fluidRow(
  column(12,h1(strong("Movie Recommendation Test: "),style = "color: #000000",align="center"))),

  fluidRow(
    column(1,h2()),
    
    column(3,h2( "Select Your Age"),
           wellPanel(
             selectInput("input_age", "Age",
                         age)
           )),
    
    column(4, h3("Select Genres You Like (most liked comes first):"),
           wellPanel(
             selectInput("input_genre", "Genre 1",
                         genres),
             selectInput("input_genre2", "Genre 2",
                         genres),
             selectInput("input_genre3", "Genre 3",
                         genres)
           )),
    
    
    column(4, h3("Which Movies You Like?\n
                 "),
           wellPanel(
             uiOutput("ui"),
             uiOutput("ui2"),
             uiOutput("ui3"),
           ))
    
    ),
  fluidRow(
    column(4,h3("")),
    column(5,
           h1("Recommended Movies: ",style= "color: #000000"),
           tableOutput("table"),
    )

  )
)
)