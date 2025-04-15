#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Straightlining",

  sidebar = sidebar(
    card(
      card_header("Upload a Data File"),
      fileInput(
        "file",
        "Choose CSV, XLSX, or SAV file",
        accept = c(".csv", ".xlsx", ".sav")
      )
    ),
    card(
      card_header("Information"),

      card(
        card_header("Number of Respondents"),
        textOutput("num_respondents")
      ),
      card(
        card_header("Number of Survey Batteries Tested"),
        textOutput("num_batts")
      ),
      card(
        card_header(
          "Number of Respondents with Potential Straightlining Problems"
        ),
        textOutput("num_straightliners")
      )
    ),
    min_height = "200px"
  ),

  card(
    card_header("Summary"),
    DTOutput("sl_summary"),
    min_height = "300px"
  ),
  card(
    card_header("Glimpse"),
    verbatimTextOutput("sl_glimpse")
  )
)
