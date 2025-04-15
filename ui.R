#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(tidyverse)
library(haven)
library(straightliner)
library(openxlsx)
library(noodles)
library(DT)

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

# folder <- here("Tech Bullseye", "2024", "data", "prep", "raw_files")
# file <- list.files(folder) %>% str_subset("Merged")
# out <- haven::read_sav(glue("{folder}/{file}")) %>% haven::as_factor()
#
# batts <- out %>%
#     names() %>%
#     # anything ending in a number is likely a battery item, take their prefixes
#     str_subset("_[[:digit:]]+$") %>%
#     str_replace("_[[:digit:]]+$", "") %>%
#     unique() %>%
#     # remove the "_other" AI batteries, it's too small
#     str_subset("_other", negate = TRUE) %>%
#     # remove paradata we add ourselves
#     str_subset("Flag", negate = TRUE)
# })
