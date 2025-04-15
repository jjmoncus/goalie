#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(haven)
library(straightliner)
library(openxlsx)
library(noodles)
library(DT)


# Define server logic required to draw a histogram
function(input, output, session) {
  df <- reactive({
    req(input$file) # Ensure a file is uploaded

    path <- input$file$datapath

    if (str_detect(path, "csv$")) {
      read.csv(path) # Read the uploaded file
    } else if (str_detect(path, "xlsx$")) {
      read.xlsx(path)
    } else if (str_detect(path, "sav$")) {
      haven::read_sav(path) %>% haven::as_factor()
    } else {
      stop("Unsupported file type")
    }
  })

  # calculations
  batts <- reactive({
    df() %>%
      names() %>%
      # anything ending in a number is likely a battery item, take their prefixes
      str_subset("_[[:digit:]]+$") %>%
      str_replace("_[[:digit:]]+$", "") %>%
      unique() %>%
      # remove paradata we add ourselves
      str_subset("(Flag)|(FLAG)", negate = TRUE) %>%
      str_subset("(Hid)|(hid)|(HID)", negate = TRUE)
  })

  all_straightlines <- reactive({
    batts() %>%
      set_names() %>%
      map(function(x) {
        vars <- find_vars(df(), x)

        straightliner::straightlining(
          df(),
          varnames = vars,
          measures = "spv"
        ) %>%
          as_tibble(rownames = "row_number") %>%
          mutate(row_number = as.integer(row_number)) %>%
          filter(spv < .3)
      }) %>%
      list_rbind(names_to = "var")
  })

  # helpful numbers

  sl_summary <- reactive({
    table(all_straightlines()$row_number) %>%
      as.data.frame() %>%
      as_tibble() %>%
      rename(`Row Number` = Var1, `Number of Straightlined Batts` = Freq) %>%
      arrange(desc(`Number of Straightlined Batts`))
  })

  # outputs
  output$num_respondents <- renderText({
    nrow(df())
  })
  output$num_batts <- renderText({
    length(batts())
  })
  output$num_straightliners <- renderText({
    nrow(sl_summary())
  })

  output$sl_summary <- renderDT({
    datatable(
      sl_summary(),
      selection = list(mode = "single", target = "row", selected = 1)
    )
  })

  output$sl_glimpse <- renderPrint({
    row_selected <- input$sl_summary_rows_selected

    # the difference between the row as presented in DT
    # and the `Row Number` value in the cell
    row_in_df <- sl_summary() %>% slice(row_selected) %>% pull(`Row Number`)
    df() %>% slice(as.integer(row_in_df)) %>% glimpse()
  })
}
