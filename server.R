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
library(careless)

source("helpful.R", local = TRUE)


# Define server logic required to draw a histogram
function(input, output, session) {
  df <- reactive({
    req(input$file) # Ensure a file is uploaded

    path <- input$file$datapath

    if (str_detect(path, "csv$")) {
      data <- read.csv(path) # Read the uploaded file
    } else if (str_detect(path, "xlsx$")) {
      data <- read.xlsx(path)
    } else if (str_detect(path, "sav$")) {
      data <- haven::read_sav(path) %>% haven::as_factor()
    } else {
      stop("Unsupported file type")
    }

    # clerical data edits
    # removing minimally-helpful variables that screw up operations
    data %>%
      # remove paradata we add ourselves
      select(-matches("flag|Flag|FLAG|Hid|hid|HID")) %>%
      # the unique example of "please specify" columns for "other" items
      select(-(matches("o$|oe$") & where(is.character)))
  })

  # calculations
  batts <- reactive({
    df() %>%
      names() %>%
      # anything ending in a number is likely a battery item, take their prefixes
      str_subset("_[[:digit:]]+$") %>%
      str_replace("_[[:digit:]]+$", "") %>%
      unique() %>%
      keep(function(x) {
        # they're all in a battery if indeed have the same levels
        vars_to_check <- df() %>% select(starts_with(x))
        # all need to be the same class
        # test1 <- vars_to_check %>% map(class) %>% unique() %>% {length(.) == 1}
        # starting with smaller case that they're all factors
        test1 <- vars_to_check %>% map_lgl(is.factor) %>% all()

        if (!test1) {
          # if test1 fails
          return(FALSE)
        } else {
          # since they're all factors, check they have the same levels

          levels_to_check <- vars_to_check %>%
            map(levels) %>% # get a list, with levels of each var
            unique() # should only be one element of list left, if they're all the same

          test_2 <- length(levels_to_check) == 1 # this is good

          test_3 <- !setequal(
            unlist(levels_to_check),
            c("Selected", "Not Selected")
          ) # matching these levels would be bad

          (test_2 && test_3)
        }
      })
  })

  all_straightlines <- reactive({
    batts() %>%
      set_names() %>%
      map(function(x) {
        vars <- find_vars(df(), x)

        almost <- straightliner::straightlining(
          df(),
          varnames = vars,
          measures = "spv"
        ) %>%
          as_tibble(rownames = "row_number") %>%
          mutate(row_number = as.integer(row_number)) %>%
          rowwise() %>%
          # presumes a unique_ID called "record", we will guess it later
          mutate(record = df() %>% slice(row_number) %>% pull(record)) %>%
          ungroup() %>%
          filter(spv < .3)

        fixes <- fix_NA_non_straightline(df(), vars)
        almost$spv[almost$row_number %in% fixes] <- NA_real_

        almost %>% filter(!is.na(spv))
      }) %>%
      list_rbind(names_to = "var")
  })

  # helpful numbers

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

  # now adding non-straightlining related things

  # first, numerify data
  num_df <- reactive({
    num_df <- clean(df)
  })

  new_problems_full <- reactive({
    # in certain instances, `vars_to_use` results in multicollinearity in `mahad`
    # if so, take a random sample and try again
    vars_to_use <- intersect(
      vars_all_full(num_df),
      vars_non_zero_sd(num_df)
    )
    trying_mahad <- try(mahad(num_df[vars_to_use], plot = FALSE) %>% z_score())
    while (class(trying_mahad) == "try-error") {
      vars_to_use <- sample(
        vars_to_use,
        (.9 * length(vars_to_use)) %>% floor(),
        replace = FALSE
      )

      trying_mahad <- try(mahad(num_df[vars_to_use], plot = FALSE))
    }

    # NEED TO FIND SOME WAY OF FORCING THE NUMBER OF VARIABLES IN EVENODD
    # TO MATCH THE SUM OF FACTORS LATER.
    # NOT SURE WHY IT'S BECOME UNEQUAL NOW

    out <- df %>% # back to original data
      mutate(
        longstrings = longstring(num_df) %>% z_score(),
        irvs = irv(num_df) %>% z_score(),
        mahads = trying_mahad %>% z_score(),
        evenodds = num_df %>%
          select(starts_with(batts)) %>%
          evenodd(
            factors = map_dbl(batts, function(x) {
              find_vars(num_df, x) %>% length()
            })
          ) %>%
          z_score()
      ) %>%
      rowwise() %>%
      mutate(
        flagged = c(
          c(longstrings, irvs, mahads) > 3,
          c(irvs, mahads) < -3 # deliberately excluding longstrings here, bottom-end zscores are not a problem
        ) %>%
          sum() %>%
          {
            . >= 1
          }
      )

    out <- out %>% add_column(row_number = 1:nrow(out))
    rows_to_keep <- which(out$flagged)

    out %>%
      slice(rows_to_keep) %>%
      select(row_number, longstrings, irvs, mahads)
  })

  # merge with other summary
  sl_summary <- reactive({
    sl_summary <- table(all_straightlines$row_number) %>%
      as.data.frame() %>%
      as_tibble() %>%
      rename(
        `Row Number` = Var1,
        `Number of (Near-)Straightlined Batts` = Freq
      ) %>%
      arrange(desc(`Number of (Near-)Straightlined Batts`)) %>%
      full_join(new_problems_full, by = c("Row Number", "row_number"))
  })
}
