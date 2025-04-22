z_score <- function(vctr) {
  mean_vctr <- mean(vctr, na.rm = TRUE)
  sd_vctr <- sd(vctr, na.rm = TRUE)

  (vctr - mean_vctr) / sd_vctr
}

vars_all_full <- function(data) {
  top <- nrow(data)
  counts <- map_dbl(data, function(x) {
    is.na(x) %>%
      {
        !.
      } %>%
      sum()
  }) %>%
    sort()
  counts[counts == top] %>% names()
}


vars_non_zero_sd <- function(data) {
  keep(data, function(x) {
    (sd(x, na.rm = TRUE) > 0) %>%
      {
        ifelse(
          is.na(.),
          -999,
          .
        )
      } %>%
      {
        . > 0
      }
  }) %>%
    names()
}


fix_NA_non_straightline <- function(data, vars) {
  almost <- data %>%
    select(all_of(vars)) %>%
    rowwise() %>%
    mutate(all_empty = c_across(all_of(vars)) %>% is.na() %>% all())

  which(almost$all_empty)
}


clean <- function(data) {
  data %>%
    # numerify things which can be
    mutate(across(where(is.factor), as.numeric)) %>%
    # removing paradata
    select(-matches("Flag|FLAG|record|new_cases|Survey|time|TIME")) %>% # eventually, allow the user to dictate what variables make it in or not
    # excluding these for MVP (but address character class later)
    select(-where(is.character), -where(is.POSIXt))
}
