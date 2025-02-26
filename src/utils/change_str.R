change_str <- function(string, from, to, lower = F) {
  purrr::map_vec(
    .x = string,
    .f = ~ .x %>%
      {
        ifelse(lower, tolower(.), stringr::str_to_title(.))
      } %>%
      stringr::str_replace_all(pattern = from, replacement = to)
  )
}
