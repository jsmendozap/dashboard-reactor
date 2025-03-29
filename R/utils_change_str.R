#' change_str
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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
