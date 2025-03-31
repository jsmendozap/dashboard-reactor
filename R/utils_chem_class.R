#' ChemCalculations Class
#'
#' @description This R6 class provides a set of tools for performing chemometric calculations
#' related to chemical reactions. It includes methods for processing data, calculating
#' molar flows, conversions, mass balances, and generating summaries.
#'
#' @details The `ChemCalculations` class is designed to handle chemometric data efficiently.
#' It uses private methods for internal calculations and public methods for user interaction.
#' The class relies on several tidyverse packages for data manipulation and processing.
#'
#' @importFrom dplyr filter slice_head pull mutate left_join rowwise ungroup select across all_of summarise distinct transmute cur_column any_of contains relocate join_by
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom purrr discard map_vec
#' @importFrom tidyselect last_col ends_with
#' @importFrom stringr str_to_title str_replace_all str_replace str_c
#' @importFrom R6 R6Class
#' @importFrom tibble as_tibble
#'
#' @return An R6 class for chemometric calculations.
#'
#' @noRd

ChemCalculations <- R6Class(
  classname = "Chemometrics",

  private = list(
    sel_events = function(which) {
      values <- self$tech %>%
        unlist %>%
        discard(~ .x == "") %>%
        names %>%
        as.numeric

      self$bd %>%
        filter(event == values) %>%
        slice_head(n = 1, by = event) %>%
        pull({{ which }})
    },

    get_mass = function() {
      self$path %>%
        as.character %>%
        strsplit(" ") %>%
        unlist %>%
        {
          .[length(.) - 1]
        } %>%
        as.numeric(.) /
        1e6
    },

    get_reactants = function() {
      self$setting %>%
        filter(grepl("Reactant", type)) %>%
        pull(compound) %>%
        private$change_str(" ", "_", T)
    },

    avgs = function() {
      self$chem_values %>%
        filter(technique == 'By Pass') %>%
        group_by(event) %>%
        summarise(
          technique = unique(technique),
          across(
            .cols = self$reactants,
            .fns = ~ mean(.x[!.x %in% boxplot.stats(.x)$out], na.rm = TRUE)
          )
        )
    },

    compute_bypass = function() {
      self$chem_values %>%
        select(event, technique) %>%
        distinct() %>%
        filter(technique == 'By Pass') %>%
        left_join(private$avgs()) %>%
        transmute(
          event,
          technique,
          across(.cols = self$reactants, .names = "{.col}_bypass")
        )
    },

    change_str = function(string, from, to, lower = F) {
      map_vec(
        .x = string,
        .f = ~ .x %>%
          {
            ifelse(lower, tolower(.), str_to_title(.))
          } %>%
          str_replace_all(pattern = from, replacement = to)
      )
    }
  ),

  public = list(
    setting = NULL,
    tech = NULL,
    path = NULL,
    bd = NULL,
    gc = NULL,
    ms = NULL,
    mass = NULL,
    reactants = NULL,
    bypass = NULL,
    chem_values = NULL,

    initialize = function(setting, tech, path, bd, gc, ms) {
      self$setting <- setting
      self$tech <- tech
      self$path <- path
      self$bd <- bd
      self$gc <- gc
      self$ms <- ms
      self$mass <- private$get_mass()
      self$chem_values <- self$compute_chem_values()
      self$reactants <- private$get_reactants()
      self$bypass <- private$compute_bypass()
    },

    compute_chem_values = function() {
      data.frame(
        event = private$sel_events(event),
        name = private$sel_events(name),
        technique = self$tech %>% unlist %>% discard(~ .x == "")
      ) %>%
        mutate(
          is = self$setting %>%
            drop_na(internal_standar) %>%
            pull(compound) %>%
            tolower(),
          qis = private$sel_events(fic_140)
        ) %>%
        left_join(self$gc, by = join_by('event')) %>%
        rowwise() %>%
        mutate(across(
          10:last_col(),
          ~ qis * (. / get(is)) * (60 / (22.4 * self$mass * 1000))
        )) %>%
        ungroup() %>%
        drop_na(time)
    },

    molar_flows = function(comps, events) {
      print(list(comps, events))
      self$chem_values %>%
        select(event, time, 10:last_col()) %>%
        pivot_longer(
          cols = 3:last_col(),
          names_to = 'Compound',
          values_to = 'value'
        ) %>%
        filter(!Compound %in% c('argon', 'nitrogen')) %>%
        mutate(Compound = private$change_str(Compound, "_", " ")) %>%
        filter(Compound %in% comps & event %in% events)
    },

    conversion = function() {
      self$chem_values %>%
        filter(technique == 'TOS') %>%
        left_join(
          self$bypass %>% mutate(event = event + 1),
          by = join_by('event')
        ) %>%
        mutate(across(
          .cols = self$reactants,
          .fns = ~ 100 *
            ((get(paste0(cur_column(), "_bypass")) - .) /
              get(paste0(cur_column(), "_bypass")))
        )) %>%
        select(time, event, all_of(self$reactants)) %>%
        pivot_longer(
          cols = 3:ncol(.),
          names_to = 'Compound',
          values_to = 'value'
        ) %>%
        mutate(Compound = private$change_str(Compound, '_', ' '))
    },

    mass_balance = function(events) {
      c_in <- self$setting %>%
        mutate(compound = private$change_str(compound, " ", "_", T)) %>%
        filter(compound %in% names(self$chem_values))

      mass_plot <- self$chem_values %>%
        filter(technique == 'TOS') %>%
        select(event, time, any_of(c_in$compound))

      b_in <- self$bypass %>%
        relocate(event, technique, contains(c_in$compound)) %>%
        {
          as.matrix(x = .[, 3:ncol(.)])
        } %*%
        as.matrix(
          filter(c_in, compound %in% self$reactants) %>% .[, 4:6]
        ) %>%
        as_tibble() %>%
        mutate(event = self$bypass$event + 1)

      mass_plot <- as.matrix(mass_plot[, 3:ncol(mass_plot)]) %*%
        as.matrix(c_in[, 4:6]) %>%
        as_tibble() %>%
        mutate(event = mass_plot$event, time = mass_plot$time) %>%
        left_join(
          b_in,
          by = join_by('event'),
          suffix = c("_out", "_in")
        ) %>%
        transmute(
          event,
          time,
          across(
            .cols = ends_with("_out"),
            .fns = ~ .x /
              get(str_replace(cur_column(), "_out", "_in")),
            .names = "{str_replace(.col, '_out', '')}"
          )
        ) %>%
        pivot_longer(
          cols = 3:last_col(),
          names_to = "Compound",
          values_to = "value"
        ) %>%
        mutate(
          Compound = private$change_str(Compound, '_', ' '),
          value = value * 100
        ) %>%
        filter(event %in% events)

      summary <- function(events) {
        mass_plot %>%
          group_by(event, Compound) %>%
          summarise(
            avg = round(mean(value), 2),
            sd = round(sd(value), 2)
          ) %>%
          filter(event %in% events)
      }

      list(
        data = mass_plot,
        summary = summary(events)
      )
    },

    boxplot = function(compounds, events) {
      self$chem_values %>%
        filter(technique == 'TOS') %>%
        left_join(
          self$bypass %>% mutate(event = event + 1),
          by = join_by('event')
        ) %>%
        mutate(across(
          .cols = self$reactants,
          .fns = ~ (get(paste0(cur_column(), "_bypass")) - .)
        )) %>%
        select(time, event, 10:20) %>%
        pivot_longer(
          cols = 3:last_col(),
          names_to = 'Compound',
          values_to = 'value'
        ) %>%
        mutate(Compound = private$change_str(Compound, '_', ' ')) %>%
        filter(
          Compound %in% compounds & event %in% events
        )
    },

    event_names = function(filter = T) {
      self$chem_values %>%
        {
          if (filter) filter(., technique == 'TOS') else .
        } %>%
        summarise(name = unique(name), .by = event) %>%
        mutate(name = str_c("Event: ", event, " - ", name)) %>%
        {
          setNames(.$name, .$event)
        }
    }
  )
)
