setup <- function() {
  if (!dir.exists(path = "~/Reactor Dashboard")) {
    dir.create(path = "~/Reactor Dashboard/Reactions", recursive = T)
    file.copy(
      from = "Reaction settings.xlsx",
      to = "~/Reactor Dashboard/Reactions/Standard.xlsx"
    )
  }
}
