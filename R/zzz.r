.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0L)
    return()

  crayon::num_colors(TRUE)
  finRes_attach()

  # if (!"package:conflicted" %in% search()) {
  #   x <- finRes_conflicts()
  #   msg(finRes_conflict_message(x), startup = TRUE)
  # }

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
