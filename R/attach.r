core <- c("BBGsymbols", "factorem", "fewISOs", "FFresearch", "GICS", "plotit", "pullit", "storethat")

core_loaded <- function() {
  search <- paste0("package:", core)
  core[search %in% search()]
}
core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}


finRes_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0L)
    return(invisible())

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("finRes ", package_version("finRes"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1L))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2L == 1L) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2L)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1L]])

  if (length(version) > 3L) {
    version[4L:length(version)] <- crayon::red(as.character(version[4L:length(version)]))
  }
  paste0(version, collapse = ".")
}
