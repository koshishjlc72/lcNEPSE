nepse_dataset_registry <- function() {
  path <- nepse_package_file("extdata", "dataset_registry.csv")
  out <- nepse_read_csv(path)
  out$supported <- tolower(out$supported) == "true"
  out
}

nepse_dataset_row <- function(dataset_id) {
  reg <- nepse_dataset_registry()
  row <- reg[reg$dataset_id == dataset_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    nepse_stop(
      "Unknown dataset `%s`. Use nepse_catalog() to list dataset IDs.",
      dataset_id
    )
  }
  row[1L, , drop = FALSE]
}

nepse_parser_version_for <- function(dataset_id) {
  nepse_dataset_row(dataset_id)$parser_version[[1L]]
}

nepse_dataset_ids <- function() {
  nepse_dataset_registry()$dataset_id
}
