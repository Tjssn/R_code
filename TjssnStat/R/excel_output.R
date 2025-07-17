#' Export Analysis Results to Excel
#'
#' This function exports structured results from a `super_param` object (stored in `output$result$excel_output$body`)
#' to an Excel file. It supports custom file paths and names, with automatic handling of existing files (e.g., appending timestamps to avoid overwrites).
#'
#' @details
#' The function performs the following steps:
#' 1. Determines the output directory (`Path`): uses the current working directory if not specified.
#' 2. Generates the output file name (`name`):
#'    - Uses `Object$output$result$excel_output$file_name` if available,
#'    - Falls back to `default_output.xlsx` if no name is provided.
#' 3. Checks for existing files and handles conflicts (asks user to overwrite/rename/abort in interactive mode).
#' 4. Creates the output directory if it doesn't exist.
#' 5. Writes data from `Object$output$result$excel_output$body` to the Excel file using `openxlsx`.
#'
#' @param Object A `super_param` object. Must contain `output$result$excel_output$body` (the data to export).
#' @param Path Character string. Path to the directory where the Excel file will be saved. Defaults to the current working directory.
#' @param name Character string. Name of the Excel file (with or without `.xlsx` extension). Defaults to:
#'   - `Object$output$result$excel_output$file_name` (if available),
#'   - `default_output.xlsx` (if no name is specified).
#'
#' @return Invisibly returns the full path to the generated Excel file (character string). Returns `NULL` if export fails.
#'
#' @note
#' - Requires the `openxlsx` package. Install it with `install.packages("openxlsx")` if missing.
#' - The input `Object` must contain `output$result$excel_output$body` (a list of data frames, where each element becomes a sheet in Excel).
#' - If the output file already exists:
#'   - In interactive mode (e.g., RStudio), users are prompted to overwrite, rename (with a timestamp), or abort.
#'   - In non-interactive mode, the file is automatically renamed with a timestamp (`YYYYMMDD_HHMMSS`).
#' - Timestamps use the system's local time.
#'
#' @seealso
#' - `excel_in`: Import configuration data from Excel.
#' - `super_param`: Generate the `super_param` object containing results to export.
#'
#' @author TjssnStat Development Team <tongjibbb@163.com>
#' @keywords Excel export results data-writing
#' @export
#' @importFrom dplyr %>%
#' @importFrom httr POST upload_file
#' @importFrom utils install.packages packageVersion
#' @importFrom stats start

excel_output <- function(Object, Path = NULL, name = NULL) {
  ############## Helper Functions for Messaging ##############
  print_info <- function(msg) {
    cat("[INFO] ", msg, "\n", sep = "")
  }
  print_success <- function(msg) {
    cat("[SUCCESS] ", msg, "\n", sep = "")
  }
  print_error <- function(msg) {
    cat("[ERROR] ", msg, "\n", sep = "")
  }
  print_step <- function(msg) {
    cat("[STEP] ", msg, "\n", sep = "")
  }

  ############## Validate Input Object ##############
  if (missing(Object) || is.null(Object)) {
    print_error("'Object' must be a valid 'super_param' object (see ?super_param).")
    return(invisible(NULL))
  }

  # Check if output data exists in Object
  output_body <- Object[["output"]][["result"]][["excel_output"]][["body"]]
  if (is.null(output_body)) {
    print_error("No data to export. 'Object$output$result$excel_output$body' is NULL.")
    return(invisible(NULL))
  }

  ############## Resolve Output Path ##############
  if (is.null(Path)) {
    Path <- getwd()
    print_info(paste("Using default output directory:", Path))
  }

  # Create directory if it doesn't exist
  if (!dir.exists(Path)) {
    dir.create(Path, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(Path)) {
      print_success(paste("Created output directory:", Path))
    } else {
      print_error(paste("Failed to create directory:", Path))
      return(invisible(NULL))
    }
  }

  ############## Resolve File Name ##############
  if (is.null(name)) {
    # Use built-in name from Object if available
    built_in_name <- Object[["output"]][["result"]][["excel_output"]][["file_name"]]
    if (!is.null(built_in_name) && built_in_name != "") {
      name <- built_in_name
      print_info(paste("Using built-in file name from Object:", name))
    } else {
      name <- "default_output.xlsx"
      print_info(paste("No name specified. Using default:", name))
    }
  } else {
    print_info(paste("Using custom file name:", name))
  }

  # Ensure .xlsx extension if missing
  if (tools::file_ext(name) == "") {
    name <- paste0(name, ".xlsx")
    print_info(paste("Added .xlsx extension: ", name, sep = ""))
  }

  ############## Handle Existing File Conflicts ##############
  full_path <- tools::file.path(Path, name)
  file_exists <- file.exists(full_path)
  user_specified <- !is.null(Path) && !is.null(name)  # User provided both path and name

  if (file_exists) {
    print_info(paste("File already exists:", full_path))

    if (interactive()) {
      # Interactive mode: prompt user for action
      choice <- utils::askYesNo(
        paste("File", full_path, "exists. Overwrite? (YES = overwrite, NO = rename, CANCEL = abort)"),
        default = FALSE
      )

      if (is.na(choice)) {  # User canceled
        print_step("Export aborted by user.")
        return(invisible(NULL))
      } else if (!choice) {  # User chose to rename
        # Append timestamp to avoid overwriting
        time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
        file_prefix <- tools::file_path_sans_ext(name)
        file_ext <- tools::file_ext(name)
        new_name <- paste0(file_prefix, "_", time_suffix, ".", file_ext)
        full_path <- tools::file.path(Path, new_name)
        print_step(paste("Renamed file to:", new_name))
      } else {  # User chose to overwrite
        print_step("Overwriting existing file...")
      }
    } else {
      # Non-interactive mode: auto-rename with timestamp
      time_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
      file_prefix <- tools::file_path_sans_ext(name)
      file_ext <- tools::file_ext(name)
      new_name <- paste0(file_prefix, "_", time_suffix, ".", file_ext)
      full_path <- tools::file.path(Path, new_name)
      print_info(paste("Auto-renamed existing file to:", new_name))
    }
  }

  ############## Export to Excel ##############
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    print_error("Package 'openxlsx' is required but not installed.\nInstall with: install.packages('openxlsx')")
    return(invisible(NULL))
  }

  tryCatch({
    openxlsx::write.xlsx(output_body, file = full_path, rowNames = FALSE)
    if (file.exists(full_path)) {
      print_success(paste("Successfully exported to:", full_path))
      return(invisible(full_path))
    } else {
      print_error("Export failed: File not created.")
      return(invisible(NULL))
    }
  }, error = function(e) {
    print_error(paste("Failed to write Excel file:", e$message))
    return(invisible(NULL))
  })
}
