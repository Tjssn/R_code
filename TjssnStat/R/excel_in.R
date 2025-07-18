#' Import Configuration Data from Excel Files
#'
#' This function reads specific sheets ("Category.Variable.Level" and "Total.Variable") from an Excel file,
#' parses the data, and updates the `update` field of the input `Object` (typically a `super_param` object).
#' It supports custom file paths and names, with automatic defaults if not specified.
#'
#' @details
#' The function performs the following steps:
#' 1. Determines the Excel file path (uses current working directory if `Path` is `NULL`).
#' 2. Generates the Excel file name (defaults to `default_output.xlsx` or uses the name from `Object` if available).
#' 3. Checks if the Excel file and required sheets exist.
#' 4. Reads data from "Category.Variable.Level" and "Total.Variable" sheets using `openxlsx`.
#' 5. Updates the `update` field of the input `Object` with the parsed data.
#'
#' @param Object A `super_param` object containing configuration metadata (typically generated by `super_param()`).
#'   Must include `output$result$excel_output$file_name` if relying on built-in naming.
#' @param Path Character string. Path to the directory containing the Excel file. Defaults to the current working directory.
#' @param name Character string. Name of the Excel file (without extension if `.xlsx` is desired). Defaults to:
#'   - `Object$output$result$excel_output$file_name` (if available),
#'   - `default_output.xlsx` (if no built-in name exists).
#'
#' @return The input `Object` with an updated `update` field, containing:
#'   - `level.data`: Data from the "Category.Variable.Level" sheet,
#'   - `summary`: Data from the "Total.Variable" sheet.
#'   Returns the original `Object` unchanged if any errors occur (e.g., missing file/sheets).
#'
#' @note
#' - Requires the `openxlsx` package. If not installed, an error message will prompt installation.
#' - The Excel file must contain two mandatory sheets:
#'   - "Category.Variable.Level": Defines variable categories and levels.
#'   - "Total.Variable": Summarizes total variables and their properties.
#' - All non-ASCII characters in the Excel file should be avoided to ensure portability.
#'
#' @seealso
#' - \code{\link{excel_out}}: Export results to an Excel file (companion function),
#' - \code{\link{super_param}}: Generate the `super_param` object required as input.
#'
#' @author TjssnStat Development Team <tongjibbb@163.com>
#' @keywords Excel import configuration data-preprocessing
#' @export
#' @importFrom dplyr %>%
#' @importFrom httr POST upload_file
#' @importFrom utils install.packages packageVersion
#' @importFrom stats start

excel_in <- function(Object = Param1,
                     Path = NULL,
                     name = NULL) {
  ############## Validate Input Object ##############
  if (missing(Object) || is.null(Object)) {
    stop("'Object' must be a valid 'super_param' object (see ?super_param).")
  }

  # Store original object to return on error
  original_Object <- Object

  ############## Resolve File Path ##############
  # Set default path to working directory if not provided
  if (is.null(Path)) {
    Path <- getwd()
    message("Using default path: ", Path)
  }

  # Validate path exists
  if (!dir.exists(Path)) {
    warning("Path '", Path, "' does not exist. Using working directory instead.")
    Path <- getwd()
  }

  ############## Resolve File Name ##############
  # Generate default name from Object if available, else use fallback
  if (is.null(name)) {
    built_in_name <- Object[["output"]][["result"]][["excel_output"]][["file_name"]]

    if (!is.null(built_in_name) && built_in_name != "") {
      # Ensure .xlsx extension
      name <- if (file_ext(built_in_name) == "") {
        paste0(built_in_name, ".xlsx")
      } else {
        built_in_name
      }
      message("Using built-in file name: ", name)
    } else {
      name <- "default_output.xlsx"
      message("No built-in name found. Using default: ", name)
    }
  } else {
    # Ensure .xlsx extension for custom names
    name <- if (file_ext(name) == "") {
      paste0(name, ".xlsx")
    } else {
      name
    }
    message("Using custom file name: ", name)
  }

  ############## Validate Excel File Exists ##############
  full_path <- file.path(Path, name)

  if (!file.exists(full_path)) {
    stop("Excel file not found at: ", full_path,
         "\nCheck 'Path' and 'name' parameters, or ensure the file exists.")
  }

  ############## Check for openxlsx Dependency ##############
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed.\n",
         "Install it with: install.packages('openxlsx')")
  }

  ############## Read Mandatory Sheets ##############
  # Get all sheet names in the Excel file
  sheet_names <- tryCatch({
    openxlsx::getSheetNames(full_path)
  }, error = function(e) {
    stop("Failed to read sheets from '", full_path, "': ", e$message)
  })

  # Read "Category.Variable.Level" sheet
  if (!"Category.Variable.Level" %in% sheet_names) {
    stop("Mandatory sheet 'Category.Variable.Level' not found in the Excel file.")
  }
  cat_data <- tryCatch({
    openxlsx::read.xlsx(full_path, sheet = "Category.Variable.Level", detectDates = TRUE)
  }, error = function(e) {
    stop("Error reading 'Category.Variable.Level' sheet: ", e$message)
  })

  # Read "Total.Variable" sheet
  if (!"Total.Variable" %in% sheet_names) {
    stop("Mandatory sheet 'Total.Variable' not found in the Excel file.")
  }
  total_data <- tryCatch({
    openxlsx::read.xlsx(full_path, sheet = "Total.Variable", detectDates = TRUE)
  }, error = function(e) {
    stop("Error reading 'Total.Variable' sheet: ", e$message)
  })

  ############## Validate Sheet Data ##############
  if (nrow(cat_data) == 0) {
    warning("Sheet 'Category.Variable.Level' is empty. Update may be incomplete.")
  }
  if (nrow(total_data) == 0) {
    warning("Sheet 'Total.Variable' is empty. Update may be incomplete.")
  }

  ############## Update Object ##############
  Object[["update"]] <- list(
    level.data = cat_data,
    summary = total_data,
    import_info = list(
      file_path = full_path,
      import_time = Sys.time()
    )
  )

  message("Successfully updated 'Object$update' with data from: ", full_path)
  return(invisible(Object))
}
