#        
analysis.response <- function(file, obj = "SuperParam", progress = NULL) {

  response <- POST(
    url = "https://tjssn.cpolar.top/upload_R",
    body = list(
      uuid = get_device_id2(),
      file = upload_file(file)
    ),
    encode = "multipart",
    progress = progress
  )

  if (httr::status_code(response) == 200) {
    temp_file <- tempfile(fileext = ".rda")
    writeBin(httr::content(response, "raw"), temp_file)
    loaded_env <- new.env()
    load(temp_file, envir = loaded_env)
    for (obj_name in ls(loaded_env)) {
      assign(obj, get(obj_name, envir = loaded_env), envir = .GlobalEnv)
    }
    unlink(temp_file)
  } else {
    cat(paste("        :", httr::status_code(response), "\n"))
    cat("    :", httr::content(response, "text"), "\n")
  }
}
