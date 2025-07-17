get_device_id <- function() {
  os <- Sys.info()["sysname"]

  if (os == "Windows") {
    # 方案1：尝试读取注册表中的系统序列号（无需wmic）
    # 注册表路径：HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion
    tryCatch({
      # 使用reg query命令读取系统序列号（Windows内置命令，多数系统可用）
      cmd <- 'reg query "HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion" /v "ProductId"'
      result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)

      # 提取ProductId（系统产品ID，具备一定唯一性）
      product_id_line <- grep("ProductId", result, value = TRUE)
      if (length(product_id_line) > 0) {
        device_id <- trimws(sub(".*REG_SZ\\s+", "", product_id_line))
        if (nchar(device_id) > 0) {
          return(paste0("WIN-", toupper(device_id)))
        }
      }
    }, error = function(e) {})

    # 方案2：若注册表读取失败，基于系统信息生成哈希（保底方案）
    sys_info <- paste(
      Sys.info()["nodename"],    # 计算机名
      Sys.info()["release"],     # 系统版本
      Sys.getenv("USERNAME"),    # 用户名
      collapse = "-"
    )
    device_id <- paste0("WIN-HASH-", substr(digest::digest(sys_info, algo = "sha1"), 1, 16))
    return(device_id)

  } else if (os == "Darwin") {  # macOS
    cmd <- 'system_profiler SPHardwareDataType | grep "Hardware UUID"'
    result <- system(cmd, intern = TRUE)
    device_id <- trimws(sub("Hardware UUID: ", "", result))
    return(paste0("MAC-", toupper(device_id)))

  } else if (os == "Linux") {
    uuid_file <- "/sys/class/dmi/id/product_uuid"
    if (file.exists(uuid_file)) {
      device_id <- readLines(uuid_file, n = 1, warn = FALSE)
      return(paste0("LIN-", toupper(trimws(device_id))))
    } else {
      sys_info <- paste(Sys.info(), collapse = "")
      device_id <- paste0("LIN-HASH-", substr(digest::digest(sys_info, algo = "sha1"), 1, 16))
      return(device_id)
    }

  } else {
    stop("不支持的操作系统：", os)
  }
}


