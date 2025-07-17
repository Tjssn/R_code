get_device_id2 <- function() {
  os <- Sys.info()["sysname"]
  uuid <- character(0)  # 初始化空向量

  if (os == "Windows") {
    # Windows：处理可能的多行输出，提取非空且非标题的行
    cmd <- 'wmic csproduct get uuid'
    result <- system(cmd, intern = TRUE)
    # 过滤空行和标题行，取第一个有效结果
    valid_lines <- trimws(result[!result %in% c("", "UUID")])
    uuid <- if (length(valid_lines) > 0) valid_lines[2] else ""

  } else if (os == "Darwin") {  # macOS
    cmd <- 'system_profiler SPHardwareDataType | grep "Hardware UUID"'
    result <- system(cmd, intern = TRUE)
    # 提取UUID（取第一个匹配结果）
    uuid <- if (length(result) > 0) trimws(sub("Hardware UUID: ", "", result[1])) else ""

  } else if (os == "Linux") {
    uuid_file <- "/sys/class/dmi/id/product_uuid"
    if (file.exists(uuid_file)) {
      uuid <- readLines(uuid_file, n = 1, warn = FALSE)  # 只读第一行
    } else {
      # 生成伪UUID（基于系统信息的MD5哈希）
      sys_info <- paste(Sys.info(), collapse = "")
      uuid <- paste0("linux-", substr(digest::digest(sys_info, algo = "md5"), 1, 36))
    }
    uuid <- trimws(uuid)  # 去除可能的空格

  } else {
    stop("不支持的操作系统：", os)
  }

  # 最终检查：确保返回单个字符串
  if (length(uuid) == 0 || uuid == "") {
    warning("无法获取有效的UUID")
    return(NA_character_)
  } else {
    return(toupper(uuid))  # 统一转为大写
  }
}
