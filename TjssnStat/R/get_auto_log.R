get_package_info_formatted <- function(packages,
                                       lib_path = NULL,
                                       install_if_missing = FALSE,
                                       quiet = FALSE,
                                       format_version = TRUE) {
  # 检查输入
  if (!is.character(packages) || length(packages) == 0) {
    stop("packages必须是非空字符向量")
  }

  # 保存原始库路径（若指定新路径）
  original_libs <- NULL
  if (!is.null(lib_path)) {
    original_libs <- .libPaths()
    .libPaths(lib_path)
  }

  # 定义获取单个包信息的函数
  get_single_package_info <- function(pkg) {
    info <- c(
      package = pkg,
      version = NA_character_,
      loaded = FALSE
    )

    # 检查是否已加载
    if (pkg %in% loadedNamespaces()) {
      version_obj <- packageVersion(pkg)
      info["version"] <- if (format_version) {
        # 格式化为点分隔字符串
        paste(version_obj, collapse = ".")
      } else {
        # 保留原始版本对象
        as.character(version_obj)
      }
      info["loaded"] <- TRUE
      return(info)
    }

    # 尝试加载包
    load_success <- require(pkg, character.only = TRUE, quietly = TRUE)

    if (load_success) {
      version_obj <- packageVersion(pkg)
      info["version"] <- if (format_version) {
        paste(version_obj, collapse = ".")
      } else {
        as.character(version_obj)
      }
      info["loaded"] <- TRUE
      return(info)
    }

    # 处理缺失包
    if (install_if_missing) {
      if (!quiet) message(paste0("安装并加载 ", pkg, " 到 ", lib_path))
      tryCatch({
        install.packages(pkg, lib = lib_path, dependencies = TRUE)
        load_success <- require(pkg, character.only = TRUE, quietly = TRUE)
        if (load_success) {
          version_obj <- packageVersion(pkg)
          info["version"] <- if (format_version) {
            paste(version_obj, collapse = ".")
          } else {
            as.character(version_obj)
          }
          info["loaded"] <- TRUE
        }
      }, error = function(e) {
        if (!quiet) message(paste0("安装失败: ", e))
      })
    }

    return(info)
  }

  # 批量获取包信息
  package_info_list <- lapply(packages, get_single_package_info)

  # 恢复原始库路径
  if (!is.null(original_libs)) {
    .libPaths(original_libs)
  }

  # 转换为数据框
  info_df <- do.call(rbind, package_info_list)
  info_df <- as.data.frame(info_df, stringsAsFactors = FALSE)

  # 格式化数据类型
  info_df$loaded <- as.logical(info_df$loaded)

  # 显示摘要
  if (!quiet) {
    loaded_count <- sum(info_df$loaded)
    message(paste0("成功加载 ", loaded_count, "/", nrow(info_df), " 个包"))
  }

  return(info_df)
}
