update_raw_with_level <- function(Object, strat_col = "start") {
  ############## 辅助日志函数 ##############
  RAW <- Object$super_table$result$RAW
  level_df <- Object[["update"]][["level.data"]]


  ############## 初始化与参数检查（增加容错性） ##############
  print_info("=== 开始执行数据集更新 ===")
  print_info(paste0("输入：RAW（", nrow(RAW), "行, ", ncol(RAW), "列），level数据集（", nrow(level_df), "行）"))

  # 必要列定义
  required_cols <- c("variable", "level", "level_label", strat_col)
  missing_cols <- setdiff(required_cols, colnames(level_df))

  # 处理缺失列：警告但不退出，仅使用存在的列
  if (length(missing_cols) > 0) {
    signal_warning(paste0("<super_param>update数据集缺少必要列：", paste(missing_cols, collapse = ", ")))
    signal_warning("将使用现有列尝试继续执行，可能导致部分功能异常")
    # 仅保留存在的列作为有效列
    valid_cols <- intersect(required_cols, colnames(level_df))
    if (length(valid_cols) == 0) {
      log_error("没有可用的必要列，无法继续执行")
      signal_error("数据不完整") # 完全无有效列时才退出
      return(NULL)
    }
  } else {
    valid_cols <- required_cols
  }

  # 提取变量列表（基于有效列）
  if ("variable" %in% valid_cols) {
    unique_vars <- unique(level_df$variable)
    print_info(paste0("待处理变量总数：", length(unique_vars), "个（基于现有列）"))
  } else {
    log_error("缺少核心列'variable'，无法识别待处理变量")
    signal_error("数据不完整")
  }

  updated_RAW <- RAW
  total_updated <- 0  # 总更新计数
  updated_vars <- 0   # 有更新的变量计数
  skipped_vars <- 0   # 跳过的变量计数

  ############## 按变量处理 ##############
  for (var in unique_vars) {
    var_updated <- 0  # 变量内更新计数
    update_details <- c()  # 记录更新详情

    # 检查变量是否存在于RAW中
    if (!var %in% colnames(updated_RAW)) {
      skipped_vars <- skipped_vars + 1
      next
    }

    # 提取当前变量的映射关系（仅使用有效列）
    var_levels <- subset(level_df, variable == var, select = valid_cols)

    # 处理映射关系（仅当'level'和'level_label'都存在时才执行替换）
    if (all(c("level", "level_label") %in% valid_cols)) {
      for (i in 1:nrow(var_levels)) {
        original_val <- var_levels$level[i]
        new_val <- var_levels$level_label[i]

        if (is.na(original_val) || is.na(new_val) || original_val == new_val) {
          next  # 跳过NA或相同值
        }

        # 计算更新行数
        idx <- (updated_RAW[[var]] == original_val) & !is.na(updated_RAW[[var]])
        update_count <- sum(idx)

        if (update_count > 0) {
          updated_RAW[idx, var] <- new_val
          var_updated <- var_updated + update_count
          update_details <- c(update_details,
                              paste0("映射", i, "（", original_val, "→", new_val, "）：", update_count, "行"))
        }
      }
    } else {
      signal_warning(paste0("变量「", var, "」：缺少'level'或'level_label'，无法执行值替换"))
    }

    # 处理因子转换（仅当'strat_col'存在时才排序）
    if (strat_col %in% valid_cols) {
      # 按strat_col排序因子水平
      sorted_labels <- if ("level_label" %in% valid_cols) {
        unique(var_levels[order(var_levels[[strat_col]]), "level_label"])
      } else {
        # 缺少'level_label'时使用原始level作为因子水平
        unique(var_levels[order(var_levels[[strat_col]]), "level"])
      }
      # 转换为因子
      updated_RAW[[var]] <- factor(updated_RAW[[var]], levels = sorted_labels)
    } else {
      signal_warning(paste0("变量「", var, "」：缺少 strat_col（", strat_col, "），无法排序因子水平"))
      # 不排序，直接转换为因子
      updated_RAW[[var]] <- as.factor(updated_RAW[[var]])
    }

    # 打印有更新的变量日志
    if (var_updated > 0) {
      updated_vars <- updated_vars + 1
      total_updated <- total_updated + var_updated

      print_info(paste0("变量「", var, "」："))
      print_info(paste0("  总更新：", var_updated, "处（", length(update_details), "组映射）"))
      print_info(paste0("  更新详情：", paste(update_details, collapse = "；")))
      if (strat_col %in% valid_cols) {
        print_info(paste0("  因子水平：", paste(sorted_labels, collapse = "→"), "\n"))
      }
    }
  }

  ############## 总结日志 ##############
  signal_success("=== 处理完成 ===")
  print_success(paste0("有效更新：", updated_vars, "个变量，共", total_updated, "处修改"))
  print_success(paste0("未更新变量：", length(unique_vars) - updated_vars - skipped_vars, "个（仅转换为因子）"))
  print_success(paste0("跳过变量：", skipped_vars, "个（不在RAW数据集中）"))
  print_success(paste0("输出数据集：", nrow(updated_RAW), "行, ", ncol(updated_RAW), "列"))

  return(updated_RAW)
}
