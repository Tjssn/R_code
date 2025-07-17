

compare_datasets_level <- function(Object) {
  A <- Object$super_table$result$data_super[["Category.Variable.Level"]]
  B <- Object$update$level.data

  # 辅助函数：统一日志格式


  ############## 1. 列名检查 ##############
  signal_step("==== 列名一致性检查 ====")
  cols_A <- colnames(A)
  cols_B <- colnames(B)

  if (!identical(cols_A, cols_B)) {
    only_A <- setdiff(cols_A, cols_B)
    only_B <- setdiff(cols_B, cols_A)
    signal_warning("列名不一致")
    print_info(paste0("A独有的列：", if (length(only_A) == 0) "无" else paste(only_A, collapse = ", ")))
    print_info(paste0("B独有的列：", if (length(only_B) == 0) "无" else paste(only_B, collapse = ", ")))
    print_info("----------------------")
    return(invisible(NULL))
  } else {
    signal_success("列名完全一致（包括顺序）")
  }

  ############## 2. 行数检查 ##############
  signal_step("==== 行数一致性检查 ====")
  rows_A <- nrow(A)
  rows_B <- nrow(B)

  if (rows_A != rows_B) {
    signal_warning("行数不一致")
    print_info(paste0("原参数行数：", rows_A))
    print_info(paste0("更改参数行数：", rows_B))
    print_info("----------------------")
    return(invisible(NULL))
  } else {
    signal_success(paste0("行数一致，均为 ", rows_A, " 行"))
  }

  ############## 3. 新增：检查同一个variable是否存在相同的start（允许此情况） ##############
  signal_step("==== variable与start组合检查 ====")
  # 确保存在"variable"和"start"列（根据实际列名调整，若列名不同需修改）
  if (all(c("variable", "start") %in% cols_A)) {
    # 提取A和B中"variable"与"start"的组合
    combo_A <- A[, c("variable", "start")]
    combo_B <- B[, c("variable", "start")]

    # 检查A中是否有相同variable下的相同start
    dup_A <- combo_A %>%
      dplyr::group_by(variable, start) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::distinct(variable, start)
    if (nrow(dup_A) > 0) {
      signal_error(paste0("A中存在", nrow(dup_A), "组相同variable下的相同start(不允许)："))
      print(dup_A, row.names = FALSE)
      return(NULL)
    } else {
      signal_success("A中同一个variable的start均唯一")
    }

    # 检查B中是否有相同variable下的相同start
    dup_B <- combo_B %>%
      dplyr::group_by(variable, start) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::distinct(variable, start)
    if (nrow(dup_B) > 0) {
      signal_error(paste0("B中存在", nrow(dup_B), "组相同variable下的相同start（不允许）："))
      print(dup_B, row.names = FALSE)
      return(NULL)
    } else {
      signal_success("B中同一个variable的start均唯一")
    }
  } else {
    signal_warning("未找到\"variable\"或\"start\"列，跳过组合检查")
  }

  ############## 4. 单元格内容检查（按列汇总差异） ##############
  signal_step("==== 单元格内容差异检查 ====")
  diff_list <- list()

  for (col in cols_A) {
    # 处理因子型变量（转为字符型比较）
    col_A <- if (is.factor(A[[col]])) as.character(A[[col]]) else A[[col]]
    col_B <- if (is.factor(B[[col]])) as.character(B[[col]]) else B[[col]]

    # 定位差异行（排除NA一致的情况）
    diff_rows <- which(!(col_A == col_B | (is.na(col_A) & is.na(col_B))))

    if (length(diff_rows) > 0) {
      val_A <- ifelse(is.na(col_A[diff_rows]), "NA", as.character(col_A[diff_rows]))
      val_B <- ifelse(is.na(col_B[diff_rows]), "NA", as.character(col_B[diff_rows]))

      diff_list[[col]] <- data.frame(
        行号 = diff_rows,
        原参数值 = val_A,
        更改参数值 = val_B,
        stringsAsFactors = FALSE
      )
    }
  }

  # 输出内容差异结果
  if (length(diff_list) == 0) {
    signal_success("未发现内容差异，所有单元格值完全一致")
  } else {
    total_diff <- sum(sapply(diff_list, nrow))
    signal_success(paste0("共发现", total_diff, "处内容修改，按列汇总如下："))
    for (col in names(diff_list)) {
      cat("\n")
      signal_success(paste0("列「", col, "」的修改（共", nrow(diff_list[[col]]), "处）："))
      # print(diff_list[[col]], row.names = FALSE)
    }
  }

  signal_step("==== 检查完成 ====")
  return(invisible(NULL))
}
