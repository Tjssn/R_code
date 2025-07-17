compare_datasets_variable <- function(Object) {
  A <- Object$super_table$result$data_super$Total.Variable
  B <- Object$update$summary
  ############## 1. 列名检查 ##############
  signal_step("==== 列名一致性检查 ====")
  cols_A <- colnames(A)
  cols_B <- colnames(B)

  if (!identical(cols_A, cols_B)) {
    only_A <- setdiff(cols_A, cols_B)
    only_B <- setdiff(cols_B, cols_A)
    signal_warning("列名不一致")
    print_info(paste0("  A独有的列：", if (length(only_A) == 0) "无" else paste(only_A, collapse = ", ")))
    print_info(paste0("  B独有的列：", if (length(only_B) == 0) "无" else paste(only_B, collapse = ", ")))
    print_info("\n----------------------")
    return(invisible(NULL))
  } else {
    signal_success("列名完全一致(包括顺序)")
  }

  ############## 2. 行数检查 ##############
  print_info("行数一致性检查")
  rows_A <- nrow(A)
  rows_B <- nrow(B)

  if (rows_A != rows_B) {
    signal_warning("行数不一致")
    print_info(paste0("  原参数行数：", rows_A))
    print_info(paste0("  更改参数行数：", rows_B))
    print_info("\n----------------------")
    return(invisible(NULL))
  } else {
    signal_success(c(paste0("行数一致，均为 ", rows_A, " 行")))
  }

  ############## 3. 单元格内容检查（按列汇总差异） ##############
  print_info("单元格内容差异检查(按列汇总)")
  diff_list <- list()  # 存储按列分类的差异

  for (col in cols_A) {
    # 处理因子型变量
    col_A <- if (is.factor(A[[col]])) as.character(A[[col]]) else A[[col]]
    col_B <- if (is.factor(B[[col]])) as.character(B[[col]]) else B[[col]]

    # 定位差异行
    diff_rows <- which(!(col_A == col_B | (is.na(col_A) & is.na(col_B))))

    if (length(diff_rows) > 0) {
      # 提取差异行的A/B值
      val_A <- ifelse(is.na(col_A[diff_rows]), "NA", as.character(col_A[diff_rows]))
      val_B <- ifelse(is.na(col_B[diff_rows]), "NA", as.character(col_B[diff_rows]))

      # 存储到列表（列名为键）
      diff_list[[col]] <- data.frame(
        行号 = diff_rows,
        原参数值 = val_A,
        更改参数值 = val_B,
        stringsAsFactors = FALSE
      )
    }
  }

  # 输出汇总结果
  if (length(diff_list) == 0) {
    signal_success("未发现内容差异，所有单元格值完全一致")
  } else {
    total_diff <- sum(sapply(diff_list, nrow))
    signal_success(c(paste0("共发现 ", total_diff, " 处内容修改，按列汇总如下：")))

    # 遍历每列差异并打印表格
    for (col in names(diff_list)) {
      signal_success(c("[INFO] 列「", col, "」的修改（共", nrow(diff_list[[col]]), "处）：\n", sep = ""))
      # 打印表格（使用format格式化列宽）
      # print(format(diff_list[[col]], justify = "left"), row.names = FALSE)
    }
  }

  ############## 结束日志 ##############
}
