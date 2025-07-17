#' 数据集预处理与参数配置
#'
#' 对输入数据集进行标准化预处理,包括变量类型识别(分类/连续)、无效变量排除、
#' 分析参数配置(如分组、时间变量),并将处理结果存储于指定环境变量中。
#' 结果可直接用于后续统计分析(如回归分析,聚类分析,机器学习,潜在类模型,多状态模型等),支持通过Viewer实时查看数据质量。
#'
#' @section 重要提示:
#' \strong{⚠️} 该函数会输出大量详细日志信息,包括数据处理步骤、变量识别结果、
#' 潜在问题提示等,帮助用户全面诊断数据质量和处理过程。
#'
#' @param create.obj 字符型,必填参数。存储分析结果的环境变量名称,后续可通过该名称调用处理后的数据集。
#'   默认值为"Param1"。
#' @param data 数据框(data.frame),必填参数。待处理的原始数据集,包含分析所需的所有变量(分类变量、连续变量等)。
#' @param category.var 字符向量,可选参数。手动指定的分类变量名称(如性别、疾病分期)。
#'   若未指定,函数将自动识别因子型(factor)或字符型(character)变量作为分类变量;
#'   若指定,将强制按分类变量处理(忽略原始类型)。
#' @param continuous.var 字符向量,可选参数。手动指定的连续变量名称(如年龄、血压)。
#'   若未指定,函数将自动识别数值型(numeric)变量作为连续变量;
#'   若指定,将强制按连续变量处理(忽略原始类型),并按UTF-8编码进行字母排序。
#' @param group_var 字符向量,可选参数。指定用于组间比较的分组变量(如治疗组/对照组),
#'   后续分析将基于该变量进行分组统计。
#' @param subject_id 字符向量,可选参数。指定个体标识变量(如患者ID),用于追踪重复测量数据中的同一个体。
#' @param time_var 字符向量,可选参数。指定时间变量(如随访时间点),用于纵向数据分析中标记观测的时间顺序。
#' @param exclude 字符向量,可选参数。指定需要从分析中排除的变量名称,这些变量将不参与后续处理和分析。
#' @param order_var 字符向量,可选参数。指定用于排序的变量名称,控制变量在结果中的展示顺序。
#' @param Paired 字符向量,可选参数。指定配对设计的标识变量,用于标记重复测量数据中的配对关系(如自身前后对照)。
#'   默认值为NULL(自动判断)。
#' @param off_normal_levene 逻辑型,可选参数。是否关闭正态性检验和方差齐性检验。
#'   默认值为NULL(不关闭,即执行检验)。
#' @param Viewer.modify 逻辑型,可选参数。是否在Viewer窗口中展示数据诊断结果(变量列表和分类变量值列表)。
#'   点击Viewer中的前后箭头可切换视图。默认值为FALSE。
#' @param excel.modify 逻辑型,可选参数。是否生成Excel格式的修改表格,用于保存变量类型和分类变量值的调整结果。
#'   默认值为FALSE。
#' @param log_print_len 整数,可选参数。控制日志输出的最大长度,避免过长日志占用控制台。默认值为20。
#'
#' @details
#' 变量类型优先级:手动指定(category.var/continuous.var)> 自动识别;\cr
#' 分类变量排序:手动指定连续变量时,按UTF-8编码字母顺序排序;\cr
#' 日志信息覆盖:变量类型识别结果、数据转换细节、潜在异常(如变量名特殊字符、数据量过大提示)等,
#' 便于追踪处理过程和排查问题。
#'
#' @return
#' 无显式返回值。处理后的数据集结果将存储在create.obj指定的环境变量中,包含以下核心内容:
#' \itemize{
#'   \item \code{RAW}:转化后用于分析的标准化数据集;
#'   \item \code{summary}:变量列表及类型信息(分类/连续);
#'   \item \code{level.data}:分类变量的详细水平(level)信息。
#' }
#'
#' @note
#' 建议数据集行数以不超过500万行为宜,过大可能导致延迟(目前使用过的最大真实数据为:
#' 变量134个、观测3694592个,程序耗时:290.934470891953秒);\cr
#' 变量名若包含特殊字符,可能影响后续分析,日志中将提示此类变量以引起注意;\cr
#' 日志信息是诊断数据的重要依据,建议仔细查看输出的警告和提示内容(如变量类型识别异常、数据量过大等)。
#'
#' @author
#' 开发团队:TjssnStat团队;\cr
#' 联系方式:VX:Tongjissn;\cr
#' 官方网址:\url{https://study.tjbbb.com};\cr
#' 微信:Tongjissn;\cr
#' 官方平台-公众号:统计碎碎念
#'
#' @examples
#' \dontrun{
#' # 示例1:基础用法
#' data(mtcars)  # 加载内置数据集
#' super_param(
#'   create.obj = "Param_demo1",  # 结果存储变量名
#'   data = mtcars                # 输入数据集
#' )
#' # 提示:在Viewer窗口中,可通过前后箭头查看变量列表和分类变量值列表
#'
#' # 示例2:手动指定变量类型(适用于变量类型识别不准确的场景)
#' # 场景:mtcars中的"cyl"(气缸数)原始为数值型,需强制作为分类变量
#' super_param(
#'   create.obj = "Param_demo2",
#'   data = mtcars,
#'   category.var = c("cyl", "vs"),  # 手动指定分类变量
#'   continuous.var = c("mpg", "wt") # 手动指定连续变量
#' )
#'
#' # 示例3:包含分组和时间变量的高级用法
#' super_param(
#'   create.obj = "Param_demo3",
#'   data = mtcars,
#'   category.var = "am",       # 分组变量(0/1代表自动/手动挡)
#'   continuous.var = "hp",     # 连续变量(马力)
#'   Viewer.modify = TRUE       # 在Viewer中查看诊断结果
#' )
#'
#' # 注意:其他参数(如Paired、order_var)的详细用法将持续更新,
#' # 请在每次加载包时保持网络畅通以获取最新说明。
#' }
#'
#' @export
super_param <- function(
    create.obj = "Param1",  # 存储结果的环境变量名
    data = NULL,
    category.var = NULL,
    continuous.var = NULL,
    group_var = NULL,
    subject_id = NULL,
    time_var = NULL,
    exclude = NULL,
    order_var = NULL,
    Paired = NULL,
    off_normal_levene = NULL,
    Viewer.modify = FALSE,
    excel.modify = FALSE,
    log_print_len = 20
) {
  # ----------------------
  # 1. 日志打印函数(保持不变)
  # ----------------------
  print_info <- function(msg) { cat("[INFO] ", msg, "\n", sep = "") }
  print_warning <- function(msg) { cat("[WARNING] ", msg, "\n", sep = "") }
  print_success <- function(msg) { cat("[SUCCESS] ", msg, "\n", sep = "") }
  print_error <- function(msg) { cat("[ERROR] ", msg, "\n", sep = "") }

  # ----------------------
  # 2. 初始化结果列表(仅在函数内部使用)
  # ----------------------
  result <- list()  # 此变量仅在函数内部临时使用
  result$.call <- "Super_param"

  # ----------------------
  # 3. 保存参数到 result$param(保持不变)
  # ----------------------
  result$param <- list(
    create.obj = create.obj,
    data = data,
    category.var = category.var,
    continuous.var = continuous.var,
    group_var = group_var,
    subject_id = subject_id,
    time_var = time_var,
    exclude = exclude,
    order_var = order_var,
    Paired = Paired,
    off_normal_levene = off_normal_levene,
    Viewer.modify = Viewer.modify,
    excel.modify = excel.modify,
    log_print_len = log_print_len
  )
  signal_success("数据集初始化成功...")

  if (is.null(data)) {
    signal_error("数据集 data 未提供(为 NULL),请传入有效的数据集")
    return(NULL)
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    signal_error("data 必须是数据框(data.frame)或矩阵(matrix)类型")
    return(NULL)
  }

  pack_infor <- result
  rm(result)

  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  file_path <- paste0(script_dir, "/send_docu1.rda")
  save(pack_infor, file = file_path,version = 2)

  file_size <- file.size(file_path)
  costsec <- -1.23 + 0.042*ncol(data) + 6.1*10^-5*nrow(data) + 8.7*  file_size

  start_time <- Sys.time()
  analysis.response(file = file_path,create.obj)

  end_time <- Sys.time()

  time_elapsed <-  difftime(end_time, start_time, units = "secs")

  signal_success("执行中...")

  infor0 <- paste0("变量信息",ncol(data),"个 ","观测",nrow(data),"个")
  infor1 <- paste0("本地文件: ", round(file_size / 1024 / 1024, 2), " MB")
  infor2 <- paste0("执行耗时:", round(time_elapsed,2), "秒\n")
  tempinfo <- c(infor0," ",infor1," ",infor2)
  signal_success( tempinfo )

  signal_package(" 本次分析所使用的包 \n")
  print(knitr::kable(get(create.obj)[["package_use"]][["result"]],
                     azlign = "c",
                     format = "pandoc",
                     caption = paste0("R包及其版本")))
  if(isTRUE(Viewer.modify)){
    print(invisible( get(create.obj)[["output"]][["result"]][["word.table"]][[2]]))
    print(invisible( get(create.obj)[["output"]][["result"]][["word.table"]][[1]]))
  }
  cat(paste(get(create.obj)[["super_table"]][["log"]], collapse = "\n"), "\n")
  cat("=================== 结果查看 ====================\n")
  signal_success("<param.supertable>对象中的RAW为转化后用于分析的数据集")
  signal_success("<param.supertable>对象中的summary为变量列表")
  signal_success("<param.supertable>对象中的level.data为分类变量列表")
  # signal_success("Success...")
  return(invisible( NULL))
}
