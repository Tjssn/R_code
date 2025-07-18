# R_code
# Information
<div align="center">
  <img src="https://github-readme-stats.vercel.app/api?username=Cwd295645351&show_icons=true&theme=transparent" /> 
</div>

<span > 
  <img alt="Static Badge" src="https://img.shields.io/badge/Vue-%2342b883?style=flat-square&logo=Vue&logoColor=%23fff"> 
  <img alt="Static Badge" src="https://img.shields.io/badge/TypeScript-%230072b3?style=flat-square&logo=TypeScript&logoColor=%23fff"> 
  <img src="https://img.shields.io/badge/-JavaScript-F7DF1E?style=flat-square&logo=javascript&logoColor=white" /> 
  <img src="https://img.shields.io/badge/-HTML5-E34F26?style=flat-square&logo=html5&logoColor=white" /> 
  <img src="https://img.shields.io/badge/-CSS3-1572B6?style=flat-square&logo=css3" /> 
  <img alt="Static Badge" src="https://img.shields.io/badge/Webpack-%230072b3?style=flat-square&logo=webpack&logoColor=%23fff"> 
  <img alt="Static Badge" src="https://img.shields.io/badge/Vite-%239a60fe?style=flat-square&logo=vite&logoColor=%23fff"> 
  <img alt="Static Badge" src="https://img.shields.io/badge/Sass-%23c66394?style=flat-square&logo=Sass&logoColor=%23fff"> 
  <img alt="Static Badge" src="https://img.shields.io/badge/Visual_Studio_Code-007ACC?style=flat-square&logo=Visual-Studio-Code&logoColor=white"> 
  <img alt="Static Badge" src="https://img.shields.io/badge/Git-F05032?style=flat-square&logo=Git&logoColor=white">  
</span>

# TjssnStat

<div align="center">
  <img src="https://img.shields.io/badge/R-4.5.1-276DC3?style=flat-square&logo=R&logoColor=white" />
  <img src="https://img.shields.io/badge/tidyverse-2.0.0-56B4E9?style=flat-square&logo=RStudio&logoColor=white" />
  <img src="https://img.shields.io/badge/Excel-Interaction-217346?style=flat-square&logo=Microsoft-Excel&logoColor=white" />
  <img src="https://img.shields.io/badge/License-MIT-green?style=flat-square" />
  <br>
  <p>数据集预处理与统计分析工具包 | 支持医学、社会学、数学等多领域数据分析</p>
</div>

<div align="center">
  <img src="https://github-readme-stats.vercel.app/api/pin/?username=你的GitHub用户名&repo=TjssnStat&theme=transparent" />
</div>


## 🌟 核心功能

`TjssnStat` 专注于简化数据分析流程，提供从数据预处理到统计建模的一站式解决方案：

- **数据标准化预处理**  
  自动识别变量类型（分类/连续）、排除无效变量、配置分析参数（分组/时间变量等），输出可直接用于后续分析的标准化数据集。

- **多场景统计支持**  
  兼容生存分析、多状态模型、亚组分析等医学统计场景，同时支持社会学调查数据、数学建模数据的快速处理。

- **可视化诊断与交互**  
  通过 Viewer 实时查看数据质量报告，支持 Excel 格式导出调整结果，方便非编程用户协作。

- **高效处理大规模数据**  
  已验证可处理千万级观测数据（134 变量 + 369 万观测耗时约 290 秒），满足实际科研数据需求。


## 🚀 快速开始

### 安装包
```r
# 从GitHub安装（示例）
devtools::install_github("Tjssn/package name")

# 加载包
library(TjssnStat)


# 示例1：基础用法
data(mtcars)  # 加载内置数据集
super_param(
  create.obj = "Param_demo1",  # 结果存储变量名
  data = mtcars                # 输入数据集
)
# 提示：在Viewer窗口中，可通过前后箭头查看变量列表和分类变量值列表

# 示例2：手动指定变量类型（适用于变量类型识别不准确的场景）
# 场景：mtcars中的"cyl"（气缸数）原始为数值型，需强制作为分类变量
super_param(
  create.obj = "Param_demo2",
  data = mtcars,
  category.var = c("cyl", "vs"),  # 手动指定分类变量
  continuous.var = c("mpg", "wt") # 手动指定连续变量
)

# 示例3：包含分组和时间变量的高级用法
super_param(
  create.obj = "Param_demo3",
  data = mtcars,
  category.var = "am",       # 分组变量（0/1代表自动/手动挡）
  continuous.var = "hp",     # 连续变量（马力）
  Viewer.modify = TRUE       # 在Viewer中查看诊断结果
)
