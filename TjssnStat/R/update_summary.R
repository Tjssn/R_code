super_param_update <-function(Object=NULL,Path=NULL,name=NULL ,strat_col="start"){
  #######更新excel#######
  signal_step("Step1 导入验证中...")
  datapack2   <-excel_in(Object=Object,
                         Path=Path,
                         name=name)


  #######更参数检查#######
  signal_step("Step2 更新变量列表中...")
  compare_datasets_variable(Object=datapack2)
  cat("\n")
  signal_step("Step3 更新水平列表中...")
  compare_datasets_level(Object=datapack2)
  ######更新数据集########
  signal_step("Step4 更新水平列表...")
  NEWrawdata <- update_raw_with_level(
    Object=datapack2,
    strat_col=strat_col)
  datapack2$update$RAW <-NEWrawdata
  return(datapack2)

}


