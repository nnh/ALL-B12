# ALL-B12 基本的な情報をマージするプログラム
# Mamiko Yonejima
# 20170817 

# 関数の設定
# 列抽出とAEgrade切り離し、ファイルの出力をする関数
MakeDataSet <- function(dataframe){
  if(flg == 1){
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, 
                            c(1:10, 15:length(colnames(dataframe)))]
    flowsheet_df <- datecut_df[, c(1:10, seq(11, length(colnames(datecut_df)), by = 2))]
  }else{
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, 
                            c(1:10, 15:length(colnames(dataframe)))]
    flowsheet_df <- datecut_df[format(as.Date(datecut_df$作成日), "%Y%m%d") >= kDateShimekiri_start,
                               c(1:10, seq(11, length(colnames(datecut_df)), by = 2))]
  }
  merge_df <- merge(base_df, flowsheet_df, by = "症例登録番号", all.y = T)
  number <- match("貧血", colnames(merge_df))  # 貧血の行数を検索
  numberplus3 <- number+3
  numberplus4 <- number+4
  numberplus5 <- number+5
  numberminus1 <- number-1
  ae_grade_0 <- merge_df[, number:numberplus3]
  ae_grade_1 <- merge_df[, c(numberplus5:length(colnames(merge_df)))]
  ae_grade_dxt_0 <- sapply(ae_grade_0, substring, 10, 10)
  ae_grade_dxt_1 <- sapply(ae_grade_1, substring, 10, 10)
  上記4項目の血液毒性を除く有害事象有無 <- merge_df[, numberplus4]
  result <- cbind(merge_df[, c(1:numberminus1)], ae_grade_dxt_0, 上記4項目の血液毒性を除く有害事象有無, ae_grade_dxt_1)
  result[is.na(result)] <- ""
  write.csv(result, paste0("../output/flowsheet",i,".csv"), row.names =  F)
}　 
# mergeせず、列抽出のみの関数
MakeDataSet_1 <- function(dataframe){
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, ]
    datecut_df[is.na(datecut_df)] <- ""
    datecut_df[, c(1:11, seq(13, length(colnames(datecut_df)), by = 2)) ] 
}  

# #### 設定 ######################
# # output,rawdataはaronas上にて入出力する
# prtpath <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/04.03.02 定期モニタリングレポート/第10回/R/precleaning"
# # 締め切り日、ダウンロード日の
# flg <- 1  # 1:締め切り日1つ設定バージョン、2:定モニバージョン（startの日も設定）
# kDateShimekiri_start <- "20170601"  # flg==2の時に設定
# kDateShimekiri <- "20171130"
# kDownLoadDate <- "_171116_1434"
# kJplsg <- "JPLSG_registration_171109_1228.csv"
# #########################################################
# よみこみ
list <- list.files(paste0(prtpath, "./rawdata"))
file.name <- sub(paste0(kDownLoadDate,".*"), "", list)
df.name <- gsub("ALL-B12_", "",file.name)
setwd(paste0(prtpath, "./rawdata"))
for (i in 1:length(list)) {
   assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}
# JPLSG_registrationを単独でよみこむ
jp <- read.csv(paste0(prtpath, "/rawdata/", kJplsg), as.is=T, na.strings = c(""))
dxt_bd <- jp[, c(8, 37)]

# リスク、中止などの情報を含んだ基本的なデータセットの作成
cutdate_registration <- registration[format(as.Date(registration$作成日), "%Y%m%d") <= kDateShimekiri, c(1, 8, 9)]
cutdate_risk1 <- risk1[format(as.Date(risk1$作成日), "%Y%m%d") <= kDateShimekiri, c(9, 63, 65)]
cutdate_risk2 <- risk2 [format(as.Date(risk2$作成日), "%Y%m%d") <= kDateShimekiri, c(9, 45)]
cutdate_cancel  <- cancel[format(as.Date(cancel$作成日), "%Y%m%d") <= kDateShimekiri, c(9, 13, 17, 25, 26, 27, 37)]
cutdate_cancel2  <- cancel2[format(as.Date(cancel2$作成日), "%Y%m%d") <= kDateShimekiri, c(9, 17:19)]

merge_1 <- merge(cutdate_registration, dxt_bd, by = "登録コード", all.x = T)
merge_2 <- merge(merge_1, cutdate_risk1, by = "症例登録番号", all = T)
merge_3 <- merge(merge_2, cutdate_risk2, by = "症例登録番号", all = T)
merge_4 <- merge(merge_3, `allocation_ALL-B12`, by = "症例登録番号", all = T)
merge_5 <- merge(merge_4, cutdate_cancel, by = "症例登録番号", all = T)
merge_6 <- merge(merge_5, cutdate_cancel2, by = "症例登録番号", all = T)
base_df <- merge_6[, -c(2, 3)]
# flowsheet作成
for(i in c(1,3:42)){
  MakeDataSet(eval(parse(text = paste0("flowsheet", i))))
}

risk1_df <- MakeDataSet_1(risk1)
write.csv(risk1_df, "../output/risk1.csv", row.names =  F)
risk2_df <- MakeDataSet_1(risk2)
write.csv(risk2_df, "../output/risk2.csv", row.names =  F)
risk3_df <- MakeDataSet_1(risk3)
write.csv(risk3_df, "../output/risk3.csv", row.names =  F)
cancel_df <- MakeDataSet_1(cancel)
write.csv(cancel_df, "../output/cancel.csv", row.names =  F)
cancel2_df <- MakeDataSet_1(cancel2)
write.csv(cancel2_df, "../output/cancel2.csv" , row.names =  F)

# riskをすべてマージしたシートを作成
dir.create("../output/review")
colnames(risk1_df) <- paste0("risk1_", colnames(risk1_df))
colnames(risk2_df) <- paste0("risk2_", colnames(risk2_df))
colnames(risk3_df) <- paste0("risk3_", colnames(risk3_df))
colnames(cancel_df) <- paste0("cancel_", colnames(cancel_df))
merge_1 <- merge(risk1_df, risk2_df, by.x = "risk1_症例登録番号",  by.y = "risk2_症例登録番号",  all = T)
merge_2 <- merge(merge_1, risk3_df, by.x = "risk1_症例登録番号",  by.y = "risk3_症例登録番号", all = T)
risk_result <- merge(merge_2, cancel_df, by.x = "risk1_症例登録番号",  by.y = "cancel_症例登録番号", all = T)
dxt_allocation <- base_df[, c(1, 7:11)]
colnames(risk_result)[1] <- "症例登録番号"
all_risk <- merge(dxt_allocation, risk_result, by = "症例登録番号", all = T)
all_risk[is.na(all_risk)] <- ""
write.csv(all_risk, "../output/review/risk_allSheet.csv" , row.names =  F)

datecut_df <- registration[format(as.Date(registration$作成日), "%Y%m%d") <= kDateShimekiri, ]
result <- datecut_df[, c(1:11,seq(13,length(colnames(datecut_df)), by = 2)) ] 
write.csv(result, "../output/registration.csv", row.names =  F)
write.csv(result, "../output/review/registration.csv", row.names =  F)

datecut_df <- initial[format(as.Date(initial$作成日), "%Y%m%d") <= kDateShimekiri, ]
initial_df <- datecut_df[, c(1:11, seq(15, length(colnames(initial)), by = 2)) ] 
dxt.flowsheet1 <- flowsheet1[, c(9, 17)]
colnames(dxt.flowsheet1)[2] <- "flowsheet1_治療開始日"
result <- merge(dxt.flowsheet1, initial_df, by = "症例登録番号", all.y = T)
write.csv(result, "../output/initial.csv", row.names =  F)
write.csv(result, "../output/review/initial.csv", row.names =  F)
