# ALL-B12 基本的な情報をマージするプログラム
# Mamiko Yonejima
# 20170817 

# 関数の設定
# 列抽出とAEgrade切り離し、ファイルの出力をする関数
MakeDataSet <- function(dataframe){
  if(flg == 1){
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, 
                            c(2, 4:6, 8, 11, 13, 15, 18, 29:length(colnames(dataframe)))]
    flowsheet_df <- datecut_df[, c(1:9, seq(9, length(colnames(datecut_df)), by = 2))]
  }else{
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, 
                            c(2, 4:6, 8, 11, 13, 15, 18, 29:length(colnames(dataframe)))]
    flowsheet_df <- datecut_df[format(as.Date(datecut_df$作成日), "%Y%m%d") >= kDateShimekiri_start,
                               c(1:9, seq(9, length(colnames(datecut_df)), by = 2))]
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
  result <- cbind(merge_df[, c(1,3:numberminus1)], ae_grade_dxt_0, 上記4項目の血液毒性を除く有害事象有無, ae_grade_dxt_1)
  result[is.na(result)] <- ""
  # setwd("../output")
  write.csv(result, paste0("../output/flowsheet",i,".csv"))
}　 
# mergeせず、列抽出のみの関数
MakeDataSet_1 <- function(dataframe){
  if(flg == 1){
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, ]
    datecut_df[is.na(datecut_df)] <- ""
    datecut_df[format(as.Date(datecut_df$作成日), "%Y%m%d") >= kDateShimekiri_start,
               c(2, 4:6, 8, 11, 13, 15, 18, seq(28, length(colnames(datecut_df)), by = 2)) ] 
  }else{
    datecut_df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kDateShimekiri, ]
    datecut_df[is.na(datecut_df)] <- ""
    datecut_df[format(as.Date(datecut_df$作成日), "%Y%m%d") >= kDateShimekiri_start,
               c(2, 4:6, 8, 11, 13, 15, 18, seq(28, length(colnames(datecut_df)), by = 2)) ] 
  }
}  

# 締め切り日、ダウンロード日の設定 ######################
flg <- 1  # 1:締め切り日1つ設定バージョン、2:定モニバージョン（startの日も設定）
kDateShimekiri_start <- "20161201"  # flg==2の時に設定
kDateShimekiri <- "20170531"
kDownLoadDate <- "_170821_1241"
#########################################################
# よみこみ
source("./programs/ALL-B12-merge-config.R", encoding = "UTF-8")

setwd("./rawdata")
list <- list.files()
file.name <- sub(paste0(kDownLoadDate,".*"), "", list)
df.name <- sub(".*_", "", file.name)
for (i in 1:length(list)) {
  assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}

# リスク、中止などの情報を含んだ基本的なデータセットの作成
cutdate_registration <- registration[format(as.Date(registration$作成日), "%Y%m%d") <= kDateShimekiri, c(5, 18)]
cutdate_risk1 <- risk1[format(as.Date(risk1$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 78, 80)]
cutdate_risk2 <- risk2 [format(as.Date(risk2$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 60)]
cutdate_cancel  <- cancel[format(as.Date(cancel$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 28, 46, 40, 41, 42, 52)]
cutdate_cancel2  <- cancel2[format(as.Date(cancel2$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 32, 34)]
cutdate_allocation1  <- allocation1[format(as.Date(allocation1$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 28)]
cutdate_allocation2  <- allocation2[format(as.Date(allocation2$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 28)]
cutdate_allocation3  <- allocation3[format(as.Date(allocation3$作成日), "%Y%m%d") <= kDateShimekiri, c(18, 28)]

# allocationの項目を作成
allo_merge_1 <- merge(cutdate_allocation1, cutdate_allocation2, by = "症例登録番号", all = T)
allo_merge_2 <- merge(allo_merge_1, cutdate_allocation3, by = "症例登録番号", all = T)
allo_merge_2$allocation <- ifelse(is.na(allo_merge_2$割付ラベル.x) & is.na(allo_merge_2$割付ラベル.y), allo_merge_2$割付ラベル,
                           ifelse (is.na(allo_merge_2$割付ラベル.x) & is.na(allo_merge_2$割付ラベル), 
                                   allo_merge_2$割付ラベル.y, allo_merge_2$割付ラベル.x))
allocation <- allo_merge_2[, c(1,5)]

merge_1 <- merge(cutdate_registration, cutdate_risk1, by = "症例登録番号", all = T)
merge_2 <- merge(merge_1, cutdate_risk2, by = "症例登録番号", all = T)
merge_3 <- merge(merge_2, allocation, by = "症例登録番号", all = T)
merge_4 <- merge(merge_3, cutdate_cancel, by = "症例登録番号", all = T)
base_df <- merge(merge_4, cutdate_cancel2, by = "症例登録番号", all = T)
# flowsheet作成
for(i in c(1,3:42)){
  MakeDataSet(eval(parse(text = paste0("flowsheet", i))))
}

flowsheet_df <- MakeDataSet_1(risk1)
write.csv(flowsheet_df, "../output/risk1.csv")
flowsheet_df <- MakeDataSet_1(risk2)
write.csv(flowsheet_df, "../output/risk2.csv")
flowsheet_df <- MakeDataSet_1(risk3)
write.csv(flowsheet_df, "../output/risk3.csv")
flowsheet_df <- MakeDataSet_1(cancel)
write.csv(flowsheet_df, "../output/cancel.csv")
flowsheet_df <- MakeDataSet_1(cancel2)
write.csv(flowsheet_df, "../output/cancel2.csv")

datecut_df <- registration[format(as.Date(registration$作成日), "%Y%m%d") <= kDateShimekiri, ]
result <- datecut_df[, c(2, 4:6, 8, 11, 13, 15, 18, seq(32, length(colnames(registration)), by = 2)) ] 
write.csv(result, "registration.csv")

datecut_df <- initial[format(as.Date(initial$作成日), "%Y%m%d") <= kDateShimekiri, ]
initial_df <- datecut_df[, c(2, 4:6, 8, 11, 13, 15, 18, seq(30, length(colnames(initial)), by = 2)) ] 
dxt.flowsheet1 <- flowsheet1[, c(18,32)]
colnames(dxt.flowsheet1)[2] <- "flowsheet1_治療開始日"
result <- merge(dxt.flowsheet1, initial_df, by = "症例登録番号", all.y = T)
write.csv(result, "../output/initial.csv")
