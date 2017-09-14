#ALL-B12 HR 中間解析
#2017/8/22
#Mamiko Yonejima

#年齢計算のfunction
YearDif <- function(starting, ending) {
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

#Setting date
downloaded.date <- "170821_1241"  # ダウンロードcsvの日付・時間を入力
colse.date <- "2017/05/31"  # XXXX/XX/XX現在で集計

# csvを読み込む
setwd("./rawdata")
r.regi <- read.csv(paste0("ALL-B12_registration_", downloaded.date, ".csv"), na.strings = c(""), as.is = T, )
r.risk1 <- read.csv(paste0("ALL-B12_risk1_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.risk2 <- read.csv(paste0("ALL-B12_risk2_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.risk3 <- read.csv(paste0("ALL-B12_risk3_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.cancel <- read.csv(paste0("ALL-B12_cancel_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.cancel2 <- read.csv(paste0("ALL-B12_cancel2_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.sae <- read.csv(paste0("ALL-B12_sae_report_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.allocation1 <- read.csv(paste0("ALL-B12_allocation1_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.allocation2 <- read.csv(paste0("ALL-B12_allocation2_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.allocation3 <- read.csv(paste0("ALL-B12_allocation3_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
r.followup <- read.csv(paste0("ALL-B12_followup4_", downloaded.date, ".csv"), na.strings = c(""), as.is = T)
setwd("../input")
i.CTCAE <- read.csv("CTCAEv4.0.csv", as.is = T)
setwd("..")
#締め切り日2017/5/31以降に提出されたシートをカットする
cut.date.regi <- r.regi[r.regi$作成日 <= colse.date, ]
cut.date.risk1 <- r.risk1 [r.risk1$作成日 <= colse.date, ]
cut.date.risk2 <- r.risk2 [r.risk2$作成日 <= colse.date, ]
cut.date.risk3 <- r.risk3 [r.risk3$作成日 <= colse.date, ]
cut.date.cancel  <- r.cancel[r.cancel $作成日 <= colse.date, ]
cut.date.cancel2  <- r.cancel2[r.cancel2 $作成日 <= colse.date, ]
cut.date.sae <- r.sae [r.sae$作成日 <= colse.date, ]
cut.date.allocation1 <- r.allocation1 [r.allocation1$作成日 <= colse.date, ]
cut.date.allocation2 <- r.allocation2 [r.allocation2$作成日 <= colse.date, ]
cut.date.allocation3 <- r.allocation3 [r.allocation3$作成日 <= colse.date, ]

##B12解析用データベース##################################################################
#診断時年齢の計算
cut.date.regi$age.diagnosis <- YearDif(cut.date.regi$生年月日, cut.date.regi$診断年月日)

#必要項目の抽出と変数名の頭にシート名をつける
dxt.regi <- cut.date.regi[, c(2, 18, 15, 19, 11, 32, 55)]
colnames(dxt.regi) <- paste0("registration_",colnames(dxt.regi))
dxt.risk1 <- cut.date.risk1[, c(18, 79, 80, 77, 78 )]
colnames(dxt.risk1) <- paste0("暫定リスク_",colnames(dxt.risk1))
dxt.risk2 <- cut.date.risk2[, c(18, 59, 60, 40, 42, 57, 58)]
colnames(dxt.risk2) <- paste0("確定リスク_",colnames(dxt.risk2))
dxt.risk3 <- cut.date.risk3[, c(18, 38, 40, 53, 54)]  # BMA4は変数名頭にBMA4がついている為変数名変更は行わない
dxt.allocation <- cut.date.allocation2[, c(2, 18, 28, 32)]
colnames(dxt.allocation ) <- paste0("allocation_",colnames(dxt.allocation ))
dxt.followup <- r.followup[,c(18, 25, 27:50)]
colnames(dxt.followup ) <- paste0("followup_",colnames(dxt.followup ))

#ads作成のためのマージ
df <- merge(dxt.regi, dxt.risk1, by.x = "registration_症例登録番号", by.y = "暫定リスク_症例登録番号", all.x = T)
df1 <- merge(df, dxt.risk2, by.x = "registration_症例登録番号", by.y = "確定リスク_症例登録番号", all.x = T)
df2 <- merge(df1, dxt.risk3, by.x = "registration_症例登録番号", by.y = "症例登録番号", all.x = T)
df3 <- merge(df2, dxt.allocation, by.x = "registration_症例登録番号", by.y = "allocation_症例登録番号", all.x = T)
df4 <- merge(df3, dxt.followup, by.x = "registration_症例登録番号", by.y = "followup_症例登録番号", all.x = T)

#暫定リスクHRのみを抽出
ads <- df4[!is.na(df4$暫定リスク_field6) & df4$暫定リスク_field6 == 3 | !is.na(df4$確定リスク_field6) & df4$確定リスク_field6 == 3,
           -c(2,22)]
ads[is.na(ads)] <- ""
setwd("./output")
write.csv(ads, "ALL-B12_解析用データベース.csv", row.names = F)
setwd("..")

##B12症例登録一覧の作成##############################################
df4 -> base.data
#フローシートの提出状況
base.data$followup.2017.5.31.提出状況 <- ifelse(is.na(base.data$暫定リスク_field6), "暫定リスク報告未提出", 
                                           ifelse(base.data$暫定リスク_field6 != 3 & base.data$確定リスク_field6 != 3, "未実施", 
                                           ifelse(is.na(base.data$followup_再入力依頼), "未提出", "提出")))

base.data$暫定リスク報告.提出状況 <-  ifelse(is.na(base.data$暫定リスク_field6), 0, 1)
base.data$確定リスク報告.提出状況 <-  ifelse(is.na(base.data$確定リスク_field6), 0, 1) 
base.data$BMA4.TP2.提出状況 <-  ifelse(is.na(base.data$field3), 0, 1)
#L-ASP中止届のマージ
base.data.lasp <- merge(base.data, cut.date.cancel2, by.x = "registration_症例登録番号", by.y = "症例登録番号", all.x = T)


#SRとHRより割り付け群の抽出,変数名の頭にリスクをつけマージ
dxt.allocation1 <- r.allocation1[, c(2, 18, 28)]
colnames(dxt.allocation1) <- paste0("SR.", colnames(dxt.allocation1))
dxt.allocation3 <- r.allocation3[, c(2, 18, 28)]
colnames(dxt.allocation3) <-  paste0("HR.", colnames(dxt.allocation3))
base.data.m.sr <- merge(base.data.lasp, dxt.allocation1, by.x = "registration_症例登録番号", by.y = "SR.症例登録番号", all.x = T)
base.data.m.hr <- merge(base.data.m.sr, dxt.allocation3, by.x = "registration_症例登録番号", by.y = "HR.症例登録番号", all.x = T)
colnames(base.data.m.hr)[c(22, 23)] <- c("IR.作成日", "IR.割付ラベル")
#allocation提出状況の列を作成
base.data.m.hr$allocation.提出状況 <- ifelse(is.na(base.data.m.hr$SR.割付ラベル) &　is.na(base.data.m.hr$IR.割付ラベル) &　is.na(base.data.m.hr$HR.割付ラベル), 0, 1) 

#割付日, 群の列を作成
base.data.m.hr$自動割付.risk <- ifelse(base.data.m.hr$allocation.提出状況 == 0, "", 
                          ifelse(!is.na(base.data.m.hr$SR.割付ラベル), "SR", 
                            ifelse(!is.na(base.data.m.hr$IR.割付ラベル), "IR","HR")))
base.data.m.hr$割付群 <- ifelse(base.data.m.hr$allocation.提出状況 == 0, "", 
                             ifelse(!is.na(base.data.m.hr$SR.割付ラベル), base.data.m.hr$SR.割付ラベル, 
                                    ifelse(!is.na(base.data.m.hr$IR.割付ラベル), base.data.m.hr$IR.割付ラベル, base.data.m.hr$HR.割付ラベル)))
base.data.m.hr$割付日 <- ifelse(base.data.m.hr$allocation.提出状況 == 0, "", 
                             ifelse(!is.na(base.data.m.hr$SR.割付ラベル), base.data.m.hr$SR.作成日, 
                                    ifelse(!is.na(base.data.m.hr$IR.割付ラベル), base.data.m.hr$IR.作成日, base.data.m.hr$HR.作成日)))
##IRで割り付け前にL-ASP中止になった症例にフラグを立てる
base.data.m.hr$IR.割り付け前にL.ASP中止有無 <- ifelse(base.data.m.hr$allocation.提出状況 == 0 & base.data.m.hr$確定リスク報告.提出状況 ==1 & base.data.m.hr$確定リスク_field6 != 2 & base.data.m.hr$暫定リスク_field8 == 1, 9, ##TODo
                                            ifelse(base.data.m.hr$allocation.提出状況 == 0 & base.data.m.hr$暫定リスク_field8 == 1, 9, 
                                            ifelse(base.data.m.hr$allocation.提出状況 == 0 , 10,
                                            ifelse(base.data.m.hr$確定リスク報告.提出状況 == 0 , 10, 
                                            ifelse(base.data.m.hr$自動割付.risk == "SR" | base.data.m.hr$自動割付.risk == "HR", 9, 
                                            ifelse(!is.na(base.data.m.hr$allocation_L.ASP使用可能である), 0, 
                                            ifelse(is.na(base.data.m.hr$L.ASP中止日.最終投与日.), 0, 
                                           ifelse(base.data.m.hr$L.ASP中止日.最終投与日. > base.data.m.hr$割付日, 0, 1))))))))

#必要な列を抽出し出力
all.data.b12 <- base.data.m.hr[, c(1, 3, 2, 50:53, 91:93, 10, 11, 24, 94, 85, 86 )] 
colnames(all.data.b12)[15] <- "field3"
all.data.b12[is.na(all.data.b12)] <- ""
setwd("./output")
write.csv(all.data.b12, "ALL-B12_症例登録一覧.csv", row.names = F)
setwd("..")

##SAE報告のDS作成####################################################
#CTCAE codeと事象名、gradeの列を作成
cut.date.sae$MedDRAcode <- sub("-.*","",cut.date.sae$有害事象名)
cut.date.sae$grade <- sub("^.*.-","",cut.date.sae$有害事象名)

#マージする
saeCTCAE <- merge(cut.date.sae, i.CTCAE, by.x="MedDRAcode", by.y="CTCAE.v4.0..MedDRA..v12.0.Code", all.x=T)
saeCTCAE[is.na(saeCTCAE)] <- ""
#緊急、通常、すべてに分けて出力
all.sae.report <- saeCTCAE[,c(3, 19, 16, 26, 31:47, 1, 71, 66, 51:65)] 
nomal.sae.report <- all.sae.report[all.sae.report$field296 == 3 | all.sae.report$field296 == 4, ]
immediate.sae.report <- all.sae.report[all.sae.report$field296 == 1 | all.sae.report$field296 == 2 | all.sae.report$field296 == 7, ]
setwd("./output")
write.csv(all.sae.report, "ALL-B12_SAE報告_全症例.csv", row.names = F)
write.csv(nomal.sae.report, "ALL-B12_SAE報告_通常報告.csv", row.names = F)
write.csv(immediate.sae.report, "ALL-B12_SAE報告_緊急報告.csv", row.names = F)
setwd("..")

##B12中止症例一覧#############################################################################
#暫定リスクと確定リスクを中止届にマージする
pick0.risk1 <- cut.date.risk1[,c(18,80)]
pick0.risk2 <- cut.date.risk2[,c(18,60)]
add.info.cancel<- merge(cut.date.cancel, pick0.risk1, by = "症例登録番号", all.x = T)
add.info.cancel <- merge(add.info.cancel, pick0.risk2, by = "症例登録番号", all.x = T)

#必要項目の抽出と出力
ds.cancel <- add.info.cancel[,c(1, 16, 95, 96, 27, 28, 31:94 )]
ds.cancel[is.na(ds.cancel)] <- ""
setwd("./output")
write.csv(ds.cancel, "ALL-B12_中止症例一覧.csv", row.names = F)
setwd("..")

