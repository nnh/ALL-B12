# ALL-B12_deviationlistPGM.R
# 作成者：kaoru torii
# 作成日：2016/06/15
# ver.2.0
# 作成者：mamiko yonejima
# 作成日：2017/10/19
# ver.3.0
#########################
Dxt <- function(flowsheet){
  flowsheet[, c(1, 2, 9)]
}

## Config #####
prtpath <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/04.03.02 定期モニタリングレポート/第11回/R/cleaning"
kDownLoadDate <- "_180601_1009"  # フローシートのDL日付
kDev <- "ALL-B12_deviations_180601_1209.csv"
###############
# Read csv
list <- list.files(paste0(prtpath, "./rawdata"))
file.name <- sub(paste0(kDownLoadDate,".*"), "", list)
df.name <- sub(".*_", "", file.name)
setwd(paste0(prtpath, "./rawdata"))
for (i in 1:length(list)) {
  assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}
# setwd(paste0(prtpath, "./dev/rawdata"))
deviations0 <- read.csv(kDev, as.is=T, na.strings = c(""))

# inputの読み込み
sheet_name <- read.csv("../input/sheet_name.csv")

#必要項目の抽出
for(i in c(1, 3:43)){
  eval(parse(text = paste0("dxt_flowsheet", i, "<- Dxt(flowsheet", i, ")")))
    }
for(i in 1:3){
  eval(parse(text = paste0("dxt_risk", i, "<- Dxt(risk", i, ")")))
}
dxt_initial <- Dxt(initial)

# deviationsに和名シート名をマージ
deviations <- merge(deviations0, sheet_name, by = "シート名", all.x = T )

# 逸脱一覧のリストに作成日をマージさせるためフローシートのファイルを結合し、作成日リストを作成する(縦結合)
matSum <- dxt_initial
for(i in 1:3){
  matSum <- rbind(matSum, eval(parse(text = paste0("dxt_risk", i))))
}
for(i in c(1, 3:43)){
  matSum <- rbind(matSum, eval(parse(text = paste0("dxt_flowsheet", i))))
}
matSum$key <- paste0(matSum$症例登録番号, matSum$シート名英数字別名)
matSum <- matSum[, c(2, 4)]
# dvシート(逸脱一覧csv)grade4およびgrade5を削除(a1_入力値.表示データ.項目内のgradeが逸脱となっている行を削除する)
dxt_deviations <- deviations[substring(deviations$入力値.表示データ., 9, 9) != "-", ]
# IA day1投与日を削除
dxt_deviations <- dxt_deviations[dxt_deviations$フィールドラベル != "day1投与日(治療開始日)",]
# 強化療法のday1投与日を削除
dxt_deviations <- dxt_deviations[dxt_deviations$フィールドラベル != "day1投与日",]
# 強化療法の本コース最終投与日を削除 
dxt_deviations_0 <- subset(dxt_deviations, dxt_deviations$フィールドラベル != "本コース試験治療薬剤最終投与日" )
dxt_deviations_1 <- dxt_deviations[dxt_deviations$フィールドラベル == "本コース試験治療薬剤最終投与日" ,]
dxt_deviations_2 <- dxt_deviations_1[dxt_deviations_1$シート名 == "フローシート：早期強化療法(IB)" | dxt_deviations_1$シート名 == "フローシート：早期強化療法(IB+L)" |  dxt_deviations_1$シート名 == "フローシート：早期強化療法(IB+VL)", ]
dxt_deviations <- rbind(dxt_deviations_0, dxt_deviations_2)
# followupを削除 
dxt_deviations <- dxt_deviations[dxt_deviations$フィールドラベル != "最終転帰確認日", ]
colnames(dxt_deviations)[2] <- "症例登録番号"
dxt_deviations$key <- paste0(dxt_deviations$症例登録番号, dxt_deviations$sheet.name)
#施設名を抽出
dxt_deviations$施設名<- sub("-.*","",dxt_deviations$施設名科名)
#必要な項目の抽出
dxt_deviations　<- dxt_deviations[,c("症例登録番号", "順序付きフローシート順序", "施設名","シート名", "フィールドラベル", "入力値.表示データ.", "key")]
#リスクシートのマージ
risk <- merge(risk1, risk2, by = "症例登録番号", all = T)
#症例番号、暫定リスク、確定リスクの抽出
dxt_risk <- risk[, c(1, 65, 111)]
#中止届の必要項目の抽出
dxt_cancel <- cancel[,c("症例登録番号","中止時期.コース名.","治療終了.中止.理由","中止時期.day.week.","中止時期.日数.週数.")]

#マージをする
m_risk_dev <- merge(dxt_risk, dxt_deviations, by = "症例登録番号", all.y = T)
m_risk_dev_cancel <- merge(m_risk_dev, dxt_cancel, by = "症例登録番号", all.x = T)
result <- merge(matSum, m_risk_dev_cancel, by = "key", all.y = T)
result <- result[, -1]
#ソートする
result<- result[order(result$順序付きフローシート順序),]
#csvファイルへの書き出し
result[is.na(result)] <- ""
write.csv(result, eval(parse(text = paste0("'", prtpath, "/output/deviation/deviations.csv'"))), row.names=FALSE)

