# ALL-B12_deviationlistPGM.R
# 作成者：kaoru torii
# 作成日：2016/06/15
# ver.2.0
# 作成者：mamiko yonejima
# 作成日：2017/10/19
# ver.3.0
#########################
Dxt <- function(flowsheet){
  flowsheet[, c(2, 18, 5)]
}

## Config #####
prtpath <- "//192.168.200.222/Datacenter/Users/yonejima/ALL-B12"
kDownLoadDate <- "_170703_1142"
###############
# Read csv
list <- list.files(paste0(prtpath, "./rawdata"))
file.name <- sub(paste0(kDownLoadDate,".*"), "", list)
df.name <- sub(".*_", "", file.name)
setwd(paste0(prtpath, "./rawdata"))
for (i in 1:length(list)) {
  assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}
deviations <- read.csv(list.files(paste0(prtpath, "./rawdata"), pattern = "deviations"))
#必要項目の抽出
for(i in c(1, 3:43)){
  eval(parse(text = paste0("dxt_flowsheet", i, "<- Dxt(flowsheet", i, ")")))
    }
for(i in 1:3){
  eval(parse(text = paste0("dxt_risk", i, "<- Dxt(risk", i, ")")))
}
dxt_initial <- Dxt(initial)

# 逸脱一覧のリストに作成日をマージさせるためフローシートのファイルを結合し、作成日リストを作成する(縦結合)
matSum <- dxt_initial
for(i in 1:3){
  matSum <- rbind(matSum, eval(parse(text = paste0("dxt_risk", i))))
}
for(i in c(1, 3:43)){
  matSum <- rbind(matSum, eval(parse(text = paste0("dxt_flowsheet", i))))
}


# dvシート(逸脱一覧csv)grade4およびgrade5を削除(a1_入力値.表示データ.項目内のgradeが逸脱となっている行を削除する)
dxt_deviations <- deviations[substring(deviations$入力値.表示データ., 9, 9) != "-", ]
# IA day1投与日を削除
dxt_deviations <- dxt_deviations[dxt_deviations$フィールドラベル != "day1投与日(治療開始日)",]
# IB day36投与日を削除
dxt_deviations <- dxt_deviations[dxt_deviations$フィールドラベル != "day36投与日",]
# 強化療法のday1投与日を削除
dxt_deviations <- dxt_deviations[dxt_deviations$フィールドラベル != "day1投与日",]
# 強化療法の本コース最終投与日を削除 ## TODO koko
dxt_deviations <- dxt_deviations[dxt_deviations$シート名 == "フローシート：強化療法(M2)" &&  dxt_deviations$フィールドラベル != "本コース試験治療薬剤最終投与日"  ,]
dxt_deviations <- dxt_deviations[dxt_deviations$シート名 == "フローシート：強化療法(M5)" &&  dxt_deviations$フィールドラベル != "本コース試験治療薬剤最終投与日"  ,]
dxt_deviations <- dxt_deviations[dxt_deviations$シート名 == "フローシート：強化療法(M5+L)" &&  dxt_deviations$フィールドラベル != "本コース試験治療薬剤最終投与日"  ,]
dxt_deviations <- dxt_deviations[dxt_deviations$シート名 == "フローシート：強化療法(M5+VL)" &&  dxt_deviations$フィールドラベル != "本コース試験治療薬剤最終投与日"  ,]
dxt_deviations <- dxt_deviations[dxt_deviations$シート名 == "フローシート：強化療法_HRブロック(HR1)" &&  dxt_deviations$フィールドラベル != "本コース試験治療薬剤最終投与日"  ,]

#施設科名を分ける
colnames(itudatu_nae)
itudatu_nae$施設名<- sub("-.*","",itudatu_nae$施設名科名)

#変数名の変更(変数名の頭に「 」をつける)
colnames(itudatu_nae)<- paste0("",colnames(itudatu_nae))

#必要な項目の抽出
colnames(itudatu_nae)
itudatu<- itudatu_nae[,c("症例番号","順序付きフローシート順序","施設名","シート名","フィールドラベル","入力値.表示データ.")]

#リスクシートのマージ
risk3<- merge(rs1,rs2,by=c("症例登録番号"),all.x=T)

#症例番号、暫定リスク、確定リスクの抽出
risk<- risk3[,c("症例登録番号","暫定リスク判定結果","確定リスク判定結果")]

#中止届の必要項目の抽出
cancel<- cs1[,c("症例登録番号","中止時期.コース名.","治療終了.中止.理由","中止時期.day.week.","中止時期.日数.週数.")]

#マージをする(itudatu,creation_date,risk,cancelの4ファイル)※キーはitudatuファイルの「症例番号」
colnames(itudatu)
colnames(creation_date)

itudatu_a<- merge(itudatu,creation_date,by.x=c("症例番号","シート名"),by.y=c("b_症例登録番号","b_シート名"),all.x=T)
itudatu_b<- merge(itudatu_a,risk,by.x="症例番号",by.y="症例登録番号",all.x=T)
itudatu_c<- merge(itudatu_b,cancel,by.x="症例番号",by.y="症例登録番号",all.x=T)

#フローシート名の「フローシート：」を外す
colnames(itudatu_c)
study_<- sub("フローシート："," ",itudatu_c$シート名)
itudatulist3<- data.frame(itudatu_c,study_)

#並び替え
colnames(itudatulist3)
itudatulist2<- itudatulist3[,c("b_作成日","順序付きフローシート順序","症例番号","施設名","暫定リスク判定結果", "確定リスク判定結果",
                               "シート名","フィールドラベル","入力値.表示データ.","中止時期.コース名.","治療終了.中止.理由","中止時期.day.week.","中止時期.日数.週数.")]

#必要項目の抽出
itudatulist1<- itudatulist3[,c("b_作成日","順序付きフローシート順序","症例番号","施設名","暫定リスク判定結果","確定リスク判定結果","study_","フィールドラベル","入力値.表示データ."
                               ,"中止時期.コース名.","治療終了.中止.理由","中止時期.day.week.","中止時期.日数.週数.")]    

#名前の変更
names(itudatulist1)[3:9]<- c("ALL-B12 No.","施設名","暫定リスク","確定リスク","治療コース","項目","内容") 

#ソートする
itudatulist<- order(itudatulist1$治療コース)
itudatulist<- (itudatulist1[itudatulist,])
itudatulist

#csvファイルへの書き出し
setwd("../output")
write.csv(itudatulist,PathOutputdv,row.names=FALSE)

