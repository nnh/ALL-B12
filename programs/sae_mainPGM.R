#ALL-B12_sae_mainPGM.R
#作成者：Mamiko Yonejima
#作成日：2017/09/08
#ver.4.0
#*******************************************
## 読み込みファイル名の定義
path <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/04.03.02 定期モニタリングレポート/第13回/R"
#読み込みファイル名
path_sae_1 <- "/cleaning/rawdata/ALL-B12_sae_report_190605_0913.csv"

#出力日設定
path_output <- "/cleaning/output/"
Date_output  <- "20190619"
#########################################

#出力ファイルの定義
OutputFile1 <- paste("nomalsae_cutoffdate", Date_output, ".csv",sep = "")    #通常
OutputFile2 <- paste("emergencysae_cutoffdate", Date_output, ".csv", sep = "")   #緊急

#変数名の定義 #未定義
ReportNo <- "報告番号"
DayCutoff <- "作成日"
Hp <- "シート作成時施設名"
DayReport <- "有害事象報告日"
ClassReport <- "報告分類"
NomalReport <- "X.3.通常報告.15日以内に報告."
EmergencyReport1 <- "X.1.緊急一次報告.72時間以内に報告."
EmergencyReport2 <- "X.2.緊急二次報告.15日以内に報告."
Others <- "X..その他重大な医学的事象.選択.詳細"
StudyCourse <- "発生時期.治療コース名"
StudyProgress <- "発生時期.コース内"
Content <- "経過内容"
CausalityTherapy <- "因果関係.原因と考えられる治療法."
CausalityMed <- "因果関係疑いの試験薬"
CausalityOthers <- "因果関係疑いその他_詳細"
CausalityOption <- "該当する因果関係を選択して下さい"
LastReport <- "最終報告"


# CDISCの変数名に定義済み
USUBJID <- "症例登録番号"
AESTDTC <- "有害事象発生日"
AETERM <- "有害事象名"
AEENDTC <- "転帰確認日"
AETOXGR <- "grade"
AEOUT <- "報告時の転帰"
# # CTCAEファイルの読み込み
# ctcae <- read.csv(paste0(path, "./report/input/CTCAEv4.0.csv"), as.is=T)
# SAE報告書の読み込み
sae <- read.csv(paste0(path, path_sae_1), as.is = T, na.strings = "" )

#SAE報告書のMedDRA codeとgradeを分割する
sae$grade <- sub("^.*.-", "", sae$有害事象名)
sae$有害事象名 <- sub("-.*", "", sae$有害事象名)
ads <- sae

## 通常報告「field296」が「3：(3)通常報告（15日以内に報告)」と「4：(5)追加報告（通常報告後）」のデータを抽出
subnomal <- subset(ads, field296=="3" | field296=="4")　#field番号の確認を！
# 必要項目を抽出
nomalbase <- subnomal[,c(ReportNo, DayCutoff, USUBJID, Hp, DayReport, AESTDTC, ClassReport,
                         NomalReport,Others,StudyCourse,StudyProgress,AETERM,AETOXGR,
                         Content,CausalityTherapy,CausalityMed, CausalityOthers, 
                         CausalityOption, AEOUT, AEENDTC, LastReport)]

names(nomalbase)[4] <- c("施設名" )
names(nomalbase)[8:11] <- c("(3)通常報告(15日以内に報告)","「その他重大な医学的事象」選択:詳細", 
                            "発生時期:治療コース名",  "発生時期:コース内" )
names(nomalbase)[12] <- c("有害事象名")
names(nomalbase)[15] <- c("因果関係(原因と考えられる治療法)")


#有害事象報告日でソートする
sortlist <- order(nomalbase$有害事象報告日) 
nomaltrue <- nomalbase[sortlist, ]
nomaltrue[is.na(nomaltrue)] <- ""
##緊急報告                 
subemergency <- subset(ads, field296=="1"|field296=="2"|field296=="7")
#必要項目を抽出
emergencybase <- subemergency[, c(ReportNo, DayCutoff, USUBJID, Hp, DayReport, AESTDTC, ClassReport, 
                                  EmergencyReport1, EmergencyReport2, Others, StudyCourse, StudyProgress, 
                                  AETERM, AETOXGR, Content, CausalityTherapy, CausalityMed, CausalityOthers, 
                                  CausalityOption, AEOUT, AEENDTC, LastReport)]

names(emergencybase)
names(emergencybase)[4] <- c("施設名" )
names(emergencybase)[8:12] <- c("(1)緊急一次報告(72時間以内に報告)", "(2)緊急二次報告(15日以内に報告)", 
                                "「その他重大な医学的事象」選択:詳細", 
                                "発生時期:治療コース名", "発生時期:コース内")
names(emergencybase)[16] <- c("因果関係(原因と考えられる治療法)")
names(emergencybase)[13] <- c("有害事象名")


#有害事象報告日でソートする
sortlist <- order(emergencybase$有害事象報告日) 
emergencytrue <- emergencybase[sortlist, ]
emergencytrue[is.na(emergencytrue)] <- ""
# 
# #ファイルの出力
# PathOut <- setwd("../output")   #ディレクトリの変更

#通常報告
write.csv(nomaltrue, paste0(path, path_output, OutputFile1), row.names=F)

#緊急報告
write.csv(emergencytrue,  paste0(path, path_output, OutputFile2), row.names=F)

