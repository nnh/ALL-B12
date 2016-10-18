#sae_mainPGM.R
#作成者：kaoru torii
#作成日：2016/10/18
#ver.2.0
######################################################################################
#作業ディレクトリの変更
setwd("../config")
getwd()

#configファイルの実行
source('sae_config.R')

#variables_configファイルの実行
source('sae_variables_config.R')

#出力ファイルの定義
OutputFail1    = paste("nomalsae_cutoffdate",Dayoutput,".csv",sep="")    #通常
OutputFail2    = paste("emergencysae_cutoffdate",Dayoutput,".csv",sep="")   #緊急

#SAE報告書の読み込み
  setwd("../rawdata")
  sae <- read.csv(list.files(pattern="sae_report"),as.is=T)

#CTCAEファイルの読み込み
  ctcae <- read.csv(list.files(pattern="CTCAEv4.0.csv"),as.is=T)

#SAE報告書のMedDRA codeとgradeを分割する
  sae$MedDRAcode <- sub("-.*","",sae$有害事象名)
  sae$grade <- sub("^.*.-","",sae$有害事象名)

#マージする
  saeCTCAE <- merge(sae,ctcae,by.x="MedDRAcode",by.y="CTCAE.v4.0..MedDRA..v12.0.Code",all.x=T)

#締日以降のデータを削除
  saeCTCAE$作成日 <- as.Date(as.character(saeCTCAE$作成日),format="%Y/%m/%d")
  saecut <- saeCTCAE[saeCTCAE$作成日<=DayCutOff,]
  length(saecut$症例登録番号)
                 
#報告分類で分ける
##通常報告「field296」が「3：(3)通常報告（15日以内に報告)」と「4：(5)追加報告（通常報告後）」のデータを抽出
  subnomal <- subset(saecut, field296=="3" | field296=="4")　#field番号の確認を！
#必要項目を抽出
  nomalbase <- subnomal[,c(ReportNo,DayCutoff,USUBJID,Hp,DayReport,AESTDTC,ClassReport,
                           NomalReport,Others,StudyCourse,StudyProgress,AETERM,AETOXGR,
                           Content,CausalityTherapy,CausalityMed,CausalityOthers,
                           CausalityOption,AEOUT,AEENDTC,LastReport)]

  names(nomalbase)
  names(nomalbase)[4] <- c("施設名" )
  names(nomalbase)[8:11] <- c("(3)通常報告(15日以内に報告)","「その他重大な医学的事象」選択:詳細",
                              "発生時期:治療コース名","発生時期:コース内" )
  names(nomalbase)[12] <- c("有害事象名")
  names(nomalbase)[15] <- c("因果関係(原因と考えられる治療法)")

#最終報告がTRUEのデータを抽出
 nomaltrue <- subset(nomalbase,最終報告=="true")

#有害事象報告日でソートする
 sortlist <- order(nomaltrue$有害事象報告日) 
 nomaltrue <- nomaltrue[sortlist,]

##緊急報告                 
  subemergency <- subset(saecut,field296=="1"|field296=="2"|field296=="7")
#必要項目を抽出

  emergencybase <- subemergency[,c(ReportNo,DayCutoff,USUBJID,Hp,DayReport,AESTDTC,ClassReport,
                           EmergencyReport1,EmergencyReport2,Others,StudyCourse,StudyProgress,
                           AETERM,AETOXGR,Content,CausalityTherapy,CausalityMed,CausalityOthers,
                           CausalityOption,AEOUT,AEENDTC,LastReport)]

  names(emergencybase)
  names(emergencybase)[4] <- c("施設名" )
  names(emergencybase)[8:12] <- c("(1)緊急一次報告(72時間以内に報告)","(2)緊急二次報告(15日以内に報告)",
                                  "「その他重大な医学的事象」選択:詳細",
                                  "発生時期:治療コース名","発生時期:コース内")
  names(emergencybase)[16] <- c("因果関係(原因と考えられる治療法)")
  names(emergencybase)[13] <- c("有害事象名")

#最終報告がTRUEのデータを抽出
 emergencytrue <- subset(emergencybase,最終報告=="true")

#有害事象報告日でソートする
 sortlist <- order(emergencytrue$有害事象報告日) 
 emergencytrue <- emergencytrue[sortlist,]

#ファイルの出力
  setwd("../output")   #ディレクトリの変更

#通常報告
  write.csv(nomaltrue,OutputFail1,row.names=F)

#緊急報告
  write.csv(emergencytrue,OutputFail2,row.names=F)


