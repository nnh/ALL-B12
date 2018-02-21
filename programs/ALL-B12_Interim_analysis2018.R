#ALL-B12 中間解析 登録終了後の予備解析
#2018/2/19
#Mamiko Yonejima

# 年齢計算のfunction
YearDif <- function(starting, ending) {
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows
# 締め切り日カット、fieldカットのfunction
CutField <-  function(dataframe, header) {
              df <- dataframe[format(as.Date(dataframe$作成日), "%Y%m%d") <= kColseDate,
                  c(1:11, seq(13, length(colnames(dataframe)), by = 2))]
}

# dataの読み込み
#-- 設定-------------------------------------------------------------------------------------------------------------
# # output,rawdataはaronas上にて入出力する
prtpath <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/11.03.03 中間解析用生データ/登録終了後_2018/data_cleaning/180216"
kColseDate <- "2018/02/28"  # XXXX/XX/XX現在で集計
kOutputdate <- "20180221"
#------------------------------------------------------------------------------------------------------------------
# csvを読み込む
setwd(paste0(prtpath, "./rawdata"))   
registration <- read.csv(list.files(pattern = "ALL-B12_registration"), as.is = T, na.strings = c(""))
cancel1 <- read.csv(list.files(pattern = "ALL-B12_cancel_"), as.is = T, na.strings = c(""))
initial1 <- read.csv(list.files(pattern = "ALL-B12_initial1"), as.is = T, na.strings = c(""))
risk1 <- read.csv(list.files(pattern = "ALL-B12_risk1"), as.is = T, na.strings = c(""))
risk2 <- read.csv(list.files(pattern = "ALL-B12_risk2"), as.is = T, na.strings = c(""))
f1 <- read.csv(list.files(pattern = "ALL-B12_flowsheet1_"), as.is = T, na.strings = c(""))
jp <- read.csv(list.files(pattern = "JPLSG_registration"), as.is = T, na.strings = c(""))
# 締め切り日カット、fieldカット
registration_ct <- CutField(registration)
colnames(registration_ct) <- paste0("r_", colnames(registration_ct))
cancel1_ct <- CutField(cancel1)
colnames(cancel1_ct) <- paste0("c_", colnames(cancel1_ct))
initial1_ct <- CutField(initial1)
colnames(initial1_ct) <- paste0("i_", colnames(initial1_ct))
risk1_ct <- CutField(risk1)
colnames(risk1_ct) <- paste0("risk1_", colnames(risk1_ct))
risk2_ct <- CutField(risk2)
colnames(risk2_ct) <- paste0("risk2_", colnames(risk2_ct))
f1_ct <- CutField(f1)
colnames(f1_ct) <- paste0("f1_", colnames(f1_ct))
jp_ct <- jp[, c("登録コード", "生年月日")]
# シートのマージ
m1 <- merge(registration_ct, jp_ct, by.x = "r_登録コード", by.y = "登録コード", all.x = T)
m2 <- merge(m1, cancel1_ct, by.x = "r_症例登録番号", by.y = "c_症例登録番号", all.x = T)
m3 <- merge(m2, initial1_ct, by.x = "r_症例登録番号", by.y = "i_症例登録番号", all.x = T)
m4 <- merge(m3, risk1_ct, by.x = "r_症例登録番号", by.y = "risk1_症例登録番号", all.x = T)
m5 <- merge(m4, risk2_ct, by.x = "r_症例登録番号", by.y = "risk2_症例登録番号", all.x = T)
ads <- merge(m5, f1_ct, by.x = "r_症例登録番号", by.y = "f1_症例登録番号", all.x = T)
ads[is.na(ads)] <- ""
# 出力
OutputFile <- paste0("ALL-B12_ads_Interim_", kOutputdate, ".csv")
write.csv(ads, paste0(prtpath, "./output/", OutputFile), row.names=F)
