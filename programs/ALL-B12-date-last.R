# 最終薬剤投与日と中止届の最終薬剤投与日の確認
# 2020/4/17
# Mamiko Yonejima
#**********
# 設定
kDownLoadDate <- "_200417_1155"
prtpath <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/11.03.07 最終解析用生データ/データクリーニング/ALL-B12_sheets_200417_1155"

# 読み込み
list <- list.files(paste0(prtpath, "./rawdata"))
file.name <- sub(paste0(kDownLoadDate,".*"), "", list)
df.name <- gsub("ALL-B12_", "",file.name)
setwd(paste0(prtpath, "./rawdata"))
for (i in 1:length(list)) {
  assign(df.name[i], read.csv(list[i], as.is=T, na.strings = c(""), fileEncoding="CP932", stringsAsFactors=F))
}
# 関数
DateCancel1 <- function(dataframe){
  dataframe[,c("シート名英数字別名", "症例登録番号", "本コース試験治療薬剤最終投与日")]
}

DateCancel2 <- function(dataframe){
  dataframe[,c("シート名英数字別名", "症例登録番号", "本サイクル試験治療薬剤最終投与日")]
}
# 最終薬剤投与日のみを抜き出す
for(i in c(1,3:25)){
  eval(parse(text = paste0("flowsheet", i, "<- DateCancel1(flowsheet", i, ")")))
}

for(i in c(26:41)){
  eval(parse(text = paste0("flowsheet", i, "<- DateCancel2(flowsheet", i, ")")))
}

cancel <- cancel[,c("症例登録番号", "プロトコール最終薬剤投与日")]

registration <- registration[,c("シート名英数字別名","症例登録番号")]

base <- merge(registration, cancel, by = "症例登録番号", all = TRUE)

# base <- merge(base, flowsheet1, by = "症例登録番号", all = TRUE)
# base <- merge(base, flowsheet3, by = "症例登録番号", all = TRUE)

for(i in c(1, 3:41)){
  eval(parse(text = paste0("colnames(flowsheet", i, ")[c(1, 3)] <- flowsheet", i, "$シート名英数字別名[1]")))
}

for(i in c(1, 3:41)){
  eval(parse(text = paste0("base <- merge(base, flowsheet",i,", by = '症例登録番号', all = TRUE)")))
}

result <- base
setwd(paste0(prtpath, "./output"))
write.csv(result, "ALL-B12_最終薬剤投与日.csv", row.names = F)
