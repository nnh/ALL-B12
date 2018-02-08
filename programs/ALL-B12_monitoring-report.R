# ALL-B12 定モニ用背景因子、中止症例
# Mamiko yonejima
# 20180208
#**********************************#
# 関数の定義
YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# dataの読み込み
#-- 設定-------------------------------------------------------------------------------------------------------------
# # output,rawdataはaronas上にて入出力する
prtpath <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/04.03.02 定期モニタリングレポート/第10回/R/report"
# # 締め切り日、ダウンロード日の
kDateShimekiri_srt <- "20170601"
kDateShimekiri <- "20171130"
kDownLoadDate <- "_180109_0955"
kJplsg <- "JPLSG_registration_180104_0947.csv"
#---------------------------------------------------------------------------------------------------------------------
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
dxt_jp <- jp[, c("登録コード", "生年月日", "性別")]

# registrationから必要項目抽出、JPとマージ、診断時年齢、診断年算出
dxt_reg <- registration[, c("登録コード", "症例登録番号", "診断年月日", "PSスコア.")]
df_1 <- merge(dxt_jp, dxt_reg, by = "登録コード", all.y = T )
df_1$age_diagnosis <- YearDif(df_1$生年月日, df_1$診断年月日)
df_1$year_diadnosis <- substr(df_1$診断年月日, 1, 4)

# initialを締め切り日でカット、必要項目抽出、マージ、提出状況確認
date_cut_initial <- initial[format(as.Date(initial$作成日), "%Y%m%d") <= kDateShimekiri, c("症例登録番号", "腫瘍芽球....1")]
df_2 <- merge(df_1, date_cut_initial, by = "症例登録番号", all.x = T)
df_2$initial_status <- ifelse(is.na(df_2$腫瘍芽球....1), "未提出",
                       ifelse(df_2$腫瘍芽球....1 == -1.0, "不検", "提出"))

# riskを締め切り日でカット、必要項目抽出、マージ
date_cut_risk1 <- risk1[format(as.Date(risk1$作成日), "%Y%m%d") <= kDateShimekiri, c("症例登録番号", "初発時CNS浸潤.", "PSL反応性評価", "HR因子.染色体本数.G.band.")]
df_3 <- merge(df_2, date_cut_risk1, by = "症例登録番号", all.x = T)
df_3$risk1_status <- ifelse(is.na(df_3$初発時CNS浸潤.), "未提出", "提出")

# 中止届を締め切り日でカット、必要項目抽出、マージ
date_cut_cancel <- cancel[format(as.Date(cancel$作成日), "%Y%m%d") <= kDateShimekiri, c("症例登録番号", "field10", "治療終了.中止.理由")]
ads <- merge(df_3, date_cut_cancel, by = "症例登録番号", all.x = T)

# 中止症例の詳細
dxt_cancel_0 <- cancel[format(as.Date(cancel$作成日), "%Y%m%d") >= kDateShimekiri_srt & cancel$終了種別 == "中止", ]
dxt_cancel <- dxt_cancel_0[format(as.Date(dxt_cancel_0$作成日), "%Y%m%d") <= kDateShimekiri, 
                         c("症例登録番号", "シート作成時施設名", "治療中止日", "中止時期.コース名.", "中止時期.day.week.", "中止時期.日数.週数.",
                           "中止判明日", "中止判明時期.コース名.", "中止判明時期.day.week.", "中止判明時期.day.week.", "field10", "治療終了.中止.理由", 
                           "有害事象詳細", "症例登録後.診断違い以外の不適格性が判明した場合の詳細", "有害事象や治療効果不十分以外の理由で.患者本人ないしは代諾者から中止の申し出があった場合の詳細",
                           "有害事象や治療効果不十分以外の理由で.担当医により中止が必要と判断された場合の詳細")]
dxt_reg_date <- registration[, c("症例登録番号","症例登録日")]

cancel_ds <- merge(dxt_cancel, dxt_reg_date, by = "症例登録番号", all.x = T)
cancel_ds$詳細 <- ifelse(cancel_ds$field10 == 14,  cancel_ds$有害事象詳細,
                  ifelse(cancel_ds$field10 == 4, cancel_ds$症例登録後.診断違い以外の不適格性が判明した場合の詳細,
                  ifelse(cancel_ds$field10 == 10,  cancel_ds$有害事象や治療効果不十分以外の理由で.患者本人ないしは代諾者から中止の申し出があった場合の詳細,
                  ifelse(cancel_ds$field10 == 11, cancel_ds$有害事象や治療効果不十分以外の理由で.担当医により中止が必要と判断された場合の詳細, "-" ))))
cancel_ds[is.na(cancel_ds)] <- "-"

write.csv(cancel_ds, paste0(prtpath, "./output/cancel.csv"), row.names =  F)
