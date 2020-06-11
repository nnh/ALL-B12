# ALL-B12 薬剤逸脱、クエリ発行、レビュー
# Mamiko yonejima
# 20170925

#関数の設定
# 体表面積計算
Body_Surface_Area <- function(high, weight){
  floor(sqrt(high* weight / 3600)*10^(3-1)+0.5)/10^(3-1)
}
# 薬剤逸脱
# flg == 1 一回投与量の逸脱、 flg == 2 投与回数の逸脱、 flg == 3 総投与量の逸脱
# bsa(body surface area), difine_dose1(規定一回投与量), difine_dose2(規定投与回数),
# actual_dose1(実投与量), actual_dose2(実投与回数), weight_loss(減量)
Chemical_Deviation <- function(flg, bsa, difine_dose1, difine_dose2, actual_dose1, actual_dose2, weight_loss){
  dd <- bsa* difine_dose1
  if(flg == 1){
    dd_percent <- floor(actual_dose1 / dd * 100 + 0.5) # Difine dose %
    ifelse(dd_percent < 50 | dd_percent >= 150,  dd_percent, NA)
  } else if (flg == 2){
    nd_percent <- floor(actual_dose2 / difine_dose2 * 100 + 0.5) # Number dose %
    ifelse(nd_percent < 50 | nd_percent >= 150,  nd_percent, NA)
  } else {
    total_dose <- ifelse(is.na(weight_loss), actual_dose1 * actual_dose2,
                         actual_dose1 * actual_dose2 - weight_loss)
    td_percent <- floor(total_dose / (dd* difine_dose2) * 100 + 0.5)
    ifelse(td_percent < 50 | td_percent >= 150,  td_percent, NA)
  }
}
# 薬剤逸脱 HD-MTX　Protocol M-Down
# difine_total_d:M2:6.5, M5:15.5,HR2,HR2:0.5
Chemical_Deviation_Down <- function(flg, bsa, difine_dose1, difine_dose2, actual_dose1, actual_dose2, weight_loss, down, difine_total_d){
  dd <- bsa* difine_dose1
  if(flg == 1){
    dd_percent <- ifelse(is.na(down), NA,
                         ifelse(down == "はい", NA, floor(actual_dose1 / dd * 100 + 0.5)))  # Difine dose %
    ifelse(is.na(dd_percent), NA,
           ifelse(dd_percent < 50 | dd_percent >= 150,  dd_percent, NA))
  } else if (flg == 2){
    nd_percent <- floor(actual_dose2 / difine_dose2 * 100 + 0.5) # Number dose %
    ifelse(nd_percent < 50 | nd_percent >= 150,  nd_percent, NA)
  } else {
    total_dose <- ifelse(is.na(weight_loss), actual_dose1 * actual_dose2,
                         actual_dose1 * actual_dose2 - weight_loss)
    td_percent <- ifelse(is.na(down), NA,
                         ifelse(down == "はい", floor(total_dose / (bsa* difine_total_d) * 100 + 0.5),
                                floor(total_dose / (dd* difine_dose2) * 100 + 0.5)))
    ifelse(td_percent < 50 | td_percent >= 150,  td_percent, NA)
  }
}

# 逸脱一覧
Deviation <- function(flowsheet) {
  no <- match("Body_Surface_Area", colnames(flowsheet)) + 1
  dxt <- flowsheet[, c(no:length(colnames(flowsheet)))]
  dxt_div <- dxt[!apply(is.na(dxt), 1, all), ]
  dxt_flowsheet <- flowsheet[, 1:26]
  merge(dxt_flowsheet, dxt_div, by="row.names",  all.y = T)
}
#----- Config -----------------------------------------------------------------------------------------------------
# output,rawdataはaronas上にて入出力する
prtpath <- "//192.168.200.222/Datacenter/Trials/JPLSG/22_ALL-B12/04.03.02 定期モニタリングレポート/第15回/R/cleaning"
# 締め切り日、ダウンロード日の
flg <- 1  # 1:締め切り日1つ設定バージョン、2:定モニバージョン（startの日も設定）
kDateShimekiri_start <- "2019601"  # flg==2の時に設定
kDateShimekiri <- "20200531"
kDownLoadDate <- "_200602_1528"
kJplsg <- "JPLSG_registration_200601_1858.csv"
#-------------------------------------------------------------------------------------------------------------------
source("./programs/ALL-B12-merge.R", encoding = "UTF-8")

# ALL-B12-merge.Rで作成されたflowsheet～を読み込む
list <- list.files(paste0(prtpath, "./output"), pattern = "flowsheet")
file.name <- sub(".csv.*", "", list)
setwd(paste0(prtpath,"./output")) # TODO yonejima
for (i in 1:length(list)) {
  assign(file.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}

# flowsheet1
flowsheet1$Body_Surface_Area <- Body_Surface_Area(flowsheet1$身長.cm., flowsheet1$体重.kg.)
## Ip PSL
flowsheet1$Ip_PSL_210mg_キテイ <- ifelse(flowsheet1$PSL総投与量.mg. < flowsheet1$Body_Surface_Area * 210, flowsheet1$Body_Surface_Area * 210, NA)
## VCR
flowsheet1$VCR_max2.0mg <- ifelse(flowsheet1$VCR実投与量.mg..回 > 2.0, flowsheet1$VCR実投与量.mg..回, NA)
flowsheet1$VCR_実投与量_percent <- Chemical_Deviation(1,flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                  flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                   flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                  flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
## PSL
flowsheet1$IA_PSL_総投与量_percent <- ifelse(flowsheet1$PSL総実投与量.mg./(flowsheet1$Body_Surface_Area * 60 * 21) < 0.5 | flowsheet1$PSL総実投与量.mg. / (flowsheet1$Body_Surface_Area * 60 * 20) >= 1.5,
                                         floor(flowsheet1$PSL総実投与量.mg. / (flowsheet1$Body_Surface_Area * 60 * 21) *100 + 0.5), NA)
## DNR ##TODOロジック確認
flowsheet1$DNR_実投与量_percent <- Chemical_Deviation(1,flowsheet1$Body_Surface_Area, 30, 2, flowsheet1$DNR実投与量.mg..回,
                                                  flowsheet1$DNR実投与回数.SR2回.IR.HR4回, flowsheet1$DNR減量.mg.)
flowsheet1$DNR_実投与回数_percent <- ifelse(flowsheet1$暫定リスク判定結果 == "標準危険群(SR)",
                                       Chemical_Deviation(2,flowsheet1$Body_Surface_Area, 30, 2, flowsheet1$DNR実投与量.mg..回,
                                                          flowsheet1$DNR実投与回数.SR2回.IR.HR4回, flowsheet1$DNR減量.mg.),
                                       Chemical_Deviation(2,flowsheet1$Body_Surface_Area, 30, 4, flowsheet1$DNR実投与量.mg..回,
                                                          flowsheet1$DNR実投与回数.SR2回.IR.HR4回, flowsheet1$DNR減量.mg.))
flowsheet1$DNR_総投与量_percent <- ifelse(flowsheet1$暫定リスク判定結果 == "標準危険群(SR)",
                                      Chemical_Deviation(3,flowsheet1$Body_Surface_Area, 30, 2, flowsheet1$DNR実投与量.mg..回,
                                                         flowsheet1$DNR実投与回数.SR2回.IR.HR4回, flowsheet1$DNR減量.mg.),
                                      Chemical_Deviation(3,flowsheet1$Body_Surface_Area, 30, 4, flowsheet1$DNR実投与量.mg..回,
                                                         flowsheet1$DNR実投与回数.SR2回.IR.HR4回, flowsheet1$DNR減量.mg.))
## L-ASP
flowsheet1$L.ASP_実投与量_percent <- Chemical_Deviation(1,flowsheet1$Body_Surface_Area, 5000, 8, flowsheet1$L.ASP実投与量.U..回,
                                                    flowsheet1$L.ASP実投与回数.8回, flowsheet1$L.ASP減量.Ｕ.)
flowsheet1$L.ASP_実投与回数_percent <- Chemical_Deviation(2,flowsheet1$Body_Surface_Area, 5000, 8, flowsheet1$L.ASP実投与量.U..回,
                                                     flowsheet1$L.ASP実投与回数.8回, flowsheet1$L.ASP減量.Ｕ.)
flowsheet1$L.ASP_総投与量_percent <- Chemical_Deviation(3,flowsheet1$Body_Surface_Area, 5000, 8, flowsheet1$L.ASP実投与量.U..回,
                                                    flowsheet1$L.ASP実投与回数.8回, flowsheet1$L.ASP減量.Ｕ.)

# flowsheet3-5
## CPA
for(i in 3:5){
  eval(parse(text = paste0("flowsheet", i,
                           "$Body_Surface_Area <- Body_Surface_Area(flowsheet", i, "$身長.cm., flowsheet", i, "$体重.kg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$CPA_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 1000, 2, flowsheet", i,
                           "$CPA実投与量.mg..回, flowsheet", i, "$CPA実投与回数.2回, flowsheet", i, "$CPA減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$CPA_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 1000, 2, flowsheet", i,
                           "$CPA実投与量.mg..回, flowsheet", i, "$CPA実投与回数.2回, flowsheet", i, "$CPA減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$CPA_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 1000, 2, flowsheet", i,
                           "$CPA実投与量.mg..回, flowsheet", i, "$CPA実投与回数.2回, flowsheet", i, "$CPA減量.mg.)")))
}
## Ara-C
for(i in 3:5){
  eval(parse(text = paste0("flowsheet", i,
                           "$Ara.C_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 75, 16, flowsheet", i,
                           "$Ara.C実投与量.mg..回, flowsheet", i, "$Ara.C実投与回数.16回, flowsheet", i, "$Ara.C減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$Ara.C_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 75, 16, flowsheet", i,
                           "$Ara.C実投与量.mg..回, flowsheet", i, "$Ara.C実投与回数.16回, flowsheet", i, "$Ara.C減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$Ara.C_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 75, 16, flowsheet", i,
                           "$Ara.C実投与量.mg..回, flowsheet", i, "$Ara.C実投与回数.16回, flowsheet", i, "$Ara.C減量.mg.)")))
}
## 6-MP
for(i in 3:5){
  eval(parse(text = paste0("flowsheet", i,
                           "$X6.MP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 60, 28, flowsheet", i,
                           "$X6.MP実投与量.mg..日, flowsheet", i, "$X6.MP実投与回数.28日, flowsheet", i, "$X6.MP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$X6.MP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 60, 28, flowsheet", i,
                           "$X6.MP実投与量.mg..日, flowsheet", i, "$X6.MP実投与回数.28日, flowsheet", i, "$X6.MP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$X6.MP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 60, 28, flowsheet", i,
                           "$X6.MP実投与量.mg..日, flowsheet", i, "$X6.MP実投与回数.28日, flowsheet", i, "$X6.MP減量.mg.)")))
}
## L-ASP(flowsheet4,5のみ)
## L.ASP
for(i in 4:5){
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 5000, 8, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.8回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 5000, 8, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.8回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 5000, 8, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.8回, flowsheet", i, "$L.ASP減量.U.)")))
}

## VCR
flowsheet5$VCR_max2.0mg <- ifelse(flowsheet5$VCR実投与量.mg..回 > 2.0, flowsheet5$VCR実投与量.mg..回, NA)
flowsheet5$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet5$Body_Surface_Area, 1.5, 2, flowsheet5$VCR実投与量.mg..回,
                                                  flowsheet5$VCR実投与回数.2回, flowsheet5$VCR減量.mg.)
flowsheet5$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet5$Body_Surface_Area, 1.5, 2, flowsheet5$VCR実投与量.mg..回,
                                                   flowsheet5$VCR実投与回数.2回, flowsheet5$VCR減量.mg.)
flowsheet5$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet5$Body_Surface_Area, 1.5, 2, flowsheet5$VCR実投与量.mg..回,
                                                  flowsheet5$VCR実投与回数.2回, flowsheet5$VCR減量.mg.)

# flowsheet6
flowsheet6$Body_Surface_Area <- Body_Surface_Area(flowsheet6$身長.cm., flowsheet6$体重.kg.)
## 6MP
flowsheet6$X6.MP_実投与量_percent <- Chemical_Deviation(1, flowsheet6$Body_Surface_Area, 25, 56, flowsheet6$X6.MP実投与量.mg..日,
                                                    flowsheet6$X6.MP実投与日数.56日, flowsheet6$X6.MP減量.mg.)
flowsheet6$X6.MP_実投与回数_percent <- Chemical_Deviation(2, flowsheet6$Body_Surface_Area, 25, 56, flowsheet6$X6.MP実投与量.mg..日,
                                                     flowsheet6$X6.MP実投与日数.56日, flowsheet6$X6.MP減量.mg.)
flowsheet6$X6.MP_総投与量_percent <- Chemical_Deviation(3, flowsheet6$Body_Surface_Area, 25, 56, flowsheet6$X6.MP実投与量.mg..日,
                                                    flowsheet6$X6.MP実投与日数.56日, flowsheet6$X6.MP減量.mg.)
## HD-MTX
flowsheet6$HD.MTX_実投与量_percent <- Chemical_Deviation_Down(1, flowsheet6$Body_Surface_Area, 2, 4, flowsheet6$HD.MTX実投与量.g..回,
                                                          flowsheet6$HD.MTX実投与回数.4回, flowsheet6$HD.MTX減量.g., flowsheet6$ダウン症である, 3.5)
flowsheet6$HD.MTX_実投与回数_percent <- Chemical_Deviation_Down(2, flowsheet6$Body_Surface_Area, 2, 4, flowsheet6$HD.MTX実投与量.g..回,
                                                           flowsheet6$HD.MTX実投与回数.4回, flowsheet6$HD.MTX減量.g., flowsheet6$ダウン症である, 3.5)
flowsheet6$HD.MTX_総投与量_percent <- Chemical_Deviation_Down(3, flowsheet6$Body_Surface_Area, 2, 4, flowsheet6$HD.MTX実投与量.g..回,
                                                          flowsheet6$HD.MTX実投与回数.4回, flowsheet6$HD.MTX減量.g., flowsheet6$ダウン症である, 3.5)

# flowsheet7～9
## 6MP
for(i in 7:9){
  eval(parse(text = paste0("flowsheet", i,
                           "$Body_Surface_Area <- Body_Surface_Area(flowsheet", i, "$身長.cm., flowsheet", i, "$体重.kg.)")))
  eval(parse(text = paste0("flowsheet", i, "$X6.MP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i,
                           "$Body_Surface_Area, 25, 56, flowsheet", i, "$X6.MP実投与量.mg..日, flowsheet",i,
                           "$X6.MP実投与日数.56日, flowsheet", i, "$X6.MP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i, "$X6.MP_実投与回数 <- Chemical_Deviation(2, flowsheet", i,
                           "$Body_Surface_Area, 25, 56, flowsheet", i, "$X6.MP実投与量.mg..日, flowsheet",i,
                           "$X6.MP実投与日数.56日, flowsheet", i, "$X6.MP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i, "$X6.MP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i,
                           "$Body_Surface_Area, 25, 56, flowsheet", i, "$X6.MP実投与量.mg..日, flowsheet",i,
                           "$X6.MP実投与日数.56日, flowsheet", i, "$X6.MP減量.mg.)")))
}
## HD-MTX
for(i in 7:9){
  eval(parse(text = paste0("flowsheet", i, "$HD.MTX_実投与量_percent <- Chemical_Deviation_Down(1, flowsheet", i,
                           "$Body_Surface_Area, 5, 4, flowsheet", i, "$HD.MTX実投与量.g..回, flowsheet",i,
                           "$HD.MTX実投与回数.4回, flowsheet", i, "$HD.MTX減量.g., flowsheet", i, "$ダウン症である, 3.5)")))
  eval(parse(text = paste0("flowsheet", i, "$HD.MTX_実投与回数_percent <- Chemical_Deviation_Down(2, flowsheet", i,
                           "$Body_Surface_Area, 5, 4, flowsheet", i, "$HD.MTX実投与量.g..回, flowsheet",i,
                           "$HD.MTX実投与回数.4回, flowsheet", i, "$HD.MTX減量.g., flowsheet", i, "$ダウン症である, 3.5)")))
  eval(parse(text = paste0("flowsheet", i, "$HD.MTX_総投与量_percent <- Chemical_Deviation_Down(3, flowsheet", i,
                           "$Body_Surface_Area, 5, 4, flowsheet", i, "$HD.MTX実投与量.g..回, flowsheet",i,
                           "$HD.MTX実投与回数.4回, flowsheet", i, "$HD.MTX減量.g., flowsheet", i, "$ダウン症である, 3.5)")))
}
## L-ASP(flowsheet8,9のみ)
## L.ASP
for(i in 8:9){
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 12500, 4, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.4回, flowsheet", i, "$L.ASP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 12500, 4, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.4回, flowsheet", i, "$L.ASP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 12500, 4, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.4回, flowsheet", i, "$L.ASP減量.mg.)")))
}


## VCR
flowsheet9$VCR_max2.0mg <- ifelse(flowsheet9$VCR実投与量.mg..回 > 2.0, flowsheet9$VCR実投与量.mg..回, NA)
flowsheet9$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet9$Body_Surface_Area, 1.5, 4, flowsheet9$VCR実投与量.mg..回,
                                                  flowsheet9$VCR実投与回数.4回, flowsheet9$VCR減量.mg.)
flowsheet9$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet9$Body_Surface_Area, 1.5, 4, flowsheet9$VCR実投与量.mg..回,
                                                   flowsheet9$VCR実投与回数.4回, flowsheet9$VCR減量.mg.)
flowsheet9$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet9$Body_Surface_Area, 1.5, 4, flowsheet9$VCR実投与量.mg..回,
                                                  flowsheet9$VCR実投与回数.4回, flowsheet9$VCR減量.mg.)

# flowsheet10
flowsheet10$Body_Surface_Area <- Body_Surface_Area(flowsheet10$身長.cm., flowsheet10$体重.kg.)
## DEX
flowsheet10$DEX_実投与量_percent <- Chemical_Deviation(1, flowsheet10$Body_Surface_Area, 20, 5, flowsheet10$DEX実投与量.mg..日,
                                                   flowsheet10$DEX実投与日数.5日, flowsheet10$DEX減量.mg.)
flowsheet10$DEX_実投与回数_percent <- Chemical_Deviation(2, flowsheet10$Body_Surface_Area, 20, 5, flowsheet10$DEX実投与量.mg..日,
                                                    flowsheet10$DEX実投与日数.5日, flowsheet10$DEX減量.mg.)
flowsheet10$DEX_総投与量_percent <- Chemical_Deviation(3, flowsheet10$Body_Surface_Area, 20, 5, flowsheet10$DEX実投与量.mg..日,
                                                   flowsheet10$DEX実投与日数.5日, flowsheet10$DEX減量.mg.)
## HD-Ara-C
flowsheet10$HD.Ara.C_実投与量_percent <- Chemical_Deviation(1, flowsheet10$Body_Surface_Area, 2000, 4, flowsheet10$HD.Ara.C実投与量.mg..回,
                                                        flowsheet10$HD.Ara.C実投与回数.4回, flowsheet10$HD.Ara.C減量.mg.)
flowsheet10$HD.Ara.C_実投与回数_percent <- Chemical_Deviation(2, flowsheet10$Body_Surface_Area, 2000, 4, flowsheet10$HD.Ara.C実投与量.mg..回,
                                                         flowsheet10$HD.Ara.C実投与回数.4回, flowsheet10$HD.Ara.C減量.mg.)
flowsheet10$HD.Ara.C_総投与量_percent <- Chemical_Deviation(3, flowsheet10$Body_Surface_Area, 2000, 4, flowsheet10$HD.Ara.C実投与量.mg..回,
                                                        flowsheet10$HD.Ara.C実投与回数.4回, flowsheet10$HD.Ara.C減量.mg.)

## VP-16
flowsheet10$VP.16_実投与量_percent <- Chemical_Deviation(1, flowsheet10$Body_Surface_Area, 100, 5, flowsheet10$VP.16実投与量.mg..回,
                                                     flowsheet10$VP.16実投与回数.5回, flowsheet10$VP.16減量.mg.)
flowsheet10$VP.16_実投与回数_percent <- Chemical_Deviation(2, flowsheet10$Body_Surface_Area, 100, 5, flowsheet10$VP.16実投与量.mg..回,
                                                      flowsheet10$VP.16実投与回数.5回, flowsheet10$VP.16減量.mg.)
flowsheet10$VP.16_総投与量_percent <- Chemical_Deviation(3, flowsheet10$Body_Surface_Area, 100, 5, flowsheet10$VP.16実投与量.mg..回,
                                                     flowsheet10$VP.16実投与回数.5回, flowsheet10$VP.16減量.mg.)


# flowsheet11
flowsheet11$Body_Surface_Area <- Body_Surface_Area(flowsheet11$身長.cm., flowsheet11$体重.kg.)
## DEX
flowsheet11$DEX_実投与量_percent <- Chemical_Deviation(1, flowsheet11$Body_Surface_Area, 20, 5, flowsheet11$DEX実投与量.mg..日,
                                                   flowsheet11$DEX実投与日数.5日, flowsheet11$DEX減量.mg.)
flowsheet11$DEX_実投与回数_percent <- Chemical_Deviation(2, flowsheet11$Body_Surface_Area, 20, 5, flowsheet11$DEX実投与量.mg..日,
                                                    flowsheet11$DEX実投与日数.5日, flowsheet11$DEX減量.mg.)
flowsheet11$DEX_総投与量_percent <- Chemical_Deviation(3, flowsheet11$Body_Surface_Area, 20, 5, flowsheet11$DEX実投与量.mg..日,
                                                   flowsheet11$DEX実投与日数.5日, flowsheet11$DEX減量.mg.)
## VDS
flowsheet11$VDS_max5.0mg <- ifelse(flowsheet11$VDS実投与量.mg..回 > 5.0, flowsheet11$VDS実投与量.mg..回, NA)
flowsheet11$VDS_実投与量_percent <- Chemical_Deviation(1, flowsheet11$Body_Surface_Area, 3.0, 2, flowsheet11$VDS実投与量.mg..回,
                                                   flowsheet11$VDS実投与回数.2回, flowsheet11$VDS減量.mg.)
flowsheet11$VDS_実投与回数_percent <- Chemical_Deviation(2, flowsheet11$Body_Surface_Area, 3.0, 2, flowsheet11$VDS実投与量.mg..回,
                                                    flowsheet11$VDS実投与回数.2回, flowsheet11$VDS減量.mg.)
flowsheet11$VDS_総投与量_percent <- Chemical_Deviation(3, flowsheet11$Body_Surface_Area, 3.0, 2, flowsheet11$VDS実投与量.mg..回,
                                                   flowsheet11$VDS実投与回数.2回, flowsheet11$VDS減量.mg.)
## HD-MTX
flowsheet11$HD.MTX_実投与量_percent <- Chemical_Deviation_Down(1, flowsheet11$Body_Surface_Area, 5, 1, flowsheet11$HD.MTX実投与量.g..回,
                                                      flowsheet11$HD.MTX実投与回数.1回, flowsheet11$HD.MTX減量.g., flowsheet11$ダウン症である, 0.5)
flowsheet11$HD.MTX_実投与回数_percent <- Chemical_Deviation_Down(2, flowsheet11$Body_Surface_Area, 5, 1, flowsheet11$HD.MTX実投与量.g..回,
                                                       flowsheet11$HD.MTX実投与回数.1回, flowsheet11$HD.MTX減量.g., flowsheet11$ダウン症である, 0.5)
flowsheet11$HD.MTX_総投与量_percent <- Chemical_Deviation_Down(3, flowsheet11$Body_Surface_Area, 5, 1, flowsheet11$HD.MTX実投与量.g..回,
                                                      flowsheet11$HD.MTX実投与回数.1回, flowsheet11$HD.MTX減量.g., flowsheet11$ダウン症である, 0.5)
## IFO
flowsheet11$IFO_実投与量_percent <- Chemical_Deviation(1, flowsheet11$Body_Surface_Area, 800, 5, flowsheet11$IFO実投与量.mg..回,
                                                   flowsheet11$IFO実投与回数.5回, flowsheet11$IFO減量.mg.)
flowsheet11$IFO_実投与回数_percent <- Chemical_Deviation(2, flowsheet11$Body_Surface_Area, 800, 5, flowsheet11$IFO実投与量.mg..回,
                                                    flowsheet11$IFO実投与回数.5回, flowsheet11$IFO減量.mg.)
flowsheet11$IFO_総投与量_percent <- Chemical_Deviation(3, flowsheet11$Body_Surface_Area, 800, 5, flowsheet11$IFO実投与量.mg..回,
                                                   flowsheet11$IFO実投与回数.5回, flowsheet11$IFO減量.mg.)
## DNR
flowsheet11$DNR_実投与量_percent <- Chemical_Deviation(1, flowsheet11$Body_Surface_Area, 30, 1, flowsheet11$DNR実投与量.mg..回,
                                                   flowsheet11$DNR実投与回数.1回, flowsheet11$DNR減量.mg.)
flowsheet11$DNR_実投与回数_percent <- Chemical_Deviation(2, flowsheet11$Body_Surface_Area, 30, 1, flowsheet11$DNR実投与量.mg..回,
                                                    flowsheet11$DNR実投与回数.1回, flowsheet11$DNR減量.mg.)
flowsheet11$DNR_総投与量_percent <- Chemical_Deviation(3, flowsheet11$Body_Surface_Area, 30, 1, flowsheet11$DNR実投与量.mg..回,
                                                   flowsheet11$DNR実投与回数.1回, flowsheet11$DNR減量.mg.)

# flowsheet12
flowsheet12$Body_Surface_Area <- Body_Surface_Area(flowsheet12$身長.cm., flowsheet12$体重.kg.)
## DEX
flowsheet12$DEX_実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 20, 5, flowsheet12$DEX実投与量.mg..日,
                                                   flowsheet12$DEX実投与日数.5日, flowsheet12$DEX減量.mg.)
flowsheet12$DEX_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 20, 5, flowsheet12$DEX実投与量.mg..日,
                                                    flowsheet12$DEX実投与日数.5日, flowsheet12$DEX減量.mg.)
flowsheet12$DEX_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 20, 5, flowsheet12$DEX実投与量.mg..日,
                                                   flowsheet12$DEX実投与日数.5日, flowsheet12$DEX減量.mg.)
## VCR
flowsheet12$VCR_max2.0mg <- ifelse(flowsheet12$VCR実投与量.mg..回 > 2.0, flowsheet12$VCR実投与量.mg..回, NA)
flowsheet12$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 1.5, 2, flowsheet12$VCR実投与量.mg..回,
                                                   flowsheet12$VCR実投与回数.2回, flowsheet12$VCR減量.mg.)
flowsheet12$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 1.5, 2, flowsheet12$VCR実投与量.mg..回,
                                                    flowsheet12$VCR実投与回数.2回, flowsheet12$VCR減量.mg.)
flowsheet12$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 1.5, 2, flowsheet12$VCR実投与量.mg..回,
                                                   flowsheet12$VCR実投与回数.2回, flowsheet12$VCR減量.mg.)
## HD-MTX
flowsheet12$HD.MTX_実投与量_percent <- Chemical_Deviation_Down(1, flowsheet12$Body_Surface_Area, 5, 1, flowsheet12$HD.MTX実投与量.g..回,
                                                      flowsheet12$HD.MTX実投与回数.1回, flowsheet12$HD.MTX減量.g., flowsheet12$ダウン症である, 0.5)
flowsheet12$HD.MTX_実投与回数_percent <- Chemical_Deviation_Down(2, flowsheet12$Body_Surface_Area, 5, 1, flowsheet12$HD.MTX実投与量.g..回,
                                                       flowsheet12$HD.MTX実投与回数.1回, flowsheet12$HD.MTX減量.g., flowsheet12$ダウン症である, 0.5)
flowsheet12$HD.MTX_総投与量_percent <- Chemical_Deviation_Down(3, flowsheet12$Body_Surface_Area, 5, 1, flowsheet12$HD.MTX実投与量.g..回,
                                                      flowsheet12$HD.MTX実投与回数.1回, flowsheet12$HD.MTX減量.g., flowsheet12$ダウン症である, 0.5)
## CPA
flowsheet12$CPA_実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 200, 5, flowsheet12$CPA実投与量.mg..回,
                                                   flowsheet12$CPA実投与回数.5回, flowsheet12$CPA減量.mg.)
flowsheet12$CPA_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 200, 5, flowsheet12$CPA実投与量.mg..回,
                                                    flowsheet12$CPA実投与回数.5回, flowsheet12$CPA減量.mg.)
flowsheet12$CPA_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 200, 5, flowsheet12$CPA実投与量.mg..回,
                                                   flowsheet12$CPA実投与回数.5回, flowsheet12$CPA減量.mg.)
## HD.Ara.C
flowsheet12$HD.Ara.C実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 2000, 2, flowsheet12$HD.Ara.C実投与量.mg..回,
                                                   flowsheet12$HD.Ara.C実投与回数.2回, flowsheet12$HD.Ara.C減量.mg.)
flowsheet12$HD.Ara.C_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 2000, 2, flowsheet12$HD.Ara.C実投与量.mg..回,
                                                         flowsheet12$HD.Ara.C実投与回数.2回, flowsheet12$HD.Ara.C減量.mg.)
flowsheet12$HD.Ara.C_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 2000, 2, flowsheet12$HD.Ara.C実投与量.mg..回,
                                                        flowsheet12$HD.Ara.C実投与回数.2回, flowsheet12$HD.Ara.C減量.mg.)
## L.ASP(flowsheet10-12)
names(flowsheet10)[51] <- "L.ASP減量.U."
for(i in 10:12){
  eval(parse(text = paste0("flowsheet", i,
                        "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 25000, 2, flowsheet", i,
                          "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.2回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                          "$L.ASP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 25000, 2, flowsheet", i,
                         "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.2回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 25000, 2, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.2回, flowsheet", i, "$L.ASP減量.U.)")))
}
# flowsheet13-20
## VCR
for(i in 13:17){
  eval(parse(text = paste0("flowsheet", i,
                           "$Body_Surface_Area <- Body_Surface_Area(flowsheet", i, "$身長.cm., flowsheet", i, "$体重.kg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$VCR_max2.0mg <- ifelse(flowsheet", i, "$VCR実投与量.mg..回 > 2.0, flowsheet", i,
                           "$VCR実投与量.mg..回, NA)")))
  eval(parse(text = paste0("flowsheet", i, "$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet", i,
                           "$Body_Surface_Area, 1.5, 2, flowsheet", i, "$VCR実投与量.mg..回, flowsheet",i,
                           "$VCR実投与回数.2回, flowsheet", i, "$VCR減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i, "$VCR_実投与回数 <- Chemical_Deviation(2, flowsheet", i,
                           "$Body_Surface_Area, 1.5, 2, flowsheet", i, "$VCR実投与量.mg..回, flowsheet",i,
                           "$VCR実投与回数.2回, flowsheet", i, "$VCR減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i, "$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet", i,
                           "$Body_Surface_Area, 1.5, 2, flowsheet", i, "$VCR実投与量.mg..回, flowsheet",i,
                           "$VCR実投与回数.2回, flowsheet", i, "$VCR減量.mg.)")))
}
for(i in 18:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$Body_Surface_Area <- Body_Surface_Area(flowsheet", i, "$身長.cm., flowsheet", i, "$体重.kg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$VCR_max2.0mg <- ifelse(flowsheet", i, "$VCR実投与量.mg..回 > 2.0, flowsheet", i,
                           "$VCR実投与量.mg..回, NA)")))
  eval(parse(text = paste0("flowsheet", i, "$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet", i,
                           "$Body_Surface_Area, 1.5, 4, flowsheet", i, "$VCR実投与量.mg..回, flowsheet",i,
                           "$VCR実投与回数.4回, flowsheet", i, "$VCR減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i, "$VCR_実投与回数 <- Chemical_Deviation(2, flowsheet", i,
                           "$Body_Surface_Area, 1.5, 4, flowsheet", i, "$VCR実投与量.mg..回, flowsheet",i,
                           "$VCR実投与回数.4回, flowsheet", i, "$VCR減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i, "$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet", i,
                           "$Body_Surface_Area, 1.5, 4, flowsheet", i, "$VCR実投与量.mg..回, flowsheet",i,
                           "$VCR実投与回数.4回, flowsheet", i, "$VCR減量.mg.)")))
}
## DEX
for(i in 13:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$DEX_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 10, 14, flowsheet", i,
                           "$DEX実投与量.mg..日, flowsheet", i, "$DEX実投与日数.14日, flowsheet", i, "$減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$DEX_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 10, 14, flowsheet", i,
                           "$DEX実投与量.mg..日, flowsheet", i, "$DEX実投与日数.14日, flowsheet", i, "$減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$DEX_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 10, 14, flowsheet", i,
                           "$DEX実投与量.mg..日, flowsheet", i, "$DEX実投与日数.14日, flowsheet", i, "$減量.mg.)")))
}
## THP
flowsheet13$THP実投与量.mg..回 <- as.numeric(flowsheet13$THP実投与量.mg..回)
for(i in 13:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$THP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 25, 2, flowsheet", i,
                           "$THP実投与量.mg..回, flowsheet", i, "$THP実投与回数.2回, flowsheet", i, "$THP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$THP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 25, 2, flowsheet", i,
                           "$THP実投与量.mg..回, flowsheet", i, "$THP実投与回数.2回, flowsheet", i, "$THP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$THP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 25, 2, flowsheet", i,
                           "$THP実投与量.mg..回, flowsheet", i, "$THP実投与回数.2回, flowsheet", i, "$THP減量.mg.)")))
}
## L-ASP
for(i in 13:14){
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 10000, 4, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.4回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 10000, 4, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.4回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 10000, 4, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.4回, flowsheet", i, "$L.ASP減量.U.)")))
}
for(i in 15:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 10000, 8, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.8回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 10000, 8, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.8回, flowsheet", i, "$L.ASP減量.U.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 10000, 8, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.8回, flowsheet", i, "$L.ASP減量.U.)")))
}
## CPA
for(i in 13:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$CPA_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 500, 1, flowsheet", i,
                           "$CPA実投与量.mg..回, flowsheet", i, "$CPA実投与回数.1回, flowsheet", i, "$CPA減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$CPA_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 500, 1, flowsheet", i,
                           "$CPA実投与量.mg..回, flowsheet", i, "$CPA実投与回数.1回, flowsheet", i, "$CPA減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$CPA_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 500, 1, flowsheet", i,
                           "$CPA実投与量.mg..回, flowsheet", i, "$CPA実投与回数.1回, flowsheet", i, "$CPA減量.mg.)")))
}
## Ara-C
for(i in 13:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$Ara.C_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 75, 8, flowsheet", i,
                           "$Ara.C実投与量.mg..回, flowsheet", i, "$Ara.C実投与回数.8回, flowsheet", i, "$Ara.C減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$Ara.C_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 75, 8, flowsheet", i,
                           "$Ara.C実投与量.mg..回, flowsheet", i, "$Ara.C実投与回数.8回, flowsheet", i, "$Ara.C減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$Ara.C_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 75, 8, flowsheet", i,
                           "$Ara.C実投与量.mg..回, flowsheet", i, "$Ara.C実投与回数.8回, flowsheet", i, "$Ara.C減量.mg.)")))
}
## 6-MP
for(i in 13:20){
  eval(parse(text = paste0("flowsheet", i,
                           "$X6.MP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 60, 14, flowsheet", i,
                           "$X6.MP実投与量.mg..日, flowsheet", i, "$X6.MP実投与回数.14日, flowsheet", i, "$X6.MP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$X6.MP_実投与回数_percent <- Chemical_Deviation(2, flowsheet", i, "$Body_Surface_Area, 60, 14, flowsheet", i,
                           "$X6.MP実投与量.mg..日, flowsheet", i, "$X6.MP実投与回数.14日, flowsheet", i, "$X6.MP減量.mg.)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$X6.MP_総投与量_percent <- Chemical_Deviation(3, flowsheet", i, "$Body_Surface_Area, 60, 14, flowsheet", i,
                           "$X6.MP実投与量.mg..日, flowsheet", i, "$X6.MP実投与回数.14日, flowsheet", i, "$X6.MP減量.mg.)")))
}

# 中間維持療法は体表面積のみ計算
for(i in c(21:26, 30, 34, 38, 42)){
  eval(parse(text = paste0("flowsheet", i,
                           "$Body_Surface_Area <- Body_Surface_Area(flowsheet", i, "$身長.cm., flowsheet", i, "$体重.kg.)")))}
# 逸脱一覧を作成する
for(i in c(1, 3:20)){
  assign(paste0("flowsheet_chemical_dev", i), Deviation(eval(parse(text = paste0("flowsheet", i)))))
}

# Output
# dir.create("../output/review")
for(i in c(1, 3:20)){
  eval(parse(text = paste0("flowsheet", i, "[is.na(flowsheet", i, ")] <- ''")))
  eval(parse(text = paste0("write.csv(flowsheet", i, ",'../output/review/flowsheet", i, "_review.csv', row.names = F)")))}
for(i in c(21:26, 30, 34, 38, 42)){
  eval(parse(text = paste0("flowsheet", i, "[is.na(flowsheet", i, ")] <- ''")))
  eval(parse(text = paste0("write.csv(flowsheet", i, ",'../output/review/flowsheet", i, "_review_bsa.csv', row.names = F)")))}
for(i in c(27:29, 31:33, 35:37, 39:41)){
  eval(parse(text = paste0("flowsheet", i, "[is.na(flowsheet", i, ")] <- ''")))
  eval(parse(text = paste0("write.csv(flowsheet", i, ",'../output/review/flowsheet", i, "_review_bsa.csv', row.names = F)")))}
flowsheet43[is.na(flowsheet43)] <- ""
write.csv(flowsheet43, "../output/review/flowsheet43_raw.csv", row.names = F)
dir.create("../output/deviation")
for(i in c(1, 3:20)){
  eval(parse(text = paste0("flowsheet_chemical_dev", i, "[is.na(flowsheet_chemical_dev", i, ")] <- ''")))
  eval(parse(text = paste0("write.csv(flowsheet_chemical_dev", i, ",'../output/deviation/flowsheet", i, "_chemical_dev.csv', row.names = F)")))}

