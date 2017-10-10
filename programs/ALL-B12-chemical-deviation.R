# ALL-B12 薬剤逸脱
# Mamiko yonejima
# 20170925

# 体表面積計算
Body_Surface_Area <- function(high, weight){
  floor(sqrt(high* weight / 3600)*10^(2-1)+0.5)/10^(2-1)
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
# difine_total_d:M2:6.5, M5:15.5
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
DeviationList <- function(df, deviation, x){
    dev <- df[!is.na(deviation), c(2:16, 18, x)]
    dev$内容 <- names(df)[x]
    names(dev)[17] <- "deviation_percent"
    print(dev)
}
# L-ASP逸脱一覧
DeviationList_L.ASP_1 <- function(df, deviation, x, course){
  dev <- df[!is.na(deviation), c(2:16, 18, x)]
  dev <- dev[- grep(course, dev$field3), ]
  dev$内容 <- names(df)[x]
  names(dev)[17] <- "deviation_percent"
  print(dev)
}      
# 2リスク共通シートの場合のL-ASP逸脱一覧
DeviationList_L.ASP_2 <- function(df, deviation, x, course1, course2){
  dev <- df[!is.na(deviation), c(2:16, 18, x)]
  dev <- dev[- grep(course1, dev$field3), ]
  dev <- dev[- grep(course2, dev$field3), ]
  dev$内容 <- names(df)[x]
  names(dev)[17] <- "deviation_percent"
  print(dev)
}                                           


# ALL-B12-merge.Rで作成されたflowsheet～を読み込む
source("./programs/ALL-B12-merge-config.R", encoding = "UTF-8")
list <- list.files("./output", pattern = "flowsheet")
file.name <- sub(".csv.*", "", list)
setwd("./output")  # TODO yonejima 
for (i in 1:length(list)) {
  assign(file.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}

# flowsheet1
flowsheet1$Body_Surface_Area <- Body_Surface_Area(flowsheet1$身長.cm., flowsheet1$体重.kg.)
## Ip PSL
flowsheet1$Ip_PSL_210mg <- ifelse(flowsheet1$PSL総投与量.mg. < flowsheet1$Body_Surface_Area * 210, flowsheet1$PSL総投与量.mg., NA)
## VCR
flowsheet1$VCR_max2.0mg <- ifelse(flowsheet1$VCR実投与量.mg..回 > 2.0, flowsheet1$VCR実投与量.mg..回, NA)
flowsheet1$VCR_実投与量_percent <- Chemical_Deviation(1,flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                     flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                       flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                      flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
## PSL
flowsheet1$IA_PSL_総投与量_percent <- ifelse(flowsheet1$PSL総実投与量.mg./(flowsheet1$Body_Surface_Area * 60 * 20) < 0.5 | flowsheet1$PSL総実投与量.mg. / (flowsheet1$Body_Surface_Area * 60 * 20) >= 1.5, 
                                             floor(flowsheet1$PSL総実投与量.mg. / (flowsheet1$Body_Surface_Area * 60 * 20) *100 + 0.5), NA)
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
                                                    flowsheet6$HD.MTX実投与回数.4回, flowsheet6$HD.MTX減量.g., flowsheet6$ダウン症である, 6.5)
flowsheet6$HD.MTX_実投与回数_percent <- Chemical_Deviation_Down(2, flowsheet6$Body_Surface_Area, 2, 4, flowsheet6$HD.MTX実投与量.g..回,
                                                     flowsheet6$HD.MTX実投与回数.4回, flowsheet6$HD.MTX減量.g., flowsheet6$ダウン症である, 6.5)
flowsheet6$HD.MTX_総投与量_percent <- Chemical_Deviation_Down(3, flowsheet6$Body_Surface_Area, 2, 4, flowsheet6$HD.MTX実投与量.g..回,
                                                      flowsheet6$HD.MTX実投与回数.4回, flowsheet6$HD.MTX減量.g., flowsheet6$ダウン症である, 6.5)

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
                           "$HD.MTX実投与回数.4回, flowsheet", i, "$HD.MTX減量.g., flowsheet", i, "$ダウン症である, 15.5)")))
  eval(parse(text = paste0("flowsheet", i, "$HD.MTX_実投与回数_percent <- Chemical_Deviation_Down(2, flowsheet", i, 
                           "$Body_Surface_Area, 5, 4, flowsheet", i, "$HD.MTX実投与量.g..回, flowsheet",i,
                           "$HD.MTX実投与回数.4回, flowsheet", i, "$HD.MTX減量.g., flowsheet", i, "$ダウン症である, 15.5)")))
  eval(parse(text = paste0("flowsheet", i, "$HD.MTX_総投与量_percent <- Chemical_Deviation_Down(3, flowsheet", i, 
                           "$Body_Surface_Area, 5, 4, flowsheet", i, "$HD.MTX実投与量.g..回, flowsheet",i,
                           "$HD.MTX実投与回数.4回, flowsheet", i, "$HD.MTX減量.g., flowsheet", i, "$ダウン症である, 15.5)")))
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
flowsheet10$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet10$Body_Surface_Area, 20, 5, flowsheet10$DEX実投与量.mg..日,
                                                  flowsheet10$DEX実投与日数.5日, flowsheet10$DEX減量.mg.)
flowsheet10$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet10$Body_Surface_Area, 20, 5, flowsheet10$DEX実投与量.mg..日,
                                                    flowsheet10$DEX実投与日数.5日, flowsheet10$DEX減量.mg.)
flowsheet10$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet10$Body_Surface_Area, 20, 5, flowsheet10$DEX実投与量.mg..日,
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
flowsheet11$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet11$Body_Surface_Area, 20, 5, flowsheet11$DEX実投与量.mg..日,
                                                   flowsheet11$DEX実投与日数.5日, flowsheet11$DEX減量.mg.)
flowsheet11$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet11$Body_Surface_Area, 20, 5, flowsheet11$DEX実投与量.mg..日,
                                                    flowsheet11$DEX実投与日数.5日, flowsheet11$DEX減量.mg.)
flowsheet11$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet11$Body_Surface_Area, 20, 5, flowsheet11$DEX実投与量.mg..日,
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
flowsheet11$HD.MTX_実投与量_percent <- Chemical_Deviation(1, flowsheet11$Body_Surface_Area, 5, 1, flowsheet11$HD.MTX実投与量.g..回,
                                                   flowsheet11$HD.MTX実投与回数.1回, flowsheet11$HD.MTX減量.g.)
flowsheet11$HD.MTX_実投与回数_percent <- Chemical_Deviation(2, flowsheet11$Body_Surface_Area, 5, 1, flowsheet11$HD.MTX実投与量.g..回,
                                                       flowsheet11$HD.MTX実投与回数.1回, flowsheet11$HD.MTX減量.g.)
flowsheet11$HD.MTX_総投与量_percent <- Chemical_Deviation(3, flowsheet11$Body_Surface_Area, 5, 1, flowsheet11$HD.MTX実投与量.g..回,
                                                      flowsheet11$HD.MTX実投与回数.1回, flowsheet11$HD.MTX減量.g.)
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
flowsheet12$VCR_実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 20, 5, flowsheet12$DEX実投与量.mg..日,
                                                   flowsheet12$DEX実投与日数.5日, flowsheet12$DEX減量.mg.)
flowsheet12$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 20, 5, flowsheet12$DEX実投与量.mg..日,
                                                    flowsheet12$DEX実投与日数.5日, flowsheet12$DEX減量.mg.)
flowsheet12$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 20, 5, flowsheet12$DEX実投与量.mg..日,
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
flowsheet12$HD.MTX_実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 5, 1, flowsheet12$HD.MTX実投与量.g..回,
                                                      flowsheet12$HD.MTX実投与回数.1回, flowsheet12$HD.MTX減量.g.)
flowsheet12$HD.MTX_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 5, 1, flowsheet12$HD.MTX実投与量.g..回,
                                                       flowsheet12$HD.MTX実投与回数.1回, flowsheet12$HD.MTX減量.g.)
flowsheet12$HD.MTX_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 5, 1, flowsheet12$HD.MTX実投与量.g..回,
                                                      flowsheet12$HD.MTX実投与回数.1回, flowsheet12$HD.MTX減量.g.)
## CPA
flowsheet12$CPA_実投与量_percent <- Chemical_Deviation(1, flowsheet12$Body_Surface_Area, 200, 5, flowsheet12$CPA実投与量.mg..回,
                                                      flowsheet12$CPA実投与回数.5回, flowsheet12$CPA減量.mg.)
flowsheet12$CPA_実投与回数_percent <- Chemical_Deviation(2, flowsheet12$Body_Surface_Area, 200, 5, flowsheet12$CPA実投与量.mg..回,
                                                    flowsheet12$CPA実投与回数.5回, flowsheet12$CPA減量.mg.)
flowsheet12$CPA_総投与量_percent <- Chemical_Deviation(3, flowsheet12$Body_Surface_Area, 200, 5, flowsheet12$CPA実投与量.mg..回,
                                                   flowsheet12$CPA実投与回数.5回, flowsheet12$CPA減量.mg.)
## L.ASP(flowsheet10-12)
for(i in 10:12){
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 25000, 2, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.2回, flowsheet", i, "$L.ASP実投与量.U..回)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 25000, 2, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.2回, flowsheet", i, "$L.ASP実投与量.U..回)")))
  eval(parse(text = paste0("flowsheet", i,
                           "$L.ASP_実投与量_percent <- Chemical_Deviation(1, flowsheet", i, "$Body_Surface_Area, 25000, 2, flowsheet", i,
                           "$L.ASP実投与量.U..回, flowsheet", i, "$L.ASP実投与回数.2回, flowsheet", i, "$L.ASP実投与量.U..回)")))
}
# flowsheet13-20
## VCR
for(i in 13:20){
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




# 逸脱一覧の作成　DeviationList関数を使い、dev xxに逸脱のある行だけピックアップ
# df1 <- data.frame(
#   no = c(1:8),
#   column = c(161:168),
#   deviation = c("flowsheet1$Ip_PSL_210mg", "flowsheet1$VCR_実投与量_percent", "flowsheet1$VCR_実投与回数_percent", "VCR_総投与量_percent",
#                 "IA_PSL_総投与量_percent", "DNR_実投与量_percent", "DNR_実投与回数_percent", "DNR_総投与量_percent")
# )
# for(i in 1:length(df1$no)){
#   assign(paste0("dev",df1$no[i] ), DeviationList(flowsheet1, df1$deviation[i], df1$column[i]))
# }
# 
# df2 <- data.frame(
#   no = c(9:11), 
#   column = c(169:171),
#   deviation = c("L.ASP_実投与量_percent", "L.ASP_実投与回数_percent", "L.ASP_総投与量_percent")
# )
# for(i in 1:length(df2$no)){
#   assign(paste0("dev",df2$no[i]), DeviationList_L.ASP(flowsheet1, df2$deviation[i], df2$column[i], 1))
# }  # flowsheet1終わり
# dev1 <- DeviationList(flowsheet1,flowsheet1$Ip_PSL_210mg, 161)
# dev2 <- DeviationList(flowsheet1,flowsheet1$VCR_実投与量_percent, 162)
# dev3 <- DeviationList(flowsheet1,flowsheet1$VCR_実投与回数_percent, 163)
# dev4 <- DeviationList(flowsheet1,flowsheet1$VCR_総投与量_percent, 164)
# dev5 <- DeviationList(flowsheet1,flowsheet1$IA_PSL_総投与量_percent, 165)
# dev6 <- DeviationList(flowsheet1,flowsheet1$DNR_実投与量_percent, 166)
# dev7 <- DeviationList(flowsheet1,flowsheet1$DNR_実投与回数_percent, 167)
# dev8 <- DeviationList(flowsheet1,flowsheet1$DNR_総投与量_percent, 168)
# dev9 <- DeviationList_L.ASP_1(flowsheet1, flowsheet1$L.ASP_実投与量_percent, 169, 1)
# dev10 <- DeviationList_L.ASP_1(flowsheet1,flowsheet1$L.ASP_実投与回数_percent, 170, 1)
# dev11 <- DeviationList_L.ASP_1(flowsheet1,flowsheet1$L.ASP_総投与量_percent, 171, 1)
# dev12 <- DeviationList(flowsheet3, flowsheet3$CPA_実投与量_percent, 141)
# dev13 <- DeviationList(flowsheet3, flowsheet3$CPA_実投与回数_percent, 142)
# dev14 <- DeviationList(flowsheet3, flowsheet3$CPA_総投与量_percent, 143)
# dev15 <- DeviationList(flowsheet3, flowsheet3$Ara.C_実投与量_percent, 144)
# dev16 <- DeviationList(flowsheet3, flowsheet3$Ara.C_実投与回数_percent, 145)
# dev17 <- DeviationList(flowsheet3, flowsheet3$Ara.C_総投与量_percent, 146)
# dev18 <- DeviationList(flowsheet3, flowsheet3$X6.MP_実投与量_percent, 147)
# dev19 <- DeviationList(flowsheet3, flowsheet3$X6.MP_実投与回数量_percent, 148)
# dev20 <- DeviationList(flowsheet3, flowsheet3$X6.MP_総投与量_percent, 149)
# dev21 <- DeviationList(flowsheet4, flowsheet4$CPA_実投与量_percent, 144)
# dev22 <- DeviationList(flowsheet4, flowsheet4$CPA_実投与回数_percent, 145)
# dev23 <- DeviationList(flowsheet4, flowsheet4$CPA_総投与量_percent, 146)
# dev24 <- DeviationList(flowsheet4, flowsheet4$Ara.C_実投与量_percent, 147)
# dev25 <- DeviationList(flowsheet4, flowsheet4$Ara.C_実投与回数_percent, 148)
# dev26 <- DeviationList(flowsheet4, flowsheet4$Ara.C_総投与量_percent, 149)
# dev27 <- DeviationList(flowsheet4, flowsheet4$X6.MP_実投与量_percent, 150)
# dev28 <- DeviationList(flowsheet4, flowsheet4$X6.MP_実投与回数量_percent, 151)
# dev29 <- DeviationList(flowsheet4, flowsheet4$X6.MP_総投与量_percent, 152)
# dev30 <- DeviationList_L.ASP_2(flowsheet4, flowsheet4$L.ASP_実投与量_percent, 153, 25, 41)##TODOまたは41
# dev31 <- DeviationList_L.ASP_2(flowsheet4, flowsheet4$L.ASP_実投与回数量_percent, 154, 25, 41)
# dev32 <- DeviationList_L.ASP_2(flowsheet4, flowsheet4$L.ASP_総投与量_percent, 155, 25, 41)
# dev33 <- DeviationList(flowsheet5, flowsheet5$CPA_実投与量_percent, 147)
# dev34 <- DeviationList(flowsheet5, flowsheet5$CPA_実投与回数_percent, 148)
# dev35 <- DeviationList(flowsheet5, flowsheet5$CPA_総投与量_percent, 149)
# dev36 <- DeviationList(flowsheet5, flowsheet5$Ara.C_実投与量_percent, 150)
# dev37 <- DeviationList(flowsheet5, flowsheet5$Ara.C_実投与回数_percent, 151)
# dev38 <- DeviationList(flowsheet5, flowsheet5$Ara.C_総投与量_percent, 152)
# dev39 <- DeviationList(flowsheet5, flowsheet5$X6.MP_実投与量_percent, 153)
# dev40 <- DeviationList(flowsheet5, flowsheet5$X6.MP_実投与回数量_percent, 154)
# dev41 <- DeviationList(flowsheet5, flowsheet5$X6.MP_総投与量_percent, 155)
# dev42 <- DeviationList_L.ASP_1(flowsheet5, flowsheet5$L.ASP_実投与量_percent, 156)
# dev43 <- DeviationList_L.ASP_1(flowsheet5, flowsheet5$L.ASP_実投与回数量_percent, 157)
# dev44 <- DeviationList_L.ASP_1(flowsheet5, flowsheet5$L.ASP_総投与量_percent, 158)
# dev45 <- DeviationList(flowsheet5, flowsheet5$VCR_実投与量_percent, 159)
# dev46 <- DeviationList(flowsheet5, flowsheet5$VCR_実投与回数量_percent, 160)
# dev47 <- DeviationList(flowsheet5, flowsheet5$VCR_総投与量_percent, 161)
# matSum <- NULL
# for(i in 1:47){
#  try (
#   matSum <- rbind(matSum, eval(parse(text = paste0("dev", i)))))
# }
# matSum[is.na(matSum)] <- ""
# deviation1 <- matSum
# write.csv(deviation1, "../output/deviation1.csv")
flowsheet1[is.na(flowsheet1)] <- ""
dd <- flowsheet1[, c(16,19,24,161:171)]

write.csv(flowsheet9, "../output/flowsheet9_review.csv", row.names = F)

