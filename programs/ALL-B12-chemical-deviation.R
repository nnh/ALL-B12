# ALL-B12 薬剤逸脱
# Mamiko yonejima
# 20170925

# 体表面積計算
Body_Surface_Area <- function(high, weight){
  floor(sqrt(high* weight / 3600)*10^(2-1)+0.5)/10^(2-1)
}
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
DeviationList <- function(df, deviation, x){
    dev <- df[!is.na(deviation), c(2:16, 18, x)]
    dev$内容 <- names(df)[x]
    names(dev)[17] <- "deviation_percent"
    print(dev)
 }
DeviationList_L.ASP <- function(df, deviation, x, course){
  dev <- df[!is.na(deviation), c(2:16, 18, x)]
  dev$内容 <- names(df)[x]
  names(dev)[17] <- "deviation_percent"
  dev[is.na(dev$field3) | dev$field3 != course, ]##TODO
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
flowsheet1$VCR_実投与量_percent <- Chemical_Deviation(1,flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                   flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_実投与回数_percent <- Chemical_Deviation(2, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                   flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_総投与量_percent <- Chemical_Deviation(3, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                         flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
# PSL
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

dev1 <- DeviationList(flowsheet1,flowsheet1$Ip_PSL_210mg, 161)
dev2 <- DeviationList(flowsheet1,flowsheet1$VCR_実投与量_percent, 162)
dev3 <- DeviationList(flowsheet1,flowsheet1$VCR_実投与回数_percent, 163)
dev4 <- DeviationList(flowsheet1,flowsheet1$VCR_総投与量_percent, 164)
dev5 <- DeviationList(flowsheet1,flowsheet1$IA_PSL_総投与量_percent, 165)
dev6 <- DeviationList(flowsheet1,flowsheet1$DNR_実投与量_percent, 166)
dev7 <- DeviationList(flowsheet1,flowsheet1$DNR_実投与回数_percent, 167)
dev8 <- DeviationList(flowsheet1,flowsheet1$DNR_総投与量_percent, 168)
dev9 <- DeviationList_L.ASP(flowsheet1, flowsheet1$L.ASP_実投与量_percent, 169, 1)
dev10 <- DeviationList_L.ASP(flowsheet1,flowsheet1$L.ASP_実投与回数_percent, 170, 1)
dev11 <- DeviationList_L.ASP(flowsheet1,flowsheet1$L.ASP_総投与量_percent, 171, 1)
matSum <- NULL
for(i in 1:11){
  matSum <- rbind(matSum, eval(parse(text = paste0("dev", i))))
}
deviation1 <- matSum
write.csv(deviation1, "../output/deviation1.csv")
