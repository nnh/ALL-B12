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
  } else{
    total_dose <- ifelse(is.na(weight_loss), actual_dose1 * actual_dose2,
                         actual_dose1 * actual_dose2 - weight_loss)
    td_percent <- floor(total_dose / (dd* difine_dose2) * 100 + 0.5)
    ifelse(td_percent < 50 | td_percent >= 150,  td_percent, NA)
  }
}


# ALL-B12-merge.Rで作成されたflowsheet～を読み込む
list <- list.files("../output", pattern = "flowsheet")
file.name <- sub(".csv.*", "", list)
setwd("../output")  # TODO yonejima 
for (i in 1:length(list)) {
  assign(file.name[i], read.csv(list[i], as.is=T, na.strings = c("")))
}


flowsheet1$Body_Surface_Area <- Body_Surface_Area(flowsheet1$身長.cm., flowsheet1$体重.kg.)
flowsheet1$VCR_1dose_percent <- Chemical_Deviation(1,flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                   flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.) # TODO MAX2.0mg
flowsheet1$VCR_number_dose_percent <- Chemical_Deviation(2, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                   flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)
flowsheet1$VCR_total_dose_percent <- Chemical_Deviation(3, flowsheet1$Body_Surface_Area, 1.5, 4, flowsheet1$VCR実投与量.mg..回,
                                                         flowsheet1$VCR実投与回数.4回, flowsheet1$VCR減量.mg.)

