regData <- soreg::get_arsrp("soreg")

d_full <- regData
d_full %<>% mutate(op_aar = year(Operasjonsdato), op_primar = (TidlFedmeOp==0))
d_full = d_full %>% mutate(bmi_baseline = BR_Vekt/(BR_Hoyde/100)^2,
                           bmi_op = OperasjonVekt/(BR_Hoyde/100)^2,
                           bmi_6v = `6U_Vekt`/(`6U_Hoyde`/100)^2,
                           bmi_1a = `1Aar_Vekt`/(`1Aar_Hoyde`/100)^2,
                           bmi_2a = `ToAar_Vekt`/(`ToAar_Hoyde`/100)^2)


soreg::makeHist(df = d_full, var = "bmi_baseline", bins = 12)
