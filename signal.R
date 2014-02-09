AddColumn(Buy)
AddColumn(Sell)
AddColumn(Short)
AddColumn(Cover)

R[,Sell:=ExRem(Sell, Buy), by=Instrument]
R[,Buy:=ExRem(Buy,(Sell|Short)), by=Instrument]
R[,Cover:=ExRem(Cover,Short), by=Instrument]
R[,Short:=ExRem(Short,(Cover|Buy)), by=Instrument]