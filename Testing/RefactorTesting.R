require(EconModels2)

all <- read.table("data/AllData.txt", header=TRUE)

naturalCoef(
  cdModel2( response = iGDP, capital=iCapStk, labor=iLabor, time=iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  cdModel2( iGDP ~ iCapStk + iLabor + iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  cdeModel2( response = iGDP, capital=iCapStk, labor=iLabor, energy=iQ, time=iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  cdeModel2( iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  linexModel2( iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  linexModel2( response=iGDP, capital= iCapStk, labor=iLabor, energy=iQ, time= iYear, 
               data=subset(all, Country=="US") )
)
naturalCoef(
  singleFactorModel2( iGDP ~ iCapStk + iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  singleFactorModel2( response=iGDP, factor=iCapStk, time=iYear, data=subset(all, Country=="US") )
)

naturalCoef(
  singleFactorModel2( iGDP ~ iCapStk + iYear, data=subset(all, Country=="US"), constrained=TRUE )
)

model <- cesModel3( iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(all, Country=="US"))
naturalCoef(model)
