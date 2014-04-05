require(EconModels2)

All <- read.table("data/AllData.txt", header=TRUE)

model.cd1 <-  cdModel2( response = iGDP, capital=iCapStk, labor=iLabor, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.cd1 )

model.cd2 <- cdModel2( iGDP ~ iCapStk + iLabor + iYear, data=subset(All, Country=="US") )
naturalCoef( model.cd2 )

model.cde1 <- cdeModel2( response = iGDP, capital=iCapStk, labor=iLabor, energy=iU, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.cde1)

model.cde2 <- cdeModel2( iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(All, Country=="US") )
naturalCoef( model.cde2 )

model.linex1 <- linexModel2( iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(All, Country=="US") )
naturalCoef( model.linex1 )

model.linex2 <-  linexModel2( response=iGDP, capital= iCapStk, labor=iLabor, energy=iU, time= iYear, 
               data=subset(All, Country=="US") )
naturalCoef( model.linex2 )

model.sf1 <-  singleFactorModel2( iGDP ~ iCapStk + iYear, data=subset(All, Country=="US") )
naturalCoef( model.sf1 )

model.sf2 <- singleFactorModel2( response=iGDP, factor=iCapStk, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.sf2 )

model.sf3 <- singleFactorModel2( iGDP ~ iCapStk + iYear, data=subset(All, Country=="US"), constrained=TRUE )
naturalCoef( model.sf3 ) 

model.ces1 <- cesModel3( iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(All, Country=="US"))
naturalCoef(model.ces1)

model.ces2 <- cesModel3( iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(All, Country=="US"))
naturalCoef(model.ces2)
