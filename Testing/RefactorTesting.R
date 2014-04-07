require(EconModels2)

All <- read.table("data/AllData.txt", header=TRUE)

model.cd1 <-  cdModel( response = iGDP, capital=iCapStk, labor=iLabor, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.cd1 )

model.cd2 <- cdModel( iGDP ~ iCapStk + iLabor + iYear, data=subset(All, Country=="US") )
naturalCoef( model.cd2 )

model.cde1 <- cdeModel( response = iGDP, capital=iCapStk, labor=iLabor, energy=iU, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.cde1)

model.cde2 <- cdeModel( iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(All, Country=="US") )
naturalCoef( model.cde2 )

model.linex1 <- linexModel( iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(All, Country=="US") )
naturalCoef( model.linex1 )

model.linex2 <-  linexModel( response=iGDP, capital= iCapStk, labor=iLabor, energy=iU, time= iYear, 
               data=subset(All, Country=="US") )
naturalCoef( model.linex2 )

model.sf1 <-  singleFactorModel( iGDP ~ iCapStk + iYear, data=subset(All, Country=="US") )
naturalCoef( model.sf1 )

model.sf2 <- singleFactorModel( response=iGDP, factor=iCapStk, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.sf2 )

model.sf3 <- singleFactorModel( iGDP ~ iCapStk + iYear, data=subset(All, Country=="US"), constrained=TRUE )
naturalCoef( model.sf3 ) 

model.ces1 <- cesModel( iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(All, Country=="US"))
naturalCoef(model.ces1)

model.ces2 <- cesModel( iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(All, Country=="US"))
naturalCoef(model.ces2)
