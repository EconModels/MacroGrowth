require(EconModels)
require(EconData)

data(Calvin2011)
All <- Calvin2011

model.cd1 <-  cdwoeModel( response = iY, capital=iK, labor=iL, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.cd1 )

model.cd2 <- cdwoeModel( iY ~ iK + iL + iYear, data=subset(All, Country=="US") )
naturalCoef( model.cd2 )

model.cde1 <- cdeModel( response = iY, capital=iK, labor=iL, energy=iU, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.cde1)

model.cde2 <- cdeModel( iY ~ iK + iL + iU + iYear, data=subset(All, Country=="US") )
naturalCoef( model.cde2 )

model.linex1 <- linexModel( iY ~ iK + iL + iU + iYear, data=subset(All, Country=="US") )
naturalCoef( model.linex1 )

model.linex2 <-  linexModel( response=iY, capital= iK, labor=iL, energy=iU, time= iYear, 
               data=subset(All, Country=="US") )
naturalCoef( model.linex2 )

model.sf1 <-  sfModel( iY ~ iK + iYear, data=subset(All, Country=="US") )
naturalCoef( model.sf1 )

model.sf2 <- sfModel( response=iY, factor=iK, time=iYear, data=subset(All, Country=="US") )
naturalCoef( model.sf2 )

model.sf3 <- sfModel( iY ~ iK + iYear, data=subset(All, Country=="US"), constrained=TRUE )
naturalCoef( model.sf3 ) 

model.ces1 <- cesModel( iY ~ iK + iL + iQ + iYear, data=subset(All, Country=="US"))
naturalCoef(model.ces1)

model.ces2 <- cesModel( iY ~ iK + iL + iU + iYear, data=subset(All, Country=="US"))
naturalCoef(model.ces2)
