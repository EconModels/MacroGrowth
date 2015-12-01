require(EconData)
require(mosaic)
PT <- IST %>% filter(Country == "PT") %>% head(-2)

( ediff(PT$iGDP) / ediff(PT$iKservS.L ) ) / (PT$iGDP / PT$iKservS.L) -> oK
( ediff(PT$iGDP) / ediff(PT$ihLest ) ) / (PT$iGDP / PT$ihLest) -> oL
( ediff(PT$iGDP) / ediff(PT$iUMP ) ) / (PT$iGDP / PT$iUMP) -> oE

PT $ sGDP <- predict(loess(iGDP ~ iYear, data = PT))
PT $ sK <- predict(loess(iKservS.L ~ iYear, data = PT))
PT $ sL <- c(predict(loess(ihLest ~ iYear, data = PT)), NA)
PT $ sU <- c(predict(loess(iUMP ~ iYear, data = PT)), NA, NA)

( ediff(PT$sGDP) / ediff(PT$sK ) ) / (PT$sGDP / PT$sK) -> oKs
( ediff(PT$sGDP) / ediff(PT$sL ) ) / (PT$sGDP / PT$sL) -> oLs
( ediff(PT$sGDP) / ediff(PT$sU ) ) / (PT$sGDP / PT$sU) -> oEs

qplot(x = Year, y = oLs, data = PT, geom = c("point", "smooth"))
qplot(x = Year, y = oEs, data = PT, geom = c("point", "smooth"))
qplot(x = Year, y = oKs, data = PT, geom = c("point", "smooth"))
