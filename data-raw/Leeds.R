
EconUK <-
  read.table("Leeds.txt", header = TRUE, stringsAsFactors = FALSE) %>%
  select(Year, iYear, Country, iGDP, iK = iKstkPWT, iL, iXp, iXu = iU) %>%
  filter(Country == "UK", Year <= 2010)

devtools::use_data(EconUK, overwrite = TRUE)
