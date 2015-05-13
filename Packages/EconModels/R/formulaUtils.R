# some formula utilities

numeric_choices <- function(names_or_numbers, choices) {
  if (is.null(names_or_numbers)) { names_or_numbers <- choices }
  if (is.numeric(names_or_numbers)) {
    res <- names_or_numbers[names_or_numbers <= length(choices)]
  } else {
    res <- which(choices %in% names_or_numbers)
  }
  names(res) <- choices[res]
  res
}

build_formula <- function( left, right, env=parent.frame() ) {
  res <- as.formula(
    gsub( "\\+ *\\+", "+", 
          paste(left, paste(right, collapse = "+"), sep="~")
    )
  )
  environment(res) <- env
  res
}

# drop term n from the RHS of a formula
# this isn't very robust if "terms" have + in them.  But for simple things it will work.

keep_rhs <- function(formula, n, env=parent.frame()) {
  formulaString <- as.character(formula)
  formulaSplit <- strsplit(formulaString, " ~ ")[[1]]
  l <- formulaSplit[1]
  r <- strsplit( formulaSplit[2], " \\+ ")[[1]]
  build_formula( l, r[n], env=env)
}
  

# examples

f <- build_formula( "iGDP", list("iK", "iL", "iQP", "iYear")); f
g <- build_formula( "iGDP", c("iK", "iL", "iQP", "iYear")); g
identical(f,g)
# use negative indices to drop
keep_rhs(f, -3)
keep_rhs(f, 2:4)
keep_rhs(f, -(2:4))


CDformulas <- 
  list( 
    PT = c(unadjusted = iGDP ~ iKstkS.L        + iL     + iXpMP + iYear,
           adjusted = iGDP ~ iKservS.L       + ihLest + iUMP  + iYear),
    UK = c(unadjusted = iGDP ~ iKserv0.WwithRD + iL     + iXp   + iYear,
           adjusted = iGDP ~ iKserv0.WwithRD + ihLest + iU    + iYear)
  )

foo3 <- function(country=NULL, 
                 flavor = c("unadjusted", "adjusted"), 
                 include.energy = c(FALSE, TRUE),
                 formulas = CDformulas){
  
  country <- numeric_choices(country, names(formulas))
  
  if (length(country) < 1) stop("No valid countries specified.")
  
  res <- list()  
  for (c in names(country)) { 
    for (f in flavor) {
      for (ie in include.energy) {
        frmla <- formulas[[c]][[f]]
        if (!ie) frmla <- keep_rhs(frmla, -4)
        res[[paste(c,f, if (ie) "energy" else "noenergy", sep=":")]] <- frmla
      }
    }
  }
  res
}

foo3(country=1, include.energy=FALSE, flavor="adjusted")

foo3()


  