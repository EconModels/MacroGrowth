# some formula utilities

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

keep_rhsterm <- function(formula, n, env=parent.frame()) {
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
keep_rhsterm(f, -3)
keep_rhsterm(f, 2:4)
keep_rhsterm(f, -(2:4))



  