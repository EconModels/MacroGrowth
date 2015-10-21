
#' @export
inputs <- function(object, ...) {
  UseMethod("inputs")
}

#' @export
inputs.cesModel <- function(object, ...) {
  head(tail(all.vars(object$formula), -1), -1)
}


#' @export
fortify.cesModel <- function(model, data = NULL, ...) {
  if (is.null(data)) data <- model$data
  
  bind_cols(
    data,
    output_elasticity(model) %>% 
      select( starts_with("oe"), starts_with("alph"), starts_with("x"))
    )
}

#' @export
output_elasticity <- function(object, ...) {
  UseMethod("output_elasticity")
}

#' @export
output_elasticity.cesModel <- function(object, ...) {
 
  cf <- naturalCoef(object)
 
  inputVars <- inputs(object)
  # adjust for nest, if we have one recorded:
  if (!is.null(object$nest)) {
    inputVars <- inputVars[object$nest] 
  }
  
  output_elasticity(
    delta = cf$delta, 
    delta1 = cf$delta_1, 
    alpha1 = cf$alpha_1,
    alpha2 = cf$alpha_2,
    alpha3 = cf$alpha_3, 
    rho = cf$rho,
    rho1 = cf$rho_1,
    x1 = object$data[[inputVars[1]]],
    x2 = object$data[[inputVars[2]]],
    x3 = object$data[[inputVars[3]]]
  )
}

#' @export
output_elasticity.default <- function( 
  object,
  delta, delta1, 
  alpha1 = delta * delta1,
  alpha2 = delta * (1 - delta1),
  alpha3 = (1 - delta),
  rho,
  rho1,
  x1,
  x2,
  x3,
  ...) {
 
  product <- function( x, y ) {
    ifelse( ((x == 0) & is.na(y)) | ((y == 0) & is.na(x)), 0, x * y )
  }
 
  delta <- 1 - alpha3
  delta1 <- ifelse( delta == 0,  NA, alpha1 / delta )
  Q <- delta1 * x1^(-rho1) + (1 - delta1) * x2^(-rho1)
  R <- Q^(rho/rho1 - 1)
  
  # compute "shares"
  
  s1 <- ifelse(is.finite(R), alpha1 * x1 ^ (-rho1) *R, alpha1 * x1^(-rho1))
  s2 <- ifelse(is.finite(R), alpha2 * x2 ^ (-rho1) *R, alpha2 * x2^(-rho1))
  s3 <- ifelse(is.finite(R), alpha3 * x3 ^ (-rho), 0)
  
  oe1 <- s1 / (s1 + s2 + s3)
  oe2 <- s2 / (s1 + s2 + s3)
  oe3 <- s3 / (s1 + s2 + s3)
 
  A <- product(delta1, x1^(-rho1)) + product(1-delta1, x2^(-rho1))
 
  data_frame(
    delta = delta,
    delta1 = delta1,
#    alpha1 = alpha1,
#    alpha2 = alpha2,
#    alpha3 = alpha3,
    s1 = s1, 
    s2 = s2,
    s3 = s3,
#    e = rho/rho1 - 1,
    oe1 = oe1,
    oe2 = oe2,
    oe3 = oe3,
#    x1 = x1, x2 = x2, x3 = x3,
    T = s1 + s2 + s3,
    S = oe1 + oe2 + oe3,
    Q = Q,
    R = R)
}
