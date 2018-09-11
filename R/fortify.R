#' fortify LINEX models
#'
#' @param model a LINEX model
#' @param data a data frame.  Typically this is not needed
#' since the data are extracted from `model`.  But this allows
#' a drop in replacement data set, if desired.
#' @param ... additional arguments (not currently used)
#' @keywords internal
#' @export
#'
fortify.LINEXmodel <- function(model, data, ...) {
  if(missing(data)) { data <- getData(model) }
  formula <- model$formula
  data <- model.frame(formula, data)
  if (ncol(data) != 5) {
    stop(paste0('Expected 5 variables; I see ', ncol(data)))
  }
  l <- data[,2]
  k <- data[,3]
  e <- data[,4]
  a_0 <- naturalCoef(model)[, "a_0", drop=TRUE]
  c_t <- naturalCoef(model)[, "c_t", drop=TRUE]
  mutate( data,
    # These equations are from Equation 9 in
    # B.S. Warr and R.U. Ayres. Useful work and information as drivers of economic growth.
    # Ecological Economics, 73(C):93–102, Jan. 2012.
    alpha = a_0 * (l+e)/k,
    beta = a_0 * (c_t*l/e - l/k), # or beta = a_1*l/e - a_0*l/k
    gamma = 1 - alpha - beta
  )
}