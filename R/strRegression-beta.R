beta <- function(sigmacapx,
                 sigmayx) {
  stopifnot(
    is.matrix(sigmacapx),
    sigmacapx == t(sigmacapx),
    is.vector(sigmayx)
  )
  return(
    drop(
      solve(
        sigmacapx,
        sigmayx
      )
    )
  )
}
