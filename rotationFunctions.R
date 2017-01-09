QuaternionRotationMatrix <- function(q){
  r11 <- q[1]^2 + q[2]^2 - q[3]^2 - q[4]^2
  r12 <- 2 * q[2] * q[3] + 2 * q[1] * q[4]
  r13 <- 2 * q[2] * q[4] - 2 * q[1] * q[3]
  r21 <- 2 * q[2] * q[3] - 2 * q[1] * q[4]
  r22 <- q[1]^2 - q[2]^2 + q[3]^2 - q[4]^2
  r23 <- 2 * q[3] * q[4] + 2 * q[1] * q[2]
  r31 <- 2 * q[2] * q[4] + 2 * q[1] * q[3]
  r32 <- 2 * q[3] * q[4] - 2 * q[1] * q[2]
  r33 <- q[1]^2 - q[2]^2 - q[3]^2 + q[4]^2
  return(matrix(c(r11, r12, r13,
                  r21, r22, r23,
                  r31, r32, r33), nrow = 3, ncol = 3, byrow = TRUE))
}

RotateAttitude <- function(at, ac){
  n <- nrow(ac)
  gac <- ac
  for(i in seq(n)){
    RM <- QuaternionRotationMatrix(as.numeric(at[i, ]))
    gac[i, ] <- crossprod(RM, as.numeric(ac[i, ]))
  }
  gac
}
