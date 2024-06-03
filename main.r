library(pracma)

plate_size <- 40
# coefficient matrix
m <- matrix(0, nrow = plate_size^2, ncol = plate_size^2)
# constant array
v <- t(array(0, dim = plate_size^2))
# temperature position is different because of R matrix indexing:
# (top to bottom, left to right)
# the rotation is performed to produce an output similar to the task
bottom <- 50
top <- 100
left <- 200
right <- 150

### Start ###

# variable equation index
for (i in 1:plate_size) {
  for (j in 1:plate_size) {
    # matrix indexing
    id_cur <- (i - 1) * plate_size + j
    id_bottom <- i * plate_size + j
    id_top <- (i - 2) * plate_size + j
    id_left <- (i - 1) * plate_size + j - 1
    id_right <- (i - 1) * plate_size + j + 1
    # diagonal
    m[id_cur, id_cur] <- -4
    # top row check
    if (i != 1) {
      m[id_cur, id_top] <- 1
    } else {
      v[id_cur] <- v[id_cur] - top
    }
    # bottom row check
    if (i != plate_size) {
      m[id_cur, id_bottom] <- 1
    } else {
      v[id_cur] <- v[id_cur] - bottom
    }
    # left column check
    if (j != 1) {
      m[id_cur, id_left] <- 1
    } else {
      v[id_cur] <- v[id_cur] - left
    }
    # right column check
    if (j != plate_size) {
      m[id_cur, id_right] <- 1
    } else {
      v[id_cur] <- v[id_cur] - right
    }
  }
}
# inverse of the coefficient matrix
m <- solve(m)
# matrix multiplication
res <- m %*% t(v)
# resizing the 1:plate_size^2 array to a plate_size:plate_size matrix
# this requires the pracma library,
# if it is not installed, run install.packages('pracma') in the R terminal
res <- Reshape(res, plate_size, plate_size)

print(res)
# output the result to a file
write.table(res, file = "output.txt", row.names = FALSE, col.names = FALSE)