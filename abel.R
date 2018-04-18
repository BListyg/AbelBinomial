abel <- function(m,w,z){

k <- 0

out <- vector()

for(i in k:m){
out[i+1] <- sum(choose(m,i)*((w+m-i)^(m-i-1))*((z+i)^i))
}

return(sum(out))

}

w <- 3
z <- 4
k <- 0
m <- 2

isTRUE(
  (((z + w + 2)^2) / w) == abel(m = 2, w = 3, z = 4)
)
