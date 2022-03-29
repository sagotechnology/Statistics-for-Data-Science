l <- 0
w <- 1
while (w == 1) {
  l = l+1
  w = rbinom(1, size=1, prob=0.2)
}
print(l)