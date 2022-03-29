total = 0
n = 0
patients = 1000
for (n in 0:patients)
{
  l <- 0
  w = 1
  while (w == 1) {
    l = l+1
    w = rbinom(1, size=1, prob=0.2)
  }
total = total + l
}
mean = total/patients
print(mean)


