rm(list=ls())
set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))
out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
}
quantile(out, c(0.05, 0.95))

# OR 95% confidence interval can also be derived as
mean(x)-(1.96*se_x)
mean(x)+(1.96*se_x)
