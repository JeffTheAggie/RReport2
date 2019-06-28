#1a
SIM_HW = c("Not_HW","HW")
set.seed(10)
a = c(sample(SIM_HW,80,replace = TRUE,prob = c(0.15,0.85)))
NO_HW = length(a[a=="Not_HW"])
NO_HW


#1b
simulate_HW = function(x = 0){
  SIM_HW = c("Not_HW","HW")
  a = c(sample(SIM_HW,80,replace = TRUE,prob = c(0.15,0.85)))
  NO_HW = length(a[a=="Not_HW"])
  NO_HW
}
many_HW = sapply(1:1000000,simulate_HW)
many_HW_hist = hist(many_HW, main = "Students that didn't do their homework from 1000000 trials", xlab = "Number of Students that didn't do their Homework", ylab = "Number of trials")

#1c
many_HW_mean = mean(many_HW)
many_HW_sd = sd(many_HW)

#1d
all_HW = length(many_HW[many_HW == 0])
all_prob = all_HW/1000000

#1e
four_atleast_no_HW = length(many_HW[many_HW >= 4])
four_prob = four_atleast_no_HW/1000000

#1f
many_HW_median = median(many_HW)
#2a
lynx = as.numeric(lynx)
lynx_hist = hist(lynx, main = "Annual Canadian Lynx trappings 1821-1934", xlab = "The Number of Lynx that were Trapped", ylab = "The number of times that the Lynx were Trapped")
lynx_mean = mean(lynx)
lynx_var = var(lynx)

#2b
lynx_sample = sample(lynx,10,replace = TRUE)
lynx_sample_mean = mean(lynx_sample)

#2c
many_lynx_10_mean = mean(replicate(1000000,mean(sample(lynx,10,replace = TRUE))))
many_lynx_10_sd = sd(replicate(1000000,mean(sample(lynx,10,replace = TRUE))))
many_lynx_10_hist = hist(replicate(1000000,mean(sample(lynx,10,replace = TRUE))),main = "Lynx Trappings with a Sample Size of 10", xlab = "Number of Lynx Trapped", ylab = "Number of times the lynx were Trapped")

#2d
lynx_sample = sample(lynx,50,replace = TRUE)
lynx_sample_mean = mean(lynx_sample)

#2e
many_lynx_50_mean = mean(replicate(1000000,mean(sample(lynx,50,replace = TRUE))))
many_lynx_50_sd = sd(replicate(1000000,mean(sample(lynx,50,replace = TRUE))))

#2f
many_lynx_50_hist = hist(replicate(1000000,mean(sample(lynx,50,replace = TRUE))), main = "Lynx Trappings with a Sample Size of 50", xlab = "Number of Lynx Trapped", ylab = "Number of times the lynx were Trapped")