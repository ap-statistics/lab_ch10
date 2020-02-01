#Label the subjects from 1 to 3299.
#Subjects who had a stroke are labeled from 1 to 363.
#Subjects who did not have a stroke are labeled from 364 to 3299.
#In each of the 10,000 simulations, use a random generator to output 1649 unique integers from 1 to 3299.
#The subjects whose labels correspond with the outputted integers are assigned to aspirin alone group (g1)
g1 <- replicate(10000, sample(1:3299, size=1649, replace = FALSE))

#Sum up the selected subjects who had a stroke (labels <=363) in each simulation.
stroke <- ifelse(g1<=363, 1,0)
X1 <- apply(stroke,2,sum)
#stroke: 1649 x 10,000 matrix
## Each column records a different set of 1649 subjects assigned to the aspirin alone group with 1 for those who had a stroke and 0 for those who did not have a stroke.
## Adding the 1s and 0s in each column gives us X1 = the number of subjects in the aspirin alone group who had a stroke.
## That is why the 2nd argument in the apply() function is 2, which represents column-wise adddition.

#X1: vector of 10,000 sums, each corresponding to the number of subjects in the aspirin alone group who had a stroke 

#################################################################################################
#Approximate randomization distribution of difference in sample proportions (dp): phat_1 - phat_2

dp <- (X1)/1649 - (363-X1)/(1650)

#Plot histogram of the 10,000 simulated dp
hist(dp, prob=TRUE)

#p-hat-c: pooled proportion (X_1 + X_2)/(n_1 + n_2)
pc <- (206+157)/(1649+1650)
#standard error of the sampling distribution of dp under the assumption of the null hypothesis
sedp <- sqrt(pc*(1-pc)*(1/1649+1/1650))

#superimpose the theoretical sampling distribution of dp
v <- seq(-0.4,0.4,length=2000)
lines(v,dnorm(v,mean=0, sd=sedp))

################################################################################################
#observed dp in actual experiment
obs <- 206/1649 - 157/1650

#estimated p-value using simulated results
sum(dp>=obs)/10000
