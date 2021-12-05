library(data.table)
data <- read.csv("/Users/mwalte10/Documents/PBF/PHI_512/dfmo.csv")
data <- data.table(data)

#	Mean and SD of starting and 6 month put levels for both groups
# boxplot
notrt_base <- mean(data[dfmo == 0, put0])
notrt_base_CI <- quantile(data[dfmo == 0, put0], c(0.025,0.975))

trt_base <- mean(data[dfmo == 1, put0])
trt_base_CI <- quantile(data[dfmo == 1, put0], c(0.025,0.975))

notrt_six <- mean(data[dfmo == 0, put6])
notrt_six_CI <- quantile(data[dfmo == 0, put6], c(0.025,0.975))

trt_six <- mean(data[dfmo == 1, put6])
trt_six_CI <- quantile(data[dfmo == 1, put6], c(0.025,0.975))

boxplot(put0 ~ dfmo, data = data, main = 'Putrescine levels at baseline', xlab = c('Group'), ylab = 'Putrescine (umol/mg)',
        names = c('Control', 'Treatment'))

#Among people assigned to DFMO, is there evidence that putrescine is lower at 6 months post baseline compared to baseline levels? 
##	T-test between 0 and 6 month groups 
t.test(x = data[dfmo == 1, put0], y = data[dfmo == 1, put6])
dt <- data[dfmo == 1,.(put0, put6)]
dt[,id := c(1:nrow(dt))]
dt <- melt(dt, id.vars = 'id')
boxplot(value ~ variable, data = dt, main = 'Putrescine levels in treatment group at baseline and 6 months', xlab = c('Time'), ylab = 'Putrescine (umol/mg)',
        names = c('Baseline', '6 months'))


# Is there evidence that DFMO is effective at reducing putrescine levels 6 months post baseline? (HINT: this is asking something slightly different than question 2-we will want to also consider the people assigned to the 0 dose group) 
#T-test between change in putrescine between treatment groups
data[,change := put6 - put0]
t.test(x = data[dfmo == 0, change], y = data[dfmo == 1, change])

boxplot(change ~ dfmo, data = data, main = 'Change in putrescine levels from baseline', xlab = c('Group'), ylab = 'Change in putrescine (umol/mg)',
        names = c('Control', 'Treatment'))
abline(h = 0, col = 'red')

library(effectsize)
pooled_notrt = sd_pooled(data[dfmo == 0,put0], data[dfmo == 0, put6])
pooled_trt = sd_pooled(data[dfmo == 1,put0], data[dfmo == 1, put6])

lower_notrt <- mean(data[dfmo ==0, change]) - 1.98 * pooled_notrt * sqrt(2 * (1/32))
upper_notrt <- mean(data[dfmo ==0, change]) + 1.98 * pooled_notrt * sqrt(2 * (1/32))


lower_trt <- mean(data[dfmo ==1, change]) - 1.98 * pooled_trt * sqrt(2 * (1/82))
upper_trt <- mean(data[dfmo ==1, change]) + 1.98 * pooled_trt * sqrt(2 * (1/82))
