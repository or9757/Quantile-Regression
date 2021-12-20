library(quantreg)
library(dplyr)
library(tidyr)
library(PSG)
library(gridExtra)
library(grid)
data <- read.csv("C:/Users/ryans/Desktop/AMS/518/Project/NY School SAT factors (Pandas)/sat.csv")

data<-data[2:14]
#----------------------------------OUTLIERS-------------------------------------------------
summary(data$sat_score)
par(mfrow=c(1,2))
qqnorm(data$sat_score, main = "Figure 1. QQ plot of Avg.SAT Scores of NYC Schools")
qqline(data$sat_score, col ='red')
hist(data$sat_score, main = "Figure 2. Histogram of Avg.SAT scores of NYC schools")
par(mfrow=c(1,1))

#-----------------------------Coef Table------------------------------------------------------------------
#Num_Score_
Num_score_0.05<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.05))
Num_score_0.10<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.10))
Num_score_0.25<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.25))
Num_score_0.50<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.50))
Num_score_0.75<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.75))
Num_score_0.90<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.90))
Num_score_0.95<-summary(rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = 0.95))

comparison_quantile_beta<-data.frame(cbind(Num_score_0.05$coefficients[2],
Num_score_0.10$coefficients[2],
Num_score_0.25$coefficients[2],
Num_score_0.50$coefficients[2],
Num_score_0.75$coefficients[2],
Num_score_0.90$coefficients[2],
Num_score_0.95$coefficients[2]))
#avg_class_size
avg_class_size_0.05<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.05))
avg_class_size_0.10<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.10))
avg_class_size_0.25<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.25))
avg_class_size_0.50<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.50))
avg_class_size_0.75<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.75))
avg_class_size_0.90<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.90))
avg_class_size_0.95<-summary(rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = 0.95))

avg_class_size <- data.frame(cbind(avg_class_size_0.05$coefficients[2],
                        avg_class_size_0.10$coefficients[2],
                        avg_class_size_0.25$coefficients[2],
                        avg_class_size_0.50$coefficients[2],
                        avg_class_size_0.75$coefficients[2],
                        avg_class_size_0.90$coefficients[2],
                        avg_class_size_0.95$coefficients[2]))
#total_enrollment
total_enrollment_0.05<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.05))
total_enrollment_0.10<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.10))
total_enrollment_0.25<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.25))
total_enrollment_0.50<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.50))
total_enrollment_0.75<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.75))
total_enrollment_0.90<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.90))
total_enrollment_0.95<-summary(rq(sat_score ~ total_enrollment,data = data, tau = 0.95))

total_enrollment <- data.frame(cbind(total_enrollment_0.05$coefficients[2],
                                   total_enrollment_0.10$coefficients[2],
                                   total_enrollment_0.25$coefficients[2],
                                   total_enrollment_0.50$coefficients[2],
                                   total_enrollment_0.75$coefficients[2],
                                   total_enrollment_0.90$coefficients[2],
                                   total_enrollment_0.95$coefficients[2]))
#ell_percent
ell_percent_0.05<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.05))
ell_percent_0.10<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.10))
ell_percent_0.25<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.25))
ell_percent_0.50<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.50))
ell_percent_0.75<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.75))
ell_percent_0.90<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.90))
ell_percent_0.95<-summary(rq(sat_score ~ ell_percent,data = data, tau = 0.95))

ell_percent <- data.frame(cbind(ell_percent_0.05$coefficients[2],
                                     ell_percent_0.10$coefficients[2],
                                     ell_percent_0.25$coefficients[2],
                                     ell_percent_0.50$coefficients[2],
                                     ell_percent_0.75$coefficients[2],
                                     ell_percent_0.90$coefficients[2],
                                     ell_percent_0.95$coefficients[2]))
#asian_per
a_0.05<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ asian_per,data = data, tau = 0.95))

asian_per <- data.frame(cbind(a_0.05$coefficients[2],
                                     a_0.10$coefficients[2],
                                     a_0.25$coefficients[2],
                                     a_0.50$coefficients[2],
                                     a_0.75$coefficients[2],
                                     a_0.90$coefficients[2],
                                     a_0.95$coefficients[2]))
#black_per
a_0.05<-summary(rq(sat_score ~ black_per,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ black_per,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ black_per,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ black_per,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ black_per,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ black_per,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ black_per,data = data, tau = 0.95))

black_per <- data.frame(cbind(a_0.05$coefficients[2],
                              a_0.10$coefficients[2],
                              a_0.25$coefficients[2],
                              a_0.50$coefficients[2],
                              a_0.75$coefficients[2],
                              a_0.90$coefficients[2],
                              a_0.95$coefficients[2]))

#White
a_0.05<-summary(rq(sat_score ~ white_per,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ white_per,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ white_per,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ white_per,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ white_per,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ white_per,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ white_per,data = data, tau = 0.95))

white_per <- data.frame(cbind(a_0.05$coefficients[2],
                              a_0.10$coefficients[2],
                              a_0.25$coefficients[2],
                              a_0.50$coefficients[2],
                              a_0.75$coefficients[2],
                              a_0.90$coefficients[2],
                              a_0.95$coefficients[2]))

#hispanic_per
a_0.05<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ hispanic_per,data = data, tau = 0.95))

hispanic_per <- data.frame(cbind(a_0.05$coefficients[2],
                              a_0.10$coefficients[2],
                              a_0.25$coefficients[2],
                              a_0.50$coefficients[2],
                              a_0.75$coefficients[2],
                              a_0.90$coefficients[2],
                              a_0.95$coefficients[2]))
#male_per
a_0.05<-summary(rq(sat_score ~ male_per,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ male_per,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ male_per,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ male_per,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ male_per,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ male_per,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ male_per,data = data, tau = 0.95))

male_per <- data.frame(cbind(a_0.05$coefficients[2],
                                 a_0.10$coefficients[2],
                                 a_0.25$coefficients[2],
                                 a_0.50$coefficients[2],
                                 a_0.75$coefficients[2],
                                 a_0.90$coefficients[2],
                                 a_0.95$coefficients[2]))


#female_per

a_0.05<-summary(rq(sat_score ~ female_per,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ female_per,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ female_per,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ female_per,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ female_per,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ female_per,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ female_per,data = data, tau = 0.95))

female_per <- data.frame(cbind(a_0.05$coefficients[2],
                             a_0.10$coefficients[2],
                             a_0.25$coefficients[2],
                             a_0.50$coefficients[2],
                             a_0.75$coefficients[2],
                             a_0.90$coefficients[2],
                             a_0.95$coefficients[2]))
#school_dist
a_0.05<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ school_dist,data = data, tau = 0.95))

school_dist <- data.frame(cbind(a_0.05$coefficients[2],
                               a_0.10$coefficients[2],
                               a_0.25$coefficients[2],
                               a_0.50$coefficients[2],
                               a_0.75$coefficients[2],
                               a_0.90$coefficients[2],
                               a_0.95$coefficients[2]))

#saf_s_11
a_0.05<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.05))
a_0.10<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.10))
a_0.25<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.25))
a_0.50<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.50))
a_0.75<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.75))
a_0.90<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.90))
a_0.95<-summary(rq(sat_score ~ saf_s_11,data = data, tau = 0.95))

saf_s_11 <- data.frame(cbind(a_0.05$coefficients[2],
                                a_0.10$coefficients[2],
                                a_0.25$coefficients[2],
                                a_0.50$coefficients[2],
                                a_0.75$coefficients[2],
                                a_0.90$coefficients[2],
                                a_0.95$coefficients[2]))


comparison_quantile_beta <- rbind(comparison_quantile_beta,avg_class_size,total_enrollment,ell_percent,asian_per,
                                  black_per,white_per,hispanic_per, male_per, female_per, school_dist,saf_s_11)
rownames(comparison_quantile_beta) <- c("Number of score 3/4/5","avg_class_size","total_enrollment","ell_percent","asian_per",
                                        "black_per","white_per","hispanic_per", "male_per", "female_per", "school_dist","saf_s_11")
colnames(comparison_quantile_beta) <- c("tau = 0.05",0.10,0.25,0.50,0.75,0.90,0.95)


#-----------------------------PLOTTING------------------------------------------------------------------

ols_num_ex<-lm(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data)
ols_avg_class_size<-lm(sat_score ~ AVERAGE.CLASS.SIZE,data = data)
ols_total_enrollment<-lm(sat_score ~ total_enrollment,data = data)
ols_ell_percent<-lm(sat_score ~ ell_percent,data = data)
ols_asian_per<-lm(sat_score ~ asian_per,data = data)
ols_black_per<-lm(sat_score ~ black_per,data = data)
ols_white_per<-lm(sat_score ~ white_per,data = data)
ols_hispanic_per<-lm(sat_score ~ hispanic_per,data = data)
ols_male_per<-lm(sat_score ~ male_per,data = data)
ols_female_per<-lm(sat_score ~ female_per,data = data)
ols_school_dist<-lm(sat_score ~ school_dist,data = data)
ols_saf_s_11<-lm(sat_score ~ saf_s_11,data = data)

par(mfrow=c(3,4))

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 3] Number.of.Exams.with.scores.3.4.or.5", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_num_ex$coefficient[2],col = 'red')
abline(h = confint(ols_num_ex, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_num_ex, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ AVERAGE.CLASS.SIZE,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 4] Avg. Class Size", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_avg_class_size$coefficient[2],col = 'red')
abline(h = confint(ols_avg_class_size, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_avg_class_size, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ total_enrollment,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 5] Total Enrollment", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_total_enrollment$coefficient[2],col = 'red')
abline(h = confint(ols_total_enrollment, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_total_enrollment, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")


plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ ell_percent,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 6] English Learner Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_ell_percent$coefficient[2],col = 'red')
abline(h = confint(ols_ell_percent, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_ell_percent, level = 0.95)[4], col = 'red', lty = 2)
legend("bottomleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ asian_per,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 7] Asian Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_asian_per$coefficient[2],col = 'red')
abline(h = confint(ols_asian_per, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_asian_per, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ white_per,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 8] White Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_white_per$coefficient[2],col = 'red')
abline(h = confint(ols_white_per, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_white_per, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")


plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ black_per,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 9] Black Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_black_per$coefficient[2],col = 'red')
abline(h = confint(ols_black_per, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_black_per, level = 0.95)[4], col = 'red', lty = 2)
legend("bottomleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")


plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ hispanic_per,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 10] Hispanic Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_hispanic_per$coefficient[2],col = 'red')
abline(h = confint(ols_hispanic_per, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_hispanic_per, level = 0.95)[4], col = 'red', lty = 2)
legend("bottomleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ male_per,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 11] Male Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_male_per$coefficient[2],col = 'red')
abline(h = confint(ols_male_per, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_male_per, level = 0.95)[4], col = 'red', lty = 2)
legend("bottomleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ female_per,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 12] Female Percentage", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_female_per$coefficient[2],col = 'red')
abline(h = confint(ols_female_per, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_female_per, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ school_dist,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 13] School District", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_school_dist$coefficient[2],col = 'red')
abline(h = confint(ols_school_dist, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_school_dist, level = 0.95)[4], col = 'red', lty = 2)
legend("bottomleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

plot(seq(0.05,0.95,by = 0.05),rq(sat_score ~ saf_s_11,data = data, tau = seq(0.05,0.95, by =0.05))$coefficients[2,]
     ,main="[Figure 14] Safety Level", type = 'br', xlab = "tau", ylab = "Coefficients")
abline(h=ols_saf_s_11$coefficient[2],col = 'red')
abline(h = confint(ols_saf_s_11, level = 0.95)[2], col = 'red', lty = 2)
abline(h = confint(ols_saf_s_11, level = 0.95)[4], col = 'red', lty = 2)
legend("topleft", legend =  c("mean value", "Confidence Interval"), lty = 1:2, col = "red")

par(mfrow=c(1,1))



#-----------------------TEST------------------------------------------------------------------
q_5_Num_ex<-rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5, tau = 0.05, data = data)
q_95_Num_ex<-rq(sat_score ~ Number.of.Exams.with.scores.3.4.or.5, tau = 0.95, data = data)
anova(q_5_Num_ex,q_95_Num_ex)

q_5_avg_class_size<-rq(sat_score ~ AVERAGE.CLASS.SIZE, tau = 0.05, data = data)
q_95_avg_class_size<-rq(sat_score ~ AVERAGE.CLASS.SIZE, tau = 0.95, data = data)
anova(q_5_avg_class_size,q_95_avg_class_size)

q_5_total_enrollment<-rq(sat_score ~ total_enrollment, tau = 0.05, data = data)
q_95_total_enrollment<-rq(sat_score ~ total_enrollment, tau = 0.95, data = data)
anova(q_5_total_enrollment,q_95_total_enrollment)


q_5_ell_percent<-rq(sat_score ~ ell_percent, tau = 0.05, data = data)
q_95_ell_percent<-rq(sat_score ~ ell_percent, tau = 0.95, data = data)
anova(q_5_ell_percent,q_95_ell_percent)

q_10_ell_percent<-rq(sat_score ~ ell_percent, tau = 0.10, data = data)
q_90_ell_percent<-rq(sat_score ~ ell_percent, tau = 0.90, data = data)
anova(q_10_ell_percent,q_90_ell_percent)



q_5_asian_per<-rq(sat_score ~ asian_per, tau = 0.05, data = data)
q_95_asian_per<-rq(sat_score ~ asian_per, tau = 0.95, data = data)
anova(q_5_asian_per,q_95_asian_per)

q_5_black_per<-rq(sat_score ~ black_per, tau = 0.05, data = data)
q_95_black_per<-rq(sat_score ~ black_per, tau = 0.95, data = data)
anova(q_5_black_per,q_95_black_per)

q_5_white_per<-rq(sat_score ~ white_per, tau = 0.05, data = data)
q_95_white_per<-rq(sat_score ~ white_per, tau = 0.95, data = data)
anova(q_5_white_per,q_95_white_per)

q_5_hispanic_per<-rq(sat_score ~ hispanic_per, tau = 0.05, data = data)
q_95_hispanic_per<-rq(sat_score ~ hispanic_per, tau = 0.95, data = data)
anova(q_5_hispanic_per,q_95_hispanic_per)

q_5_male_per<-rq(sat_score ~ male_per, tau = 0.05, data = data)
q_95_male_per<-rq(sat_score ~ male_per, tau = 0.95, data = data)
anova(q_5_male_per,q_95_male_per)

q_5_female_per<-rq(sat_score ~ female_per, tau = 0.05, data = data)
q_95_female_per<-rq(sat_score ~ female_per, tau = 0.95, data = data)
anova(q_5_female_per,q_95_female_per)

q_5_school_dist<-rq(sat_score ~ school_dist, tau = 0.05, data = data)
q_95_school_dist<-rq(sat_score ~ school_dist, tau = 0.95, data = data)
anova(q_5_school_dist,q_95_school_dist)

q_5_saf_s_11<-rq(sat_score ~ saf_s_11, tau = 0.05, data = data)
q_95_saf_s_11<-rq(sat_score ~ saf_s_11, tau = 0.95, data = data)
anova(q_5_saf_s_11,q_95_saf_s_11)

#---------------------------------------------------------------------------------------------------
#PSG PART

design_matrix <- read.csv("C:/Users/ryans/Desktop/AMS/518/design_matrix.csv")
design_matrix <- design_matrix[1:363,]
design_matrix[,3] <- data$total_enrollment
design_matrix[,2] <- data$sat_score
design_matrix = subset(design_matrix, select = -c(intercept))
colnames(design_matrix)[2] <- "total_enrollment_at_90th"

problem.list_Decompose <- list()
problem.list_Decompose$matrix_style_classification_fidelity_magellan_wi <- as.matrix(design_matrix)
problem.list_Decompose$problem_statement <- sprintf(
  "minimize                                                       
 cvar_dev(0.50, matrix_style_classification_fidelity_magellan_wi)
value:      
  var_risk(0.50, matrix_style_classification_fidelity_magellan_wi)
  ")
result_Decompose <- rpsg_solver(problem.list_Decompose)

#Coefficient for total_enrollment
result_Decompose$point_problem_1

a<-cbind(result_Decompose$point_problem_1,comparison_quantile_beta[3,4])
colnames(a) <- c("PSG", "quantreg")

a

problem.list_Decompose <- list()
problem.list_Decompose$matrix_style_classification_fidelity_magellan_wi <- as.matrix(design_matrix)
problem.list_Decompose$problem_statement <- sprintf(
  "minimize                                                       
 cvar_dev(0.90, matrix_style_classification_fidelity_magellan_wi)
value:      
  var_risk(0.90, matrix_style_classification_fidelity_magellan_wi)
  ")
result_Decompose <- rpsg_solver(problem.list_Decompose)

#Coefficient for total_enrollment
result_Decompose$point_problem_1

cbind(result_Decompose$point_problem_1,comparison_quantile_beta[3,6])

a<-cbind(result_Decompose$point_problem_1,comparison_quantile_beta[3,6])
colnames(a) <- c("PSG", "quantreg")

a
