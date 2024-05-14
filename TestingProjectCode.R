library(devtools)
install_github("Sirlukel/RandomTestPapersV2")
library(randomtestpapersV2)

#Testing Code

small_data <- gen_student_data(gen_stand_ran_test(100, "small_test", F, dupe = 1), 100, ran_type = 'n')
type1_results <- estimate(small_data, estimate_type = 2, group = F)
plot_results(type1_results, small_data)
check_ests(type1_results, small_data)


three_data <- gen_student_data(gen_stand_ran_test(99, "small_test", F, dupe = 3), 100, ran_type = 'n')
three_results <- estimate(three_data, estimate_type = 1, group = T)
plot_results(three_results, three_data)
check_ests(three_results, three_data)


big_data <- gen_student_data(gen_stand_ran_test(40, "Bigger Test", F, dupe = 20), 100, ran_type = 'n')
big_results <- estimate(big_data, estimate_type = 1, group = T)
plot_results(big_results, big_data)
check_ests(big_results, big_data)


#Comparing the log-likelihood of our true values against the log-likelihood of our estimates
all_students_likelihood(small_data[seq(1,5000, by = 50),2], small_data[1,5], small_data[1,4], small_data[1,6], small_data[,7])
all_students_likelihood(type1_results$abils, type1_results$discs, type1_results$diffs, small_data[1,6], small_data[,7])

all_students_likelihood(three_data[seq(1,9900, by = 99),2], three_data[1:3,5], three_data[1:3,4], three_data[1:3,6], three_data[,7])
all_students_likelihood(three_results$abils, three_results$discs, three_results$diffs, three_data[1:3,6], three_data[,7])

all_students_likelihood(big_data[seq(1,4000, by = 40),2], big_data[1:20,5], big_data[1:20,4], big_data[1:20,6], big_data[,7])
all_students_likelihood(big_results$abils, big_results$discs, big_results$diffs, big_data[1:20,6], big_data[,7])



#Testing different replications.
MSEs <- c()
for (j in 1:50)
{
  set.seed(6)
  temp_data <- gen_student_data(gen_stand_ran_test(j, "small_test", F, dupe = 1), 100, ran_type = 'n')
  temp_results <- estimate(temp_data, replication = 1, estimate_type = 2, group = F)
  MSEs <- c(MSEs, check_ests(temp_results, temp_data, replication = 1)$MSE)
}
par(mfrow = c(1,1))
plot(1:50, MSEs, type = 'l', ylim = c(0,20), xlab = "Number of replications of the question", ylab = "Mean Squared Error")


#Extra Plots for report
est_abils <- find_optimised_abilities(small_data[1,5], small_data[1,4], small_data[1,6], small_data[,7], ns = 100, nq = 1)
plot(small_data[seq(1, 2500, 25),2], est_abils$abils, xlab = "True Abilities", ylab = "Estimated Abilites")
abline(a = 0, b = 1, col = "red")
est_MSE(big_data[seq(1, 1000, 20),2], est_abils$abils)
