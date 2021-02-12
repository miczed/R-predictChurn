library(data.table)

test_that("customer with highest prob is always higher than customer with lowest", {
  customer <- data.table::fread("data/data_customer.csv")
  personal <- data.table::fread("data/data_personal.csv")

  all <- merge(customer, personal ,by="CustomerId", all=TRUE)
  all[,Exited := as.factor(Exited)]
  all[,Gender := as.factor(Gender)]

  highest_churn <- predictChurn(all,"15653251")
  lowest_churn <- predictChurn(all,"15662641")
  expect_true(highest_churn > lowest_churn)
})
