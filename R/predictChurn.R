library(data.table)
predictChurn <- function(data, customerId) {
  model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, data=data, family="binomial")
  customer <- data[data$CustomerId == customerId,]
  # 4.2 throw error if customer is not found
  if(nrow(customer) == 0) {
    stop(sprintf("Error: No customer with id \"%s\" found.",customerId))
  }
  churnProb <- predict(model, customer,type="response")
  return(churnProb)
}
