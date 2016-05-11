MTHourlyRate <- function(results,reward)
{
  hourlyRate <- (3600/mean(results$SecondsOnHIT[which(results$AssignmentStatus != "Rejected")]))*reward
  print(paste0("The average Hourly Rate for non-rejected HITs is $",round(hourlyRate,2)))
  return(hourlyRate)
}
