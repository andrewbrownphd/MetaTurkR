MTConfirm <- function(options = c("Y","N","ALL"),
                      prompt = "Continue? (Y=continue; N=stop)",
                      cont = "",
                      confirm = TRUE){
  #if confirmation is turned off, exit MTConfirm
  if(!confirm) return(invisible())

  while(!cont %in% options){
    cont <- readline(prompt)
  }
  if(cont == options[1]) return(invisible(""))
  if(cont == options[2]) return(stop("Execution terminated"))
  if(cont == options[3]) return(invisible("ALL"))
}
# EXAMPLE
# cont <- ""
# for(i in 1:10){
#   print(i)
#   cont <- MTConfirm(cont = cont)
# }
