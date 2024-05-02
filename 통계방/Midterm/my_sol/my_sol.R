p_list = c(3, 50)
rho_list = c(0, 0.7)


my_poi <- function(){
  print("Nonpenalized Regression")
  }

my_ridge <- function(){
  print("Ridge Regressoin")
}





methods = c(my_poi, my_ridge)


#---------------------------#
acc_est <- function(){
  mse = 1
  var = 2
  bias =3
  list(MSE = mse, VAR = var, Bias = bias)
}
perform <- function(){
  cs =1
  is =2
  ac =3
  list(cs =cs, is = is, ac= ac)
}

com_time <- function(){
  time = 10
  list(Time = time)
}
#-----------------------------#


my_compute <- function(rho, p, method){
  acc = acc_est()
  per = perform()
  com = com_time()
  c(acc$mse, acc$var, acc$bias, per$IS, per$ac, com$time)
}


for (rho in rho_list) {
  for (p in p_list){
    for (method in methods) {
      my_compute(rho, p, mehtod)
      
    }
    
  }
    
}
  

