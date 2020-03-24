###############################################################
###############################################################
###############################################################

# Create a lag variable
# the data is lag within person and within days

lag.Y = function(data){ 

Y_lag = rep(0,nrow(data))
subjno.i = unique(data$subjno)
for (i in subjno.i){
n.i = which(data$subjno==i)
Day.i = data$Day[n.i]
for (t in unique(Day.i)){
k.i = n.i[which(data$Day[n.i]==t)]
Y_lag[k.i] = shift(data$Y[k.i],1)
}}

return(Y_lag)
}
