
## Disagreement and Agreement: Part 1

##lets assign the test values for a and b
a = c(0.1,0.4,0.6,1)
b = c(0.15,0.6,0.9,1)

##variable assignment

x_0 = -0.8
y_0 = 0.6


t_vals=seq(0,20,by=1)
x_vals <- matrix(NA, nrow=length(t_vals), ncol=4)
x_vals[1,]<-x_0

y_vals <- matrix(NA, nrow=length(t_vals), ncol=4)
y_vals[1,]<-y_0


##disagreement
d_0 = x_0 - y_0
d_vals <- matrix(NA, nrow=length(t_vals), ncol=4)
d_vals[1,] = d_0


for(i in 1:4) {
  for(t in 1:(length(t_vals)-1)) {
    d_vals[t+1,i] = (1-a[i]-b[i])*d_vals[t,i]
  }
}


##Plotting Disagreement
plot(t_vals, d_vals[,1], type="o",
     col="goldenrod3",
     ylim=c(-1.5,1.5),
     xlab="Time",
     ylab="Disagreement",
     main="Disagreement Over Time")


lines(t_vals, d_vals[,2], type="o", col="red4")
lines(t_vals, d_vals[,3], type="o", col="royalblue")
lines(t_vals, d_vals[,4], type="o", col="darkgreen")

abline(h=0, lty=2)

legend("right",
       legend=c("Case A", "Case B", "Case C", "Case D"),
       col=c("goldenrod3", "red4", "royalblue", "darkgreen"),
       lty=1)

##Now let's Calculate the opinions for each time step
for(i in 1:4) {
  for(t in 1:(length(t_vals)-1)) {
    x_vals[t+1,i] = (1-a[i])*x_vals[t,i]+a[i]*y_vals[t,i]
    y_vals[t+1,i] = (1-b[i])*y_vals[t,i]+b[i]*x_vals[t,i]
  }
}
##Case A
plot(t_vals, x_vals[,1], type="o",
     col="lightseagreen",
     ylim=c(-1,1),
     xlab="Time",
     ylab="Opinion",
     main="Case A")

lines(t_vals, y_vals[,1], type="o", col="palevioletred")

legend("right",
       legend=c("Person 1", "Person 2"),
       col=c("lightseagreen", "palevioletred"),
       lty=1)
##
##Case B
plot(t_vals, x_vals[,2], type="o",
     col="grey35",
     ylim=c(-1,1),
     xlab="Time",
     ylab="Opinion",
     main="Case B")

lines(t_vals, y_vals[,2], type="o", col="red")

legend("right",
       legend=c("Person 1", "Person 2"),
       col=c("grey35", "red"),
       lty=1)

##Case C
plot(t_vals, x_vals[,3], type="o",
     col="royalblue",
     ylim=c(-1,1),
     xlab="Time",
     ylab="Opinion",
     main="Case C")

lines(t_vals, y_vals[,3], type="o", col="indianred")

legend("right",
       legend=c("Person 1", "Person 2"),
       col=c("royalblue", "indianred"),
       lty=1)


##Case D
plot(t_vals, x_vals[,4], type="o",
     col="goldenrod3",
     ylim=c(-1,1),
     xlab="Time",
     ylab="Opinion",
     main="Case D")

lines(t_vals, y_vals[,4], type="o", col="darkgreen")

legend("right",
       legend=c("Person 1", "Person 2"),
       col=c("goldenrod3", "darkgreen"),
       lty=1)

