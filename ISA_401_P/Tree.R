# Classification Tree with rpart
library(rpart)

rb_data <- read.csv("RB_College")

award <- sapply(as.character(rb_data$Year), function(x){
    if(substr(x,0,1) == "*"){
        return(1)
      }else{
        return(0)
      }
})

tbl <- 




rb_data <- cbind.data.frame(rb_data, award)

# grow tree 
fit <- rpart(DrAV ~  award,
             method="class", data=rb_data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Kyphosis")