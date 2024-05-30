# TASK-1
emplData<-data.frame( 
  name = c("Amar","Brijesh","Balveer","Charan","Daksh","Dev","Ekansh","Farhan","Gautam","Girish","Harsh","Jagdish","Karan","Madhav","Ojas"), 
  age = c(23, 31, 19, 22, 24, 26, 28, 19, 24, 26, 19, 20, 22, 24, 27),
  height = c(160, 175, 156, 164, 168, 154, 159, 162, 169, 166, 158, 159, 148, 170, 157),
  weight = c(60, 76, 77, 72, 89, 68, 81, 72, 75, 69, 71, 85, 80, 78, 62),
  stringsAsFactors = FALSE)

print(emplData)

# TASK-2
print(emplData[6,"age"])

# TASK-3
print(emplData$age)

# TASK-4
y<- c(5000, 5500, 4000, 5500, 4800, 6000, 7500, 4000, 4600, 5000, 6000, 7000, 4000, 5000, 6000)
emplData<-cbind(emplData, salary = y) 
print(emplData)

# TASK-5
x<-list("Sachin", 21, 152, 64, 6500) 
emplData<-rbind(emplData, x)
print(emplData)

# TASK-6
a<- as.vector(emplData$salary)
print(quantile(a))

# TASK-7
b<- as.vector(emplData$age)
plot(b, a, main="TASK-7", xlab="Age", ylab="Salary", pch=19, col="red")

# TASK-8
hist(b, main = "TASK-8", col = "lightblue", xlab = "Age")

# TASK-9
model<- lm(a~b,data = emplData)
summary(model)

# TASK-10
plot(b,a, main="TASK-10", xlab="Age", ylab="Salary", pch=19, col="blue")
abline(model, col="black", lty="dashed", lwd=3)