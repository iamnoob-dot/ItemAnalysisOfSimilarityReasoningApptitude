# Load data
data <- read.csv("data.csv")
data$Total <- apply(subset(data, select = Q01:Q10), 1, sum)
data <- subset(data, select = -c(ID))
l <- nrow(data)
if !system.file(package = 'DescTools'){
    install.packages('DescTools') # gives us the Mode Function
}
library(DescTools)
# Summary stats
print("Summary statistics:")
print(summary(data))
print("Mode:")
print(sapply(data, Mode))
print("Standard Deviation:")
print(sapply(data, sd)) # gives Item Variability

# Kelly's Dichotomization, DV, DI
sorted <- subset(
data[order(data$Total, decreasing = TRUE), ],
select = Q01:Q10
)
top <- head(sorted, round(l*27/100))
bottom <- tail(sorted, round(l*27/100))

dv = (sapply(top, sum) + sapply(bottom, sum)) / 2*round(27*l/100)
print("DV: ")
print(dv)
di = (sapply(top, sum) - sapply(bottom, sum)) / round(27*l/100)
print("DI: ")
print(di)


# Item-Total Correlation
it_cr = sapply(subset(data, select = Q01:Q10), function(v) {
return(cor(v, data$Total))})
print("Item-Total Correlation: ")
print(it_cr)
