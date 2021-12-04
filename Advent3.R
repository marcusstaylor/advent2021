### DAY 3a
library(stringi)
setwd("C:/Users/Marcus/OneDrive/Desktop")
input = read.csv("input3.csv", header=FALSE)
input$V1 = as.character(input$V1)
input$V1 = stri_pad_left(input$V1, 12, "0")


Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df = data.frame()
for (i in 1:nrow(input)) {
  data = t(unlist(strsplit(input$V1[i], "")))
  row = matrix(data=0, nrow=1, ncol=12)
  
  for (j in 1:12) {
    row[1,j] = data[1,j]
  }
  
  df = rbind(df, row)
}

gamma = ""
epsilon = ""
for (i in 1:ncol(answer)) {
  #answer[1,i] = Mode(df[,i])
  common = Mode(df[,i])
  gamma = paste(gamma, common, sep="")
  if (common == "0") {
    epsilon = paste(epsilon, "1", sep="")
  } else {
    epsilon = paste(epsilon, "0", sep="")
  }
}

strtoi(gamma, 2) * strtoi(epsilon, 2)

### DAY 3b
oxy = df
for (i in 1:ncol(oxy)) {
  if (nrow(oxy) == 1) {
    break;
  }
  mode = data.frame(table(oxy[,i]))
  if (nrow(mode) == 1) {
    oxy = subset(oxy, oxy[,i] == mode$Var1[1])
  } else {
    if (mode$Freq[2] >= mode$Freq[1]) {
      oxy = subset(oxy, oxy[,i] == 1)
    } else {
      oxy = subset(oxy, oxy[,i] == 0)
    }
  }
}

c02 = df
for (i in 1:ncol(c02)) {
  if (nrow(c02) == 1) {
    break;
  }
  mode = data.frame(table(c02[,i]))
  if (nrow(mode) == 1) {
    c02 = subset(c02, c02[,i] == mode$Var1[1])
  } else {
    if (mode$Freq[1] <= mode$Freq[2]) {
      c02 = subset(c02, c02[,i] == 0)
    } else {
      c02 = subset(c02, c02[,i] == 1)
    }
  }
}

oxyrate = ""
c02rate = ""
for (i in 1:ncol(c02)) {
  oxyrate = paste(oxyrate, oxy[,i], sep="")
  c02rate = paste(c02rate, c02[,i], sep="")
}

strtoi(oxyrate, 2) * strtoi(c02rate, 2)