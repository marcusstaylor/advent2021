setwd("C:/Users/Marcus/OneDrive/Desktop/Advent")

### DAY 4.1
filled = boards
for (i in 1:ncol(nums)) {
  filled[filled==nums[1,i]] = -1
  for (j in seq(1, nrow(boards), by=5)) {
    curr = filled[j:(j+4),]
    for (k in 1:nrow(curr)) {
      x = data.frame(table(curr[k,] == -1))
      y = data.frame(table(curr[,k] == -1))
      if (("TRUE" %in% x$Var1 & x$Freq[1] == 5) | ("TRUE" %in% y$Var1 & y$Freq[1] == 5)) {
        print(j)
        curr[curr==-1]=0
        print(sum(curr)*nums[1,i])
        stop();
      }
    }
  }
}

### DAY 4.2

filled = boards
count=0
for (i in 1:ncol(nums)) {
  filled[filled==nums[1,i]] = -1
  for (j in seq(1, nrow(boards), by=5)) {
    curr = filled[j:(j+4),]
    for (k in 1:nrow(curr)) {
      x = data.frame(table(curr[k,] == -1))
      y = data.frame(table(curr[,k] == -1))
      if (("TRUE" %in% x$Var1 & x$Freq[1] == 5) | ("TRUE" %in% y$Var1 & y$Freq[1] == 5)) {
        count = count+1
        print(count)
        if (count==100) {
          curr[curr==-1]=0
          print(sum(curr)*nums[1,i])
          stop();
        }
        filled = filled[-(j:(j+4)),]
        break;
      }
    }
  }
}