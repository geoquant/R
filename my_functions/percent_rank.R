# Function to calculate percentrank with a certain interval "gap"
# Input: "scoreVec" is a vector of scores
#        "gap" is any speceficied interval 
# output: vector of percentrank
percentrank <- function(scoreVec, gap)
{
  scoreVec <- as.vector(scoreVec)
  prank <- NULL
  
  for(i in 1:length(scoreVec))
  {
    if(gap < length(scoreVec))
    {
      if(i <= (length(scoreVec) - gap))
      {
        testScore <- scoreVec[i:(i + gap - 1)]
        temp_data <- data.frame(testScore, score.rank = rank(testScore, ties.method = "min"))
        score <- unique(temp_data[temp_data$testScore == scoreVec[i], "score.rank"])
        prank[i] <- as.integer((score - 1) / (gap - 1) * 1000) / 1000
        rm(temp_data) ; rm(score)
      }
      if(i > (length(scoreVec) - gap))
      {
        testScore <- scoreVec[i:length(scoreVec)]
        temp_data <- data.frame(testScore, score.rank = rank(testScore, ties.method = "min"))
        score <- unique(temp_data[temp_data$testScore == scoreVec[i], "score.rank"])
        prank[i] <- as.integer((score - 1) / (gap - 1) * 1000) / 1000
        rm(temp_data) ; rm(score)
      }
    }
    if(gap == length(scoreVec))
    {
      temp_data <- data.frame(scoreVec, score.rank = rank(scoreVec, ties.method = "min"))
      score <- unique(temp_data[temp_data$scoreVec == scoreVec[i], "score.rank"])
      prank[i] <- as.integer((score - 1) / (gap - 1) * 1000) / 1000
      rm(temp_data) ; rm(score)
    }
  }
  prank
}