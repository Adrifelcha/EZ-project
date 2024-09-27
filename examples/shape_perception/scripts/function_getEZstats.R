######################################################################
# A custom function to compute EZ summary statistics
######################################################################

ez_summaries <- function(data){
  # Identify condition and subject ID
  cond <- unique(data$cond)
  sub <- unique(data$sub)
  # Prepare EZ statistics
  output<- c()
  for(i in sort(sub)){
    # For each subject...
    for(k in sort(cond)){
      # On each condition...
      # Isolate the data
      subset <- data[which(data$sub==i&data$cond==k),]
      # And prepare summary statistics
      output <- rbind(output, c("sub" = unique(subset$sub), "cond" = unique(subset$cond),
                                "change" = unique(subset$change), 
                                "change_quality" = unique(subset$change_quality),
                                "change_type" = unique(subset$change_type),
                                "nTrials" = nrow(subset), "score" = sum(subset$response),
                                "meanRT"  = mean(subset$rt), "varRT"= var(subset$rt)))
    }
  }
  # Return data frame with summary statistics per condition and subject
  return(as.data.frame(output))
}