show_design = function(settings){
  cat("========== EZBHDDM Design Parameters: =======\n")
  cat("Number of Participants:    ", settings$nPart,"\n")
  cat("Trials Per Person:",settings$nTrials,"\n")
  cat("Priors:\n")
  t(settings$prior)
}