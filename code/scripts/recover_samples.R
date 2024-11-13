setwd("../../generative_priors/samples/")

recover_output <- list()
for(archive in dir()){
  print(archive)
  load(archive)
  recover_output <- rbind(recover_output,output)
  }

