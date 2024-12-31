#setwd("../trueValues_narrow/samples/")

getwd()
recover_output <- list()
for(archive in dir()){
  print(archive)
  load(archive)
  recover_output <- rbind(recover_output,output)
}

output <- recover_output