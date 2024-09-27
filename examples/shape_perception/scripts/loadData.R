# Read the data file
data_raw <- read.csv("https://raw.githubusercontent.com/Adrifelcha/EZ-project/main/examples/shape_perception/data/vpw08.csv")

# Change column names
colnames(data_raw) <- c("sub", "change_quality", "change_type", "noChange", "response", "rt")


# Clean up data
# Remove RTs larger than 3 seconds
tmp <- data_raw[which(data_raw$rt<=3),]
# Identify each condition
change <- 1-tmp$noChange
cond <- rep(0,nrow(tmp))
cond[which(tmp$change_quality==0&tmp$change_type==0)] <- 1
cond[which(tmp$change_quality==1&tmp$change_type==0)] <- 2
cond[which(tmp$change_quality==0&tmp$change_type==1)] <- 3
cond[which(tmp$change_quality==1&tmp$change_type==1)] <- 4
cond[which(change==0)] <- 5

# Prepare final data set to use for our analysis
data <- data.frame("sub" = tmp$sub, "cond" = cond, "change" = change, 
                   "change_quality" = tmp$change_quality, "change_type" = tmp$change_type,
                   "response" = tmp$response, "rt" = tmp$rt)