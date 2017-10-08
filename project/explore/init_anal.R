## 
employee_info_path <- "/home/alex/data_analytics/DataSets1_9182017/Employees_info/"


# list_employ_info_files <- list.files(path = employee_info_path,
#                 include.dirs = FALSE)

list_employ_info_files <- c("2009-12",
                            "2010-Jan",
                            "2010-Feb",
                            "2010-Mar",
                            "2010-Apr",
                            "2010-May",
                            "2010-Jun",
                            "2010-Jul",
                            "2010-Aug",
                            "2010-Sep",
                            "2010-Oct",
                            "2010-Nov",
                            "2010-Dec",
                            "2011-Jan",
                            "2011-Feb",
                            "2011-Mar",
                            "2011-Apr",
                            "2011-May")

# just put year-mon.csv into a list
for (i1 in 1:length(list_employ_info_files)) {
  list_employ_info_files[i1] <- paste(list_employ_info_files[i1],".csv",sep="")
}


## get employee info for first month
employee_data <- read.table(paste(employee_info_path,list_employ_info_files[1],sep=""), header = TRUE, sep = ",")


## get employee info for all months

# number of employee info files
N_files <- length(list_employ_info_files)

for (i1 in 2:N_files) {
  # 
  # load employee data for that month
  employee_data_temp <- read.table(paste(employee_info_path,list_employ_info_files[i1],sep=""), header = TRUE, sep = ",")
  # choose user_id, role columns
  employee_data_temp <- employee_data_temp[,c("user_id","Role")]
  
  # paste roles together
  colnames(employee_data_temp)[2] <- paste("Role",toString(i1-1),sep = "")
  # each role becomes it's own column as this goes on
  
  # merge together by user id. so should get a dataframe
  # of user_id and all their roles
  employee_data <- merge(x = employee_data, y = employee_data_temp, by = "user_id", all = TRUE)
}
# just makes data frame of user, domain, name, email, role1, role2, etc

## tabulate types of employees



## get logon_info
logon_info <- read.table(paste("/home/alex/data_analytics/DataSets1_9182017/","logon_info.csv",sep=""), header = TRUE, sep = ",")

## compute number of logons and number of logoffs
# primary machine
employee_data$primary_machine <- array(NA,c(length(employee_data$Role),1))
# how many pc they touch
employee_data$pc_count <- array(0,c(length(employee_data$Role),1))
# number of times they logon primary machine
employee_data$primary_num_logon <- array(0,c(length(employee_data$Role),1))
# number of times they logoff primary machine
employee_data$primary_num_logoff <- array(0,c(length(employee_data$Role),1))
# number of times they logon in general
employee_data$num_logon <- array(0,c(length(employee_data$Role),1))
# number of times they logoff in general
employee_data$num_logoff <- array(0,c(length(employee_data$Role),1))
# average time of first logon in the morning
employee_data$mean_first_logon <- array(0,c(length(employee_data$Role),1))
# earliest time they logon
employee_data$min_first_logon <- array(0,c(length(employee_data$Role),1))
# latest time they logon
employee_data$max_first_logon <- array(0,c(length(employee_data$Role),1))

for (i1 in 1:length(employee_data$Role)) {
  loc_keep <- paste("ACME/",employee_data$user_id[i1],sep="")==logon_info$user
  d_temp <- logon_info[loc_keep,]
  u_pc <- unique(d_temp$pc)
  
  employee_data$pc_count[i1] <- length(u_pc)
  
  count_each_pc <- array(0,c(length(u_pc),1))
  
  for (i2 in 1:length(u_pc)) {
    count_each_pc[i2] <- sum(u_pc[i2]==d_temp$pc)
  }
  
  loc_i2 <- which(max(count_each_pc)==count_each_pc)
  employee_data$primary_machine[i1] <- u_pc[loc_i2]
  
  
  employee_data$primary_num_logon[i1] <- sum(u_pc[loc_i2]==d_temp$pc & d_temp$activity=="Logon")
  employee_data$primary_num_logoff[i1] <- sum(u_pc[loc_i2]==d_temp$pc & d_temp$activity=="Logoff")
  
  employee_data$num_logon[i1] <- sum(loc_keep & logon_info$activity=="Logon")
  employee_data$num_logoff[i1] <- sum(loc_keep & logon_info$activity=="Logoff")
}



