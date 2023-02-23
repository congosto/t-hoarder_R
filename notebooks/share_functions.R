# Shared functions in notebooks

# Get key file
get_key_file <- function(){
  key_file <- dlgInput(paste0(
    "Key File\n",
    "(The file with the bearer token, it must be in the keys directory"))$res
  return(key_file)
}
# Check dataset exists
check_dataset_exist <- function(dataset){
  if (file.exists(dataset)) {
    reply <- dlgInput(message=paste0("El dataset already exists, do you want to overwrite it? (y/n)"))$res
    if (reply != "y") {
      stop("Use another dataset name")
    }else{
      cat("The dataset will be overwritten\n")
      }
  }else{
    dir.create(data_path)
    cat(paste(data_path," created\n"))
  }
}
check_dates <- function(start_time, end_time ) {
  if (end_time == "YYYY-MM-DD 00:00:00") {
    now <- Sys.time() - minutes(5)
    end_time <- with_tz(now, "UCT")
  }else{if (end_time < start_time) {stop ("end_time is less than start_time")}
  }
  return (end_time)
}
