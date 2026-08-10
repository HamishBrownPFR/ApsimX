
log_qflag <- function(severity = "INFO", category = "GENERAL", message) {
  # Format: [Q-FLAG] | SEVERITY | CATEGORY | Message
  flag_string <- sprintf("[Q-FLAG] | %s | %s | %s", severity, category, message)
  
  # Print to console as a message so it is captured in your pipeline_log.txt
  message(flag_string)
}