# init_aws <- function(AWS_ACCESS_KEY_ID,
#          AWS_SECRET_ACCESS_KEY,
#          AWS_DEFAULT_REGION,
#          bucket
#          ) {
#
# }

enable_push_to_aws <- function() {
  PUSH_TO_AMAZON <- TRUE
}

disable_push_to_aws <- function() {
  PUSH_TO_AMAZON <- FALSE
}

get_push_to_aws <- function() {
  return(PUSH_TO_AMAZON)
}
