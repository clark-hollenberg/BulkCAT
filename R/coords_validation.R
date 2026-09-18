# check if observations are clustered around one of the poles.
check_polar <- function(lat) {

  if ((mean(lat) > 70) || (mean(lat) < -70))
    {return(TRUE)}
  else
    {return(FALSE)}

}

# function to check if EOO across the antimeridian is necessary
check_antimeridian <- function(lon) {

  lon_pos <- lon[lon >= 0]
  lon_neg <- lon[lon < 0]

  # Need observations on both sides of Greenwich for antimeridian to be relevant
  if (length(lon_pos) == 0 || length(lon_neg) == 0) {
    return(FALSE)
  }

  # taking means is more robust to truly global distributions
  mean_pos <- mean(lon_pos)
  mean_neg <- mean(lon_neg)

  # Distance between the two groups without wrapping
  normal_distance <- mean_pos - mean_neg

  # Distance between the groups across the antimeridian
  wrapped_distance <- (180 - mean_pos) + (180 + mean_neg)

  return(wrapped_distance < normal_distance)
}
