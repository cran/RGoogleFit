#' @title GetFitOauth2Token
#' @rdname GetFitOauth2Token
#' @description
#' Retrieves or refreshes an OAuth2 token. Two options must be set:
#' \code{RGoogleFit.client_id}
#' \code{RGoogleFit.client_secret}
#' @export
GetFitOauth2Token <- function() {
  client_id = getOption('RGoogleFit.client_id')
  client_secret = getOption('RGoogleFit.client_secret')
  
  assert(!is.null(client_id),
         'Please set \'RGoogleFit.client_id\' option')
  assert(!is.null(client_secret),
         'Please set \'RGoogleFit.client_secret\' option')
  
  oauth2_token <- get0('RGoogleFit.token')
  
  if (!is.null(oauth2_token)) {
    if (!oauth2_token$validate()) {
      oauth2_token$refresh()
    }
    
  }
  else
  {
    oauth2_token <- oauth2.0_token(
      endpoint = oauth_endpoints("google"),
      app = oauth_app(
        "google",
        key = getOption("RGoogleFit.client_id"),
        secret = getOption("RGoogleFit.client_secret")
      ),
      scope = c(
        "https://www.googleapis.com/auth/fitness.activity.read",
        "https://www.googleapis.com/auth/fitness.location.read"
      ) ,
      use_oob = FALSE,
      cache = TRUE
    )
  }
  
  assign('RGoogleFit.token', oauth2_token)
  
  return(oauth2_token$credentials$access_token)
  
}