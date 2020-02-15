#' @title GetFitOauth2Token
#' @rdname GetFitOauth2Token
#' @description
#' Retrieves or refreshes an OAuth2 token. Two options must be set:
#' \itemize{
#' \item \code{RGoogleFit.client_id}
#' \item \code{RGoogleFit.client_secret}
#' }
#' @export
GetFitOauth2Token <- function() {
    client_id = getOption("RGoogleFit.client_id")
    client_secret = getOption("RGoogleFit.client_secret")
    
    assert(!is.null(client_id), "Please set 'RGoogleFit.client_id' option")
    assert(!is.null(client_secret), "Please set 'RGoogleFit.client_secret' option")
    
    oauth2_token <- RGoogleFitObjects$oauth2_object
    
    if (!is.null(oauth2_token)) {
        if (!oauth2_token$validate()) {
            oauth2_token$refresh()
        }
        
    } else {
        oauth2_token <- oauth2.0_token(endpoint = oauth_endpoints("google"), app = oauth_app("google", 
            key = getOption("RGoogleFit.client_id"), secret = getOption("RGoogleFit.client_secret")), 
            scope = c("https://www.googleapis.com/auth/fitness.activity.read", "https://www.googleapis.com/auth/fitness.location.read", 
                "https://www.googleapis.com/auth/fitness.body.read", "https://www.googleapis.com/auth/fitness.nutrition.read"), 
            use_oob = TRUE, cache = TRUE)
        if (!oauth2_token$validate()) {
            oauth2_token$refresh()
        }
    }
    
    RGoogleFitObjects$oauth2_object <- oauth2_token
    
    return(oauth2_token$credentials$access_token)
    
}

#' @title GetFitOauth2Object
#' @rdname GetFitOauth2Object
#' @description
#' Returns the whole Oauth2 object. Useful for debugging purposes.
#' @export
GetFitOauth2Object <- function() {
    
    return(RGoogleFitObjects$oauth2_object)
    
}
