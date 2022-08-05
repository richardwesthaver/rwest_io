use crate::{RWEST_IO, COOKIE_NAME};
use async_trait::async_trait;
use http::header::{self, SET_COOKIE};
use headers::HeaderMap;
use async_session::{Session, SessionStore};
use serde::{Serialize, Deserialize};
// use axum::Router;
use axum::{Extension, TypedHeader,
	   response::{IntoResponse, Redirect, Response, Html},
	   extract::{Query, FromRequest, RequestParts,
		     rejection::TypedHeaderRejectionReason,
	   },
};
use oauth2::{
    basic::BasicClient, reqwest::async_http_client, AuthUrl, AuthorizationCode, ClientId,
    ClientSecret, CsrfToken, RedirectUrl, Scope, TokenResponse, TokenUrl,
};

pub use async_session::MemoryStore;

// The user data we'll get back from Discord.
// https://discord.com/developers/docs/resources/user#user-object-user-structure
#[derive(Debug, Serialize, Deserialize)]
pub struct User {
    id: String,
    avatar: Option<String>,
    username: String,
    discriminator: String,
}

pub fn discord_oauth_client(client_id: &str, client_secret: &str, redirect_url: Option<&str>, auth_url: Option<&str>, token_url: Option<&str>) -> BasicClient {
    // Environment variables (* = required):
    // *"CLIENT_ID"     "REPLACE_ME";
    // *"CLIENT_SECRET" "REPLACE_ME";
    //  "REDIRECT_URL"  "http://127.0.0.1:3000/auth/authorized";
    //  "AUTH_URL"      "https://discord.com/api/oauth2/authorize?response_type=code";
    //  "TOKEN_URL"     "https://discord.com/api/oauth2/token";

  let redirect_url = redirect_url
    .unwrap_or_else(|| {"http://127.0.0.1:8000/auth/authorized"}).to_string();

  let auth_url = auth_url
    .unwrap_or_else(|| {"https://discord.com/api/oauth2/authorize?response_type=code"}).to_string();

    let token_url = token_url
    .unwrap_or_else(|| "https://discord.com/api/oauth2/token").to_string();

    BasicClient::new(
        ClientId::new(client_id.to_string()),
        Some(ClientSecret::new(client_secret.to_string())),
        AuthUrl::new(auth_url).unwrap(),
        Some(TokenUrl::new(token_url).unwrap()),
    )
    .set_redirect_uri(RedirectUrl::new(redirect_url).unwrap())
}

pub async fn discord_auth(Extension(client): Extension<BasicClient>) -> impl IntoResponse {
    let (auth_url, _csrf_token) = client
        .authorize_url(CsrfToken::new_random)
        .add_scope(Scope::new("identify".to_string()))
        .url();
    // Redirect to Discord's oauth service
    Redirect::to(&auth_url.to_string())
}

pub async fn login(
  Extension(_store): Extension<MemoryStore>,
) -> impl IntoResponse {
  Html(r#"
<html>
<p>choose a login method</p>
<ul>
<li><a href = "./auth/discord">discord</a></li>
</ul>
</html>
"#)
}

pub async fn logout(
    Extension(store): Extension<MemoryStore>,
    TypedHeader(cookies): TypedHeader<headers::Cookie>,
) -> impl IntoResponse {
    let cookie = cookies.get(COOKIE_NAME).unwrap();
    let session = match store.load_session(cookie.to_string()).await.unwrap() {
        Some(s) => s,
        // No session active, just redirect
        None => return Redirect::to("/"),
    };

    store.destroy_session(session).await.unwrap();
    Redirect::to(RWEST_IO)
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct AuthRequest {
    code: String,
    state: String,
}

pub async fn discord_login_authorized(
  Query(query): Query<AuthRequest>,
  Extension(store): Extension<MemoryStore>,
  Extension(oauth_client): Extension<BasicClient>,
//  Extension(pool): Extension<PgPool>,
) -> impl IntoResponse {
    // Get an auth token
    let token = oauth_client
        .exchange_code(AuthorizationCode::new(query.code.clone()))
        .request_async(async_http_client)
        .await
        .unwrap();

    // Fetch user data from discord
    let client = reqwest::Client::new();
    let user_data: User = client
        // https://discord.com/developers/docs/resources/user#get-current-user
        .get("https://discordapp.com/api/users/@me")
        .bearer_auth(token.access_token().secret())
        .send()
        .await
        .unwrap()
        .json::<User>()
        .await
        .unwrap();

    // Create a new session filled with user data
    let mut session = Session::new();
    session.insert("user", &user_data).unwrap();

    // Store session and get corresponding cookie
    let cookie = store.store_session(session).await.unwrap().unwrap();

    // Build the cookie
    let cookie = format!("{}={}; SameSite=Lax; Path=/", COOKIE_NAME, cookie);

    // Set cookie
    let mut headers = HeaderMap::new();
    headers.insert(SET_COOKIE, cookie.parse().unwrap());
  (headers, Redirect::to("/"))
}

pub struct AuthRedirect;

impl IntoResponse for AuthRedirect {
    fn into_response(self) -> Response {
        Redirect::temporary("/auth/discord").into_response()
    }
}

#[async_trait]
impl<B> FromRequest<B> for User
where
    B: Send,
{
    // If anything goes wrong or no session is found, redirect to the auth page
    type Rejection = AuthRedirect;

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(store) = Extension::<MemoryStore>::from_request(req)
            .await
            .expect("`MemoryStore` extension is missing");

        let cookies = TypedHeader::<headers::Cookie>::from_request(req)
            .await
            .map_err(|e| match *e.name() {
                header::COOKIE => match e.reason() {
                    TypedHeaderRejectionReason::Missing => AuthRedirect,
                    _ => panic!("unexpected error getting Cookie header(s): {}", e),
                },
                _ => panic!("unexpected error getting cookies: {}", e),
            })?;
        let session_cookie = cookies.get(COOKIE_NAME).ok_or(AuthRedirect)?;

        let session = store
            .load_session(session_cookie.to_string())
            .await
            .unwrap()
            .ok_or(AuthRedirect)?;

        let user = session.get::<User>("user").ok_or(AuthRedirect)?;

        Ok(user)
    }
}
