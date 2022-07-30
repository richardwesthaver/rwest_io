use elsrv::cfg::Cfg;

use clap::Parser;
use anyhow::Result;
use async_session::{MemoryStore, Session, SessionStore};
use std::result::Result as StdResult;
use axum::{
  body::Body,
  
  http::StatusCode,
    async_trait,
    extract::{
        rejection::TypedHeaderRejectionReason, Extension, FromRequest, Query, RequestParts,
        TypedHeader,
    },
    http::{header::SET_COOKIE, HeaderMap, Request},
    response::{IntoResponse, Redirect, Response},
  routing::{get, get_service, any_service},
    Router,
};
use http::header;
use oauth2::{
    basic::BasicClient, reqwest::async_http_client, AuthUrl, AuthorizationCode, ClientId,
    ClientSecret, CsrfToken, RedirectUrl, Scope, TokenResponse, TokenUrl,
};
use serde::{Deserialize, Serialize};
use std::{io, net::SocketAddr, env, path::{PathBuf, Path}, fs, convert::Infallible};
use tower::service_fn;
use tower_http::{services::{ServeDir, ServeFile}, trace::TraceLayer};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use tracing::trace;

static COOKIE_NAME: &str = "SESSION_ID";
static RWEST_IO: &str = "https://rwest.io";
static UNAUTH_DEFAULT: &str = "visit `/auth/discord` to begin.";

#[derive(Debug)]
pub struct ClientState {
  socket: SocketAddr,
//  user_id: 
// TODO: crypto
}


#[derive(Debug)]
pub enum ServerStatus {
  Up,
  Down,
}

#[derive(Debug)]
pub struct ServerState {
  status: ServerStatus,
  rx_socket: SocketAddr,
  tx_socket: SocketAddr,
  clients: Vec<ClientState>,
  location: PathBuf,
// TODO: crypto
}

impl Default for ServerState {
  fn default() -> Self {
    ServerState {
      status: ServerStatus::Down,
      rx_socket: "127.0.0.1:3000".parse().unwrap(),
      tx_socket: "127.0.0.1:3000".parse().unwrap(),
      clients: vec![],
      location: PathBuf::from("."),
    }
  }
}
pub struct Server {
  state: ServerState,
}

impl Server {
  pub fn new() -> Server {
    Server {
      state: ServerState::default(),
    }
  }
}

pub fn map_org_dir<P: AsRef<Path>>(path: P, mut coll: &mut Vec<String>) -> StdResult<(), io::Error> {
  let path = path.as_ref();
  if path.is_dir() {
    for elt in fs::read_dir(path)? {
      let elt = elt?;
      let p = elt.path();
      if p.is_dir() {
        map_org_dir(p, &mut coll)?;
      } else if p.is_file() {
        coll.push(p.display().to_string())
      }
    }
  } else if path.is_file() {
      coll.push(path.display().to_string())
  }
  Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
  tracing_subscriber::registry()
    .with(tracing_subscriber::EnvFilter::new(
      std::env::var("RUST_LOG")
        .unwrap_or_else(|_| "elsrv=debug,tower_http=debug".into()),
        ))
    .with(tracing_subscriber::fmt::layer())
    .init();

  let config = Cfg::parse();
  let org_path = config.org_path;
  let mut org_files: Vec<String> = vec![];
  map_org_dir(&org_path, &mut org_files)?;
  
  let mut print_files = String::new();
  for i in org_files.iter() {
    print_files.push_str(&format!("{}\n", i));
  }
  let addr: SocketAddr = config.elsrv_addr.parse()?;
  println!("listening on {}", addr);
  println!("/org -- {}", &org_path.display());
  println!("/auth/discord");
  println!("/logout\n\n");
  println!("org files found:");
  println!("{}", &print_files);
  let oauth_client = discord_oauth_client(
    &config.discord_client_id,
    &config.discord_client_secret,
    Some(&config.discord_redirect_url),
    Some(&config.discord_auth_url),
    Some(&config.discord_token_url)
  );
  let oauth_store = MemoryStore::new();
  let mut app = Router::new()
    .route("/", get(index))
    .route("/org", get(|user: Option<User>| async
		       {
			 match user {
			   Some(u) => print_files,
			   None => UNAUTH_DEFAULT.to_string(),
			 }
		       }
    ))
//    .fallback(get_service(ServeDir::new(org_path)).handle_error(handle_io_error))
    .route("/auth/discord", get(discord_auth))
    .route("/auth/authorized", get(login_authorized))
    .route("/logout", get(logout))
    .layer(TraceLayer::new_for_http())
    .layer(Extension(oauth_store))
    .layer(Extension(oauth_client));  

  for i in org_files.iter() {
    app = app.route(&format!("{}",i.strip_prefix("../").unwrap()), get_service(ServeFile::new(i)).handle_error(handle_io_error));
  }
  axum::Server::bind(&addr)
    .serve(app.into_make_service())
    .await
    .unwrap();
  Ok(())
}

async fn handle_io_error(_err: io::Error) -> impl IntoResponse {
    (StatusCode::INTERNAL_SERVER_ERROR, "Something went wrong...")
}

fn discord_oauth_client(client_id: &str, client_secret: &str, redirect_url: Option<&str>, auth_url: Option<&str>, token_url: Option<&str>) -> BasicClient {
    // Environment variables (* = required):
    // *"CLIENT_ID"     "REPLACE_ME";
    // *"CLIENT_SECRET" "REPLACE_ME";
    //  "REDIRECT_URL"  "http://127.0.0.1:3000/auth/authorized";
    //  "AUTH_URL"      "https://discord.com/api/oauth2/authorize?response_type=code";
    //  "TOKEN_URL"     "https://discord.com/api/oauth2/token";

  let redirect_url = redirect_url
    .unwrap_or_else(|| {"http://127.0.0.1:3000/auth/authorized"}).to_string();

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

// The user data we'll get back from Discord.
// https://discord.com/developers/docs/resources/user#user-object-user-structure
#[derive(Debug, Serialize, Deserialize)]
struct User {
    id: String,
    avatar: Option<String>,
    username: String,
    discriminator: String,
}

// Session is optional
async fn index(user: Option<User>) -> impl IntoResponse {
    match user {
      Some(u) => format!(
        "operator: {:?}
/el -- execute elisp
/org -- view org files",
        u,
      ),
      None => UNAUTH_DEFAULT.to_string(),
    }
}

async fn discord_auth(Extension(client): Extension<BasicClient>) -> impl IntoResponse {
    let (auth_url, _csrf_token) = client
        .authorize_url(CsrfToken::new_random)
        .add_scope(Scope::new("identify".to_string()))
        .url();
    // Redirect to Discord's oauth service
    Redirect::to(&auth_url.to_string())
}

async fn logout(
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

//  TODO 2022-07-29: implement org handler
//    - this requires that the file list is available in a store so that
//      it can be accessed
//    - requires auth (can make a macro for this condition)
//    - make sure all files are unaccessible without auth
async fn org_index(
  user: Option<User>) -> impl IntoResponse {
  match user {
    Some(_) => "".to_string(),
    None => UNAUTH_DEFAULT.to_string(),
  }
}
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct AuthRequest {
    code: String,
    state: String,
}

async fn login_authorized(
    Query(query): Query<AuthRequest>,
    Extension(store): Extension<MemoryStore>,
    Extension(oauth_client): Extension<BasicClient>,
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

struct AuthRedirect;

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
