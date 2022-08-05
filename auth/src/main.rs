use anyhow::Result;
use auth::cfg::Cfg;
use clap::Parser;
use proto::{
  auth::{discord_oauth_client, discord_auth, discord_login_authorized, login, logout, MemoryStore},
};

use axum::{
  extract::Extension,
  routing::get,
  Router,
};
use std::net::SocketAddr;
use tower_http::trace::TraceLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> Result<()> {
  tracing_subscriber::registry()
    .with(tracing_subscriber::EnvFilter::new(
      std::env::var("RUST_LOG")
        .unwrap_or_else(|_| "authsrv=debug,tower_http=debug".into()),
        ))
    .with(tracing_subscriber::fmt::layer())
    .init();

  let config = Cfg::parse();

  let addr: SocketAddr = config.addr;
  println!("listening on {}", addr);
  let oauth_client = discord_oauth_client(
    &config.discord_client_id,
    &config.discord_client_secret,
    Some(&config.discord_redirect_url),
    Some(&config.discord_auth_url),
    Some(&config.discord_token_url)
  );
  let oauth_store = MemoryStore::new();
  let app = Router::new()
    .route("/auth/discord", get(discord_auth))
    .route("/auth/authorized", get(discord_login_authorized))
    .route("/login", get(login))
    .route("/logout", get(logout))
    .layer(TraceLayer::new_for_http())
    .layer(Extension(oauth_store))
    .layer(Extension(oauth_client));  

  axum::Server::bind(&addr)
    .serve(app.into_make_service())
    .await?;
  Ok(())
}
