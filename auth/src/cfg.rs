use std::net::SocketAddr;

#[derive(clap::Parser)]
pub struct Cfg {
  /// host address for elsrv
  #[clap(default_value="127.0.0.1:8000", env)]
  pub addr: SocketAddr,
  /// host address of postgres DB
  #[clap(default_value="127.0.0.1:3000", long, env)]
  pub database_url: SocketAddr,
  /// oauth client_id
  #[clap(long, env)]
  pub discord_client_id: String,
  /// oauth client_secret
  #[clap(long, env)]
  pub discord_client_secret: String,
  /// oauth redirect_url
  #[clap(long, env, default_value="http://127.0.0.1:8000/auth/authorized")]
  pub discord_redirect_url: String,
  /// oauth auth_url
  #[clap(long, env, default_value="https://discord.com/api/oauth2/authorize?response_type=code")]
  pub discord_auth_url: String,
  /// oauth token_url
  #[clap(long, env, default_value="https://discord.com/api/oauth2/token")]
  pub discord_token_url: String,
}
