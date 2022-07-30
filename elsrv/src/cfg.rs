use std::path::PathBuf;

#[derive(clap::Parser)]
pub struct Cfg {
  #[clap(default_value=".", env)]
  pub org_path: PathBuf,
  /// host address for elsrv
  #[clap(default_value="127.0.0.1:8080", env)]
  pub elsrv_addr: String,
  /// host address of postgres DB
  #[clap(long, env)]
  pub db_addr: String,
  /// oauth client_id
  #[clap(long, env)]
  pub discord_client_id: String,
  /// oauth client_secret
  #[clap(long, env)]
  pub discord_client_secret: String,
  /// oauth redirect_url
  #[clap(long, env, default_value="https://127.0.0.1:3000/auth/authorized")]
  pub discord_redirect_url: String,
  /// oauth auth_url
  #[clap(long, env, default_value="https://discord.com/api/oauth2/authorize?response_type=code")]
  pub discord_auth_url: String,
  /// oauth token_url
  #[clap(long, env, default_value="https://discord.com/api/oauth2/token")]
  pub discord_token_url: String,
}
