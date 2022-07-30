use sqlx::postgres::PgPoolOptions;
use sqlx::PgPool;
use sqlx::migrate::{MigrateError, Migrator};
use std::net::SocketAddr;

#[derive(Debug)]
pub struct Db {
  pool: PgPool,
}

impl Db {
  pub fn new() {

  }
  pub async fn migrate(&self) -> Result<(), MigrateError> {
    sqlx::migrate!("./migrations").run(&self.pool).await
  }
}
