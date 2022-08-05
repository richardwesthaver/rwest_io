use async_trait::async_trait;
use sqlx::postgres::PgPoolOptions;
use sqlx::PgPool;
use http::StatusCode;
use sqlx::migrate::MigrateError;
use std::time::Duration;
use axum::extract::{FromRequest, RequestParts, Extension};

#[derive(Debug)]
pub struct Db {
  pool: PgPool,
}

impl Db {
  pub async fn new(db_addr: &str) -> Db {
    let pool = PgPoolOptions::new().max_connections(32).idle_timeout(Duration::from_secs(4))
      .connect(&db_addr).await.expect("could not establish db connection.");

    Db { pool }
  }
  pub async fn migrate(&self) -> Result<(), MigrateError> {
    sqlx::migrate!("./migrations").run(&self.pool).await
  }
}

// we can also write a custom extractor that grabs a connection from the pool
// which setup is appropriate depends on your application
struct DatabaseConnection(sqlx::pool::PoolConnection<sqlx::Postgres>);

#[async_trait]
impl<B> FromRequest<B> for DatabaseConnection
where
    B: Send,
{
    type Rejection = (StatusCode, String);

    async fn from_request(req: &mut RequestParts<B>) -> Result<Self, Self::Rejection> {
        let Extension(pool) = Extension::<PgPool>::from_request(req)
            .await.unwrap();


      let conn = pool.acquire().await.unwrap();

        Ok(Self(conn))
    }
}
