use http::StatusCode;
use axum::response::{IntoResponse, Response};


pub async fn internal_error<E>(_err: E) -> impl IntoResponse
where
    E: std::error::Error,
{
    (StatusCode::INTERNAL_SERVER_ERROR, "Something went wrong...")
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
  #[error("an internal database error occurred")]
  Sqlx(#[from] sqlx::Error),
  #[error("{0}")]
  UnprocessableEntity(String),  
}

impl IntoResponse for Error {
    fn into_response(self) -> Response {
        // Normally you wouldn't just print this, but it's useful for debugging without
        // using a logging framework.
      tracing::warn!("API error: {:?}", self);

      self.status_code()
      .into_response()
    }
}

impl Error {
    fn status_code(&self) -> StatusCode {
        use Error::*;

        match self {
            Sqlx(_) => StatusCode::INTERNAL_SERVER_ERROR,
            UnprocessableEntity(_) => StatusCode::UNPROCESSABLE_ENTITY,
        }
    }
}
