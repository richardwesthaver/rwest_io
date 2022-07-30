use axum::Router;

pub fn router() -> Router {
  Router::new()
    .route("/login")
    .route("/auth/discord")
    .route("/auth/success")
    .route("/logout")
}
