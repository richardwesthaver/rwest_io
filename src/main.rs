use axum::{handler::Handler, http::StatusCode, routing::{get, get_service}, Router, response::{Html, IntoResponse}};
use std::net::SocketAddr;
use tower_http::{services::ServeDir, trace::TraceLayer};

mod gen;

const DEFAULT_PORT: u16 = 3030;

#[tokio::main]
async fn main() {
  if std::env::var_os("RUST_LOG").is_none() {
    std::env::set_var("RUST_LOG", "debug")
  }
  tracing_subscriber::fmt::init();

  let args: Vec<String> = std::env::args().collect();
  let port = if args.len() > 1 {
    args[1].parse::<u16>().unwrap()
  } else {
    DEFAULT_PORT
  };

  let socket: SocketAddr = ([0, 0, 0, 0], port).into();

  let app = Router::new()
    .nest(
      "/static",
      get_service(ServeDir::new("static")).handle_error(|e: std::io::Error| async move {
	(StatusCode::INTERNAL_SERVER_ERROR, format!("internal error: {}", e))
      }),
    )
    .route("/blog", get(gen::render_post))
    .layer(TraceLayer::new_for_http())
    .fallback(handler_404.into_service());
  
  tracing::debug!("listening on {}", socket);
  axum::Server::bind(&socket)
    .serve(app.into_make_service())
    .await
    .unwrap();
}

async fn handler() -> Html<&'static str> {
    Html("<h1>Hello, World!</h1>")
}

async fn handler_404() -> impl IntoResponse {
    (StatusCode::NOT_FOUND, "nothing to see here")
}
