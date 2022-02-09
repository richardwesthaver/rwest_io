use client::App;
use sauron::Render;
use std::convert::Infallible;
use std::net::SocketAddr;
use warp::{http::Response, Filter};

mod page;

// path relative to the working directory when you run the server binary
const PKG_DIR: &str = "client/pkg";
const FAVICON_FILE: &str = "client/favicon.ico";
const STYLE_CSS_FILE: &str = "client/style.css";
const DEFAULT_PORT: u16 = 3030;

#[tokio::main]
async fn main() {
  let pkg_files = warp::path("pkg").and(warp::fs::dir(PKG_DIR));
  let favicon = warp::path("favicon.ico").and(warp::fs::file(FAVICON_FILE));
  let style_css = warp::path("style.css").and(warp::fs::file(STYLE_CSS_FILE));

  let home =
    warp::path::end().and_then(move || render_index(StorySorting::Top));

  let routes = index
    .or(warp::get()
        .and(pkg_files.or(favicon).or(favicon_svg).or(style_css)));

  let port = if let Ok(port) = std::env::var("PORT") {
    if let Ok(port) = port.parse::<u16>() {
      port
    } else {
      DEFAULT_PORT
    }
  } else {
    DEFAULT_PORT
  };

  let socket: SocketAddr = ([0, 0, 0, 0], port).into();
  println!("serving at: {}", socket);

  warp::serve(routes).run(socket).await;
}

async fn render_index(
    sorting: StorySorting,
) -> Result<impl warp::Reply, Infallible> {
    let stories = api::get_stories_with_sorting(sorting).await.expect("must not error");
    let app = App::with_stories(stories);
    let index = page::index(&app).render_to_string();
    Ok(Response::builder().body(index))
}
