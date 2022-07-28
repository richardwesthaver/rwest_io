use askama::Template;

// path relative to the working directory when you run the server binary
pub const PKG_DIR: &str = "pkg";
pub const FAVICON_FILE: &str = "favicon.ico";
pub const CSS_FILE: &str = "style.css";

#[derive(Template)]
#[template(path = "base.html")]
pub struct BaseTemplate<'a> {
  pub title: &'a str,
}

#[derive(Template)]
#[template(path = "rocks.html")]
pub struct TestTemplate;

pub async fn render_base() -> BaseTemplate<'static> {
  BaseTemplate {title: "rwest.io"}
}

pub async fn render_post() -> TestTemplate {
  TestTemplate
}
