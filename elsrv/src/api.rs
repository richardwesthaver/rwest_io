use axum::Router;

pub mod index;
//pub mod auth;
//pub mod run;
//pub mod org;

fn api_router() -> Router {
    // This is the order that the modules were authored in.
    index::router()
    .merge(auth::router())
    .merge(run::router())
    .merge(org::router())
}
