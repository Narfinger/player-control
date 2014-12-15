extern crate nickel;
extern crate http;

use std::io::net::ip::Ipv4Addr;
use std::collections::HashMap;
use nickel::{ Nickel, Request, Response, NickelError, HttpRouter, MiddlewareResult, StaticFilesHandler};
use nickel::{Halt, Continue};
//middleware stuff
use http::method::{Get, Head};//for middleware handling
use std::string::String;
use std::io::File;
use std::default::Default;

mod dbus;

fn index_handler (request: &Request, response: &mut Response) {
    let mut data = HashMap::<&str, &str>::new();
    data.insert("Artist", "TODO");
    data.insert("Title", "TODO");
    data.insert("Album", "TODO");
    data.insert("StatusM", "TODO");
    data.insert("StatusS", "TODO");
    data.insert("TitleS", "TODO");

    response.render("index.tpl", &data);
}


//ideally i want to write a custom middleware hwere i can just input strings and it will check and outpu
//but i can't get this middleware to work
/*#[deriving(Clone)]
#[deriving(Default)]
struct StaticFileMiddleware {
    files: HashMap<String, String>
}
impl Send for StaticFileMiddleware { }
impl Sync for StaticFileMiddleware { }

#[no(warn_unused_variables)]
impl nickel::Middleware for StaticFileMiddleware {
    fn invoke<'a, 'b> (&self, req: &mut Request<'b, 'a>, res: &mut Response)  -> MiddlewareResult {
        print!("TTT");
        Ok(Continue)
/*        match req.origin.method {
            Get | Head => {
                res.send("TTTTT");
                Ok(Halt)
            },            
            _ => Ok(Continue)
        }*/
    }
}
*/

fn main() {
    let mut server = Nickel::new();
    let mut router = Nickel::router();

    router.get("/", index_handler);
    server.utilize(router);

    let t = dbus::env_kded4_pid();
    match t {
       Ok(v) => println!("{}", v),
       Err(e) => println!("{}", e),
    };
    
//    server.utilize(StaticFilesHandler::new("static/"));
//    server.listen(Ipv4Addr(127, 0, 0, 1), 6767);

    println!("Hello, world!");
}
