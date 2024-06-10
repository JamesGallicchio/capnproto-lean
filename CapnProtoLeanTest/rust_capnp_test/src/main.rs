use capnp::message;
use rust_capnp_test::schema_capnp;

fn main() -> Result<(), capnp::Error> {
    let mut message = message::Builder::new_default();
    let b = message.init_root::<schema_capnp::expr::Builder::<'_>>();
    
    let mut b = b.init_var();
    b.set_name("bahaha");
    capnp::serialize::write_message(
        std::io::stdout(),
        &message)?;
    return Ok(());
}
