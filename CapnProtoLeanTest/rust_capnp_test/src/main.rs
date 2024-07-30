use capnp::message;
use rust_capnp_test::schema_capnp;

fn main() -> Result<(), capnp::Error> {
    let mut message = message::Builder::new_default();
    let mut b = message.init_root::<schema_capnp::request::Builder::<'_>>();
    
    b.set_name("bahaha");
    let v = b.init_value();
    let mut t = v.init_expr().init_binary();
    t.reborrow().init_lhs().init_var().set_name("xxx");
    t.init_rhs().init_const().set_value(1);
    capnp::serialize::write_message(
        std::io::stdout(),
        &message)?;
    return Ok(());
}
