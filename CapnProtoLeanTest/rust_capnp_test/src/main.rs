use capnp::message;
use rust_capnp_test::schema_capnp;

fn main() -> Result<(), capnp::Error> {
    let mut message = message::Builder::new_default();
    let mut b = message.init_root::<schema_capnp::test::Builder::<'_>>();
    
    b.set_foo(255);
    b.set_bar(&[1,2,3,4,5,6,7,8,9,10])?;
    let mut elems = b.init_zing(2);
    for i in 0..2 {
        elems.reborrow().get(i).set_foo(16 * (1 + (i as u64)) + 1);
    }

    //let r = message.into_reader();
    capnp::serialize::write_message(
        std::io::stdout(),
        &message)?;
    return Ok(());
}
