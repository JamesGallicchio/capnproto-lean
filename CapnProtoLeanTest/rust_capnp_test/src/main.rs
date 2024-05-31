use capnp::message;
use rust_capnp_test::schema_capnp;

fn main() -> Result<(), capnp::Error> {
    let mut message = message::Builder::new_default();
    let mut b = message.init_root::<schema_capnp::test::Builder::<'_>>();
    
    b.set_foo(255);
    b.set_bar(&[1,2,3,4,5,6,7,8,9,10])?;
    let mut elems = b.init_zing(2);
    for i in 0..2 {
        let mut b = elems.reborrow().get(i);
        b.set_foo(16 * (1 + (i as u64)) + 1);
        b.set_bar(&[1000,2000,3000])?;
    }

    capnp::serialize::write_message(
        std::io::stdout(),
        &message)?;
    return Ok(());
}
