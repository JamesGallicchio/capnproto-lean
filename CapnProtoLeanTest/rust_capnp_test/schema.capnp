@0xf96cf60b14767df6;

struct Test {
    foo @0 : UInt64;
    bar @1 : List(UInt64);
    zing @2 : List(Test);
    bonk @3 : Bool;
    boonk @4 : Bool;
}

struct Request {
    name @0 : Text;
    value :union {
        test @1 : Test;
        expr @2 : Expr;
    }
}

struct Expr {
    union {
        var :group {
            name @0 : Text;
        }
        const :group {
            value @1 : UInt64;
        }
        binary :group {
            lhs @2 : Expr;
            rhs @3 : Expr;
        }
    }
}
