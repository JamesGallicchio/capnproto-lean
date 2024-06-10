@0xf96cf60b14767df6;

struct Test {
    foo @0 : UInt64;
    bar @1 : List(UInt64);
    zing @2 : List(Test);
}

struct Request {
    name @0 : Text;
    expr @1 : Expr;
}

struct Expr {
    union {
        var :group {
            name @0 : Text;
        }
        const :group {
            value @1 : Text;
        }
        binary :group {
            lhs @2 : Expr;
            rhs @3 : Expr;
        }
    }
}
