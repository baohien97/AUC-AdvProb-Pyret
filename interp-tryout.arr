import s-exp as S
import lists as L

S.read-s-exp("(+ 3 (* 4 5))")

check:
  S.read-s-exp("(+ 3 (* 4 5))") is
    S.s-list([list:
      S.s-sym("+"),
      S.s-num(3),
      S.s-list([list:
        S.s-sym("*"),
        S.s-num(4),
        S.s-num(5)])])
end

data ArithC:
  | numC(n :: Number)
  | plusC(l :: ArithC, r :: ArithC)
  | multC(l :: ArithC, r :: ArithC)
end

data ArithExt:
  | numExt(n :: Number)
  | plusExt(l :: ArithExt, r :: ArithExt)
  | multExt(l :: ArithExt, r :: ArithExt)
  | bminusExt(l :: ArithExt, r :: ArithExt)
  | uminusExt(e :: ArithExt)
end 

fun desugar(s :: ArithExt) -> ArithC:
  cases (ArithExt) s:
    | numExt(n) => numC(n)
    | plusExt(l, r) => plusC(desugar(l), desugar(r))
    | multExt(l, r) => multC(desugar(l), desugar(r))
    | bminusExt(l, r) => plusC(desugar(l), multC(numC(-1), desugar(r)))
    | uminusExt(e) => desugar(bminusExt(numExt(0), e))
      # multC(numExt(-1), desugar(e))
      # bminusExt(numExt(0), desugar(e)) => data type error
  end
end 

fun parse(s :: S.S-Exp) -> ArithExt:
  cases (S.S-Exp) s:
    | s-num(n) => numExt(n)
    | s-list(shadow s) =>
      cases (List) s:
        | empty => raise("parse: unexpected empty list")
        | link(op, args) =>
          argL = L.get(args, 0)
          argR = L.get(args, 1)
          if op.s == "+":
            plusExt(parse(argL), parse(argR))
          else if op.s == "-":
            bminusExt(parse(argL), parse(argR))
          else if op.s == "*":
            multExt(parse(argL), parse(argR))
          end
      end
    | else => raise("parse: not number or list")
  end
end

fun interp(e :: ArithC) -> Number: # keep this unchanged
  cases (ArithC) e:
    | numC(n) => n
    | plusC(l, r) => interp(l) + interp(r)
    | multC(l, r) => interp(l) * interp(r)
  end
where:
  interp(desugar(parse(S.read-s-exp("(- -1 2)")))) is -3
end


