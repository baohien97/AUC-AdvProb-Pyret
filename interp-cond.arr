import s-exp as S
import lists as L

data ExprC:
  | trueC
  | falseC
  | numC(n :: Number)
  | plusC(l :: ExprC, r :: ExprC)
  | multC(l :: ExprC, r :: ExprC)
  | ifC(test :: ExprC, consequent :: ExprC, alternate :: ExprC) # => not a statement but a data type
  | ifC2(test :: ExprC, consequent :: ExprC)
  | condC(exps :: List<ExprC>)
  | notC(l :: ExprC, r :: ExprC)
end

data ExprExt:
  | trueExt
  | falseExt
  | numExt(n :: Number)
  | plusExt(l :: ExprExt, r :: ExprExt)
  | multExt(l :: ExprExt, r :: ExprExt)
  | bminusExt(l :: ExprExt, r :: ExprExt)
  | uminusExt(e :: ExprExt)
  | ifExt(test :: ExprExt, consequent :: ExprExt, alternate :: ExprExt)
  | condExt(exps :: List<ExprExt>) 
  | notExt(l :: ExprExt, r :: ExprExt)
end
data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
end

fun desugar(s :: ExprExt) -> ExprC:
  cases (ExprExt) s:
    | trueExt => trueC
    | falseExt => falseC
    | numExt(n) => numC(n)
    | plusExt(l, r) => plusC(desugar(l), desugar(r))
    | multExt(l, r) => multC(desugar(l), desugar(r))
    | bminusExt(l, r) => plusC(desugar(l), multC(numC(-1), desugar(r)))
    | uminusExt(e) => multC(numExt(-1), desugar(e))
    | ifExt(test, consequent, alternate) => ifC(desugar(test), desugar(consequent), desugar(alternate))
    | condExt(exps) => condC(desugar(exps))
    | notExt(l, r) => notC(desugar(l), desugar(r))
  end
where:
  desugar(ifExt(falseExt, numExt(5), multExt(numExt(-2), numExt(5)))) is ifC(falseC, numC(5), multC(numC(-2), numC(5))) 
  desugar(ifExt(notExt(plusExt(numExt(3), numExt(4)), numExt(9)), numExt(7), numExt(10))) is ifC(notC(plusC(numC(3), numC(4)), numC(9)), numC(7), numC(10))
end

fun parse(s :: S.S-Exp) -> ExprExt:
  cases (S.S-Exp) s:
    | s-num(n) => numExt(n)
    | s-sym(sym) => 
      if sym == "true":
        trueExt
      else if sym == "false":
        falseExt
      end
    | s-list(l) =>
      cases (List<S.S-Exp>) l:
        | empty => raise("unexpected empty list")
        | link(exp, args) =>
          if exp.s == "if":
            if (args.length() < 3) or (args.length() > 3):
              "need 3 arguments"             
            else if args.length() == 3: 
              test = L.get(args, 0)
              consequent = L.get(args, 1)
              alternate = L.get(args, 2)
              ifExt(parse(test), parse(consequent), parse(alternate))
            end
          else if exp.s == "cond":
            cases (List) args:
              | empty => raise("")
              | link(fr, rr) => 

            end
          else:
            argL = L.get(args, 0)
            argR = L.get(args, 1)
            if exp.s == "+":
              plusExt(parse(argL), parse(argR))
            else if exp.s == "-":
              bminusExt(parse(argL), parse(argR))
            else if exp.s == "*":
              multExt(parse(argL), parse(argR))
            else if (exp.s == ">") and (argL.n > argR.n):
              trueExt
            else if (exp.s == "<") and (argL.n < argR.n):
              trueExt             
            else if (exp.s == "=") and (argL.n == argR.n):
              trueExt
            else if (exp.s == "and") and ((parse(argL) == trueExt) and (parse(argR) == trueExt)):
              trueExt
            else if (exp.s == "or") and (((parse(argL) == trueExt) and (parse(argR) == trueExt)) or ((parse(argL) == falseExt) and (parse(argR) == trueExt)) or ((parse(argL) == trueExt) and (parse(argR) == trueExt))):
              trueExt
            else if (exp.s == "not"):
              notExt(parse(argL), parse(argR))
            else:
              falseExt              
            end
          end       
      end
    | else => raise("not parsable")
  end
where:
  parse(S.read-s-exp("(if true (+ 5 6) (* 7 8))")) is ifExt(trueExt, plusExt(numExt(5), numExt(6)), multExt(numExt(7), numExt(8)))
  parse(S.read-s-exp("(if (> 3 4) (+ 5 6) (* 8 10))")) is ifExt(falseExt, plusExt(numExt(5), numExt(6)), multExt(numExt(8), numExt(10)))
  parse(S.read-s-exp("(if (< 3 4) (+ 5 (* 3 4)) 5)")) is ifExt(trueExt, plusExt(numExt(5), multExt(numExt(3), numExt(4))), numExt(5))
  parse(S.read-s-exp("(if (= 5 6) 5 (* -2 5))")) is ifExt(falseExt, numExt(5), multExt(numExt(-2), numExt(5)))   
  parse(S.read-s-exp("(if (or (> 3 4) (> 4 3)) (+ 5 6) (* 9 10))")) is ifExt(trueExt, plusExt(numExt(5), numExt(6)), multExt(numExt(9), numExt(10)))
  parse(S.read-s-exp("(if (and (> 5 4) (< 4 3)) 5 6)")) is ifExt(falseExt, numExt(5), numExt(6))
  parse(S.read-s-exp("(if (not (+ 3 4) 9) 7 10)")) is ifExt(notExt(plusExt(numExt(3), numExt(4)), numExt(9)), numExt(7), numExt(10))
  parse(S.read-s-exp("(cond ((> 1 2) 9) ((= 5 6) 10) ((< 9 10) 11))")) is condExt([list: ]) # Write a different function for correctly outputing the behaviour of if with a single branch.
  
end

fun interp(e :: ExprC) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(n)
    | plusC(l, r) => arith-binop({(x, y): x + y}, l, r)
    | multC(l, r) => arith-binop({(x, y): x * y}, l, r)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | notC(l, r) =>
      if (interp(l).n > interp(r).n) or (interp(l).n < interp(r).n):
        boolV(true)
      else:
        boolV(false)
      end
    | ifC(test, consequent, alternative) =>
    ic = interp(test)
    if is-boolV(ic): # built-in syntax of Pyret
      if ic.b: # b :: Boolean
        interp(consequent)
      else:
        interp(alternative)
      end
    else:
      raise('not a boolean')
    end
  end
where:
  interp(numC(8)) is numV(8)
  interp(plusC(numC(3), numC(4))) is numV(7)
  interp(multC(numC(5), numC(6))) is numV(30)
  interp(multC(plusC(numC(1), numC(2)), multC(numC(2), numC(5)))) is numV(30)
  interp(ifC(trueC, numC(3), numC(4))) is numV(3)
  interp(ifC(falseC, numC(5), numC(6))) is numV(6)
  interp(desugar(parse(S.read-s-exp("(if (> 3 4) (+ 5 6) (* 7 8))")))) is numV(56)
  interp(desugar(parse(S.read-s-exp("(if (< 7 9) (* -2 5) (* 5 8))")))) is numV(-10)
  interp(desugar(parse(S.read-s-exp("(if (< 8 9) (* 2 (* 3 5)) (+ 9 8))")))) is numV(30)
  interp(desugar(parse(S.read-s-exp("(if (= 5 6) 5 (* -2 5))")))) is numV(-10)
  interp(desugar(parse(S.read-s-exp("(if (or (> 3 4) (> 4 3)) (+ 5 6) (* 9 10))")))) is numV(11)
  interp(desugar(parse(S.read-s-exp("(if (and (> 5 4) (< 4 3)) 5 6)")))) is numV(6)
  interp(desugar(parse(S.read-s-exp("(if (not (+ 3 4) 9) 7 10)")))) is numV(7)    
end

fun arith-binop(op :: (Number, Number -> Number), l :: ExprC, r :: ExprC) -> Value:
  lval = interp(l)
  rval = interp(r)
  if (lval.n > 128) or (rval.n > 128):
    raise('this only works for 8-bit arithmetic')
  else if is-numV(lval) and is-numV(rval):
    numV(op(lval.n, rval.n))
  else:
    raise('argument not a number')
  end
end
