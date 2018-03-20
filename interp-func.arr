import s-exp as S
import lists as L

data FunDefC:
  | fdC(name :: String, arg :: String, body :: ExprC)
end

data ExprC:
  | trueC
  | falseC
  | numC(n :: Number)
  | plusC(l :: ExprC, r :: ExprC)
  | multC(l :: ExprC, r :: ExprC)
  | ifC(c :: ExprC, t :: ExprC, e :: ExprC)
  | greaterC(l :: ExprC, r :: ExprC)
  | smallerC(l :: ExprC, r :: ExprC)
  | equalC(l :: ExprC, r :: ExprC)
  | notC(e :: ExprC)
  | andC(l :: ExprC, r :: ExprC)
  | appC(f :: String, a :: ExprC)
  | idC(s :: String)
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
  | ifExt2(test :: ExprExt, consequent :: ExprExt)
  | greaterExt(l :: ExprExt, r :: ExprExt)
  | smallerExt(l :: ExprExt, r :: ExprExt)
  | equalExt(l :: ExprExt, r :: ExprExt)
  | notExt(e :: ExprExt)
  | andExt(l :: ExprExt, r :: ExprExt)
  | orExt(l :: ExprExt, r :: ExprExt)
  | condExt(exps :: List<ExprExt>) 
  | appExt(f :: String, a :: ExprExt)
  | idExt(s :: String)
end

data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
end

data Binding:
  | bind(name :: String, value :: Value)
end

fun if-converter(l :: List<ExprExt>) -> ExprExt:
  doc: "converts list of IfExt2 to IfExt structure"
  cases (List) l:
    | empty => falseExt
    | link(f, r) => 
      cases (ExprExt) f:
        | ifExt2(test, consequent) => ifExt(test, consequent, if-converter(r))
        | else => raise("")
      end
  end
where:
  if-converter(empty) is falseExt
  if-converter([list: ifExt2(falseExt, numExt(9)), ifExt2(trueExt, numExt(10))]) is ifExt(falseExt, numExt(9), ifExt(trueExt, numExt(10), falseExt))
  if-converter([list: ifExt2(greaterExt(numExt(1), numExt(2)), numExt(9)), ifExt2(equalExt(numExt(5), numExt(6)), numExt(10))]) is ifExt(greaterExt(numExt(1), numExt(2)), numExt(9), ifExt(equalExt(numExt(5), numExt(6)), numExt(10), falseExt))
end

fun desugar(s :: ExprExt) -> ExprC:
  cases (ExprExt) s:
    | trueExt => trueC
    | falseExt => falseC
    | numExt(n) => numC(n)
    | plusExt(l, r) => plusC(desugar(l), desugar(r))
    | multExt(l, r) => multC(desugar(l), desugar(r))
    | bminusExt(l, r) => plusC(desugar(l), multC(numC(-1), desugar(r)))
    | uminusExt(e) => desugar(bminusExt(numExt(0), e))
    | ifExt(test, consequent, alternate) => ifC(desugar(test), desugar(consequent), desugar(alternate))
    | ifExt2(test, consequent) => ifC(desugar(test), desugar(consequent), falseC)
    | greaterExt(l, r) => greaterC(desugar(l), desugar(r))
    | smallerExt(l, r) => smallerC(desugar(l), desugar(r))
    | equalExt(l, r) => equalC(desugar(l), desugar(r))
    | notExt(e) => notC(desugar(e))
    | andExt(l ,r) => andC(desugar(l), desugar(r))
    | orExt(l, r) => notC(andC(notC(desugar(r)), notC(desugar(r))))
    | condExt(exps) => desugar(if-converter(exps))
    | appExt(f, a) => appC(f, desugar(a)) 
    | idExt(i) => idC(desugar(i))
  end
end

fun insert-if(sl :: S.S-Exp) -> S.S-Exp:
  doc: "a separate function for correctly outputing the behaviour of if in a single branch; supporting function for parse and parse-cond"
  cases (S.S-Exp) sl:
    | s-list(l) =>
      cases (List) l:
        | empty => S.s-list(empty)
        | link(f, r) => S.s-list(link(S.s-sym("if"), link(f,r)))
      end
    | else => raise("not s-list")
  end
where:
  insert-if(S.read-s-exp("()")) is S.read-s-exp("()")
  insert-if(S.read-s-exp("((> 1 2) 9)")) is S.read-s-exp("(if (> 1 2) 9)")
end

fun parse-cond(l :: List<S.S-Exp>) -> List<ExprExt>:
  doc: "returns a list of ExprExt from a list of S-Exp; mutually recursive with parse"
  cases (List) l:
    | empty => empty
    | link(f, r) => link(parse(insert-if(f)), parse-cond(r))
  end
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
            if (args.length() > 3):
              "too many arguments for if!"             
            else if args.length() == 3: 
              test = L.get(args, 0)
              consequent = L.get(args, 1)
              alternate = L.get(args, 2)
              ifExt(parse(test), parse(consequent), parse(alternate))
            else if args.length() == 2:
              test = L.get(args, 0)
              consequent = L.get(args, 1)
              ifExt2(parse(test), parse(consequent))
            end
          else if exp.s == "cond":
            cases (List) args:
              | empty => raise("")
              | link(f, r) => condExt(link(parse(insert-if(f)), parse-cond(r)))
            end          
          else if exp.s == ">":
            greaterExt(parse(L.get(args, 0)), parse(L.get(args, 1)))
          else if exp.s == "<":
            smallerExt(parse(L.get(args, 0)), parse(L.get(args, 1)))
          else if exp.s == "=":
            equalExt(parse(L.get(args, 0)), parse(L.get(args, 1)))
          else if exp.s == "not":
            notExt(parse(L.get(args, 0)))
          else if exp.s == "and":
            andExt(parse(L.get(args, 0)), parse(L.get(args, 1)))
          else if exp.s == "or":
            orExt(parse(L.get(args, 0)), parse(L.get(args, 1)))
          else:
            argL = L.get(args, 0)
            argR = L.get(args, 1)
            if exp.s == "+":
              plusExt(parse(argL), parse(argR))
            else if exp.s == "-":
              bminusExt(parse(argL), parse(argR))
            else if exp.s == "*":
              multExt(parse(argL), parse(argR))       
            end
          end       
      end
    | else => raise("not parsable")
  end
where:
  parse(S.read-s-exp("(if true (+ 5 6) (* 7 8))")) is ifExt(trueExt, plusExt(numExt(5), numExt(6)), multExt(numExt(7), numExt(8)))
  parse(S.read-s-exp("(if (> 3 4) (+ 5 6) (* 8 10))")) is ifExt(greaterExt(numExt(3), numExt(4)), plusExt(numExt(5), numExt(6)), multExt(numExt(8), numExt(10)))
  parse(S.read-s-exp("(if (< 3 4) (+ 5 (* 3 4)) 5)")) is ifExt(smallerExt(numExt(3), numExt(4)), plusExt(numExt(5), multExt(numExt(3), numExt(4))), numExt(5))
  parse(S.read-s-exp("(if (= 5 6) 5 (* -2 5))")) is ifExt(equalExt(numExt(5), numExt(6)), numExt(5), multExt(numExt(-2), numExt(5)))   
  parse(S.read-s-exp("(if (or (> 3 4) (> 4 3)) (+ 5 6) (* 9 10))")) is ifExt(orExt(greaterExt(numExt(3), numExt(4)), greaterExt(numExt(4), numExt(3))), plusExt(numExt(5), numExt(6)), multExt(numExt(9), numExt(10)))
  parse(S.read-s-exp("(if (and (> 5 4) (< 4 3)) 5 6)")) is ifExt(andExt(greaterExt(numExt(5), numExt(4)), smallerExt(numExt(4), numExt(3))), numExt(5), numExt(6))
  parse(S.read-s-exp("(if (not (= 3 9)) 7 10)")) is ifExt(notExt(equalExt(numExt(3), numExt(9))), numExt(7), numExt(10))
  parse(S.read-s-exp("(cond ((> 1 2) 9) ((= 5 6) 10) ((< 9 10) 11))")) is condExt([list: ifExt2(greaterExt(numExt(1), numExt(2)), numExt(9)), ifExt2(equalExt(numExt(5), numExt(6)), numExt(10)), ifExt2(smallerExt(numExt(9), numExt(10)), numExt(11))])
end

type Environment = List<Binding>
empty-env = empty
extend-env = link

fun arith-binop(
    op :: (Number, Number -> Number),
    l :: ExprC,
    r :: ExprC,
    env :: Environment,
    fds :: List<FunDefC>) -> Value: # recursive call unchanged
  lval = interp(l, env, fds)
  rval = interp(r, env, fds)
  if (lval.n > 128) or (rval.n > 128):
    raise('this only works for 8-bit arithmetic')
  else if is-numV(lval) and is-numV(rval):
    numV(op(lval.n, rval.n))
  else:
    raise('argument not a number')
  end
end 

fun get-fundef(name :: String, fds :: List<FunDefC>) -> FunDefC:
  cases (List<FunDefC>) fds:
    | empty => raise("couldn't find function")
    | link(f, r) =>
      if f.name == name:
        f
      else:
        get-fundef(name, r)
      end
  end
end

fun interp(e :: ExprC, env :: Environment, fds :: List<FunDefC>) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(n)
    | plusC(l, r) => arith-binop({(x, y): x + y}, l, r, env, fds)
    | multC(l, r) => arith-binop({(x, y): x * y}, l, r, env, fds)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | ifC(cnd, thn, els) =>
      ic = interp(cnd, env, fds)
      if is-boolV(ic):
        if ic.b:
          interp(thn, env, fds)
        else:
          interp(els, env, fds)
        end
      else:
        raise('not a boolean')
      end
    | greaterC(l, r) => 
      if interp(l, env, fds).n > interp(r, env, fds).n:
        boolV(true)
      else:
        boolV(false)
      end
    | smallerC(l, r) =>
      if interp(l, env, fds).n < interp(r, env, fds).n:
        boolV(true)
      else:
        boolV(false)
      end
    | equalC(l, r) =>
      if interp(l, env, fds).n == interp(r, env, fds).n:
        boolV(true)
      else:
        boolV(false)
      end
    | notC(shadow e) => 
      if interp(e, env, fds) == boolV(false):
        boolV(true)
      else:
        boolV(false)
      end
    | andC(l, r) =>
      if (interp(l, env, fds) == boolV(true)) and (interp(r, env, fds) == boolV(true)):
        boolV(true)
      else:
        boolV(false)
      end         
    | idC(s) => lookup(s, env)
    | appC(f, a) =>
      fd = get-fundef(f, fds)
      arg-val = interp(a, env, fds)
      interp(fd.body, extend-env(bind(fd.arg, arg-val), empty-env), fds)
  end
where:
  
  f1 = fdC("double", "x", plusC(idC("x"), idC("x")))
  f2 = fdC("quad", "x", appC("double", appC("double", idC("x"))))
  f3 = fdC("const5", "_", numC(5))
  f4 = fdC("f4", "x", ifC(idC("x"), numC(1), numC(0)))
  funs = [list: f1, f2, f3, f4]
  fun i(e): interp(e, empty-env, funs) end
  
  i(plusC(numC(5), appC("quad", numC(3)))) is numV(17)
  i(multC(appC("const5", numC(3)), numC(4))) is numV(20)
  i(plusC(numC(10), appC("const5", numC(10)))) is numV(10 + 5)
  i(plusC(numC(10), appC("double", plusC(numC(1), numC(2)))))
    is numV(10 + 3 + 3)
  i(desugar(parse(S.read-s-exp("(if (< 7 9) (* -2 5) (* 5 8))")))) is numV(-10)
  i(desugar(parse(S.read-s-exp("(if (= 5 6) 5 (* -2 5))")))) is numV(-10)
  i(desugar(parse(S.read-s-exp("(if (or (> 3 4) (> 4 3)) (+ 5 6) (* 9 10))")))) is numV(11)
  i(desugar(parse(S.read-s-exp("(if (not (= 3 4)) 7 10)")))) is numV(7)
  i(desugar(parse(S.read-s-exp("(if (and (> 5 4) (< 4 3)) 5 6)")))) is numV(6)
  i(desugar(parse(S.read-s-exp("(cond ((> 1 2) 9) ((= 5 6) 10) ((< 9 10) 11))")))) is numV(11)
  i(plusC(numC(10), appC("quad", plusC(numC(1), numC(2)))))
    is numV(10 + 3 + 3 + 3 + 3)
  interp(appC("f1", numC(3)), empty-env,
    [list: fdC("f1", "x", appC("f2", numC(4))),
           fdC("f2", "y", plusC(idC("x"), idC("y")))])
    raises "unbound identifier: x"
end

fun lookup(s :: String, env :: Environment) -> Value: #eager application
  cases (List<Binding>) env:
    | empty => raise(string-append("unbound identifier: ", s))
    | link(f, r) => 
      if f.name == s:
        f.value
      else:
        lookup(s, r)
      end
  end
end 

fun subst(w :: Value, at :: String, in :: ExprC) -> ExprC: #lazy application
  cases (ExprC) in:
    | numC(n) => in
    | plusC(l, r) => plusC(subst(w, at, l), subst(w, at, r))
    | multC(l, r) => multC(subst(w, at, l), subst(w, at, r))
    | trueC => trueC
    | falseC => falseC
    | ifC(cnd, thn, els) =>
      ifC(subst(w, at, cnd), subst(w, at, thn), subst(w, at, els))
    | appC(f, a) => appC(f, subst(w, at, a))
    | idC(s) =>
      if s == at:
        w
      else:
        in
      end
  end
end