# Assignment 6 - Pyret


# Family Tree
data Family-Tree:
  | no-parent
  | child(
      father :: Family-Tree,
      mother :: Family-Tree,
      name :: String,
      year :: Number,
      eyes :: String)
end

# Oldest Generation:
carl = child(no-parent, no-parent, "Carl", 1926, "green")
bettina = child(no-parent, no-parent, "Bettina", 1926, "green")

# Middle Generation:
adam = child(carl, bettina, "Adam", 1950, "hazel")
dave = child(carl, bettina, "Dave", 1955, "black")
eva = child(carl, bettina, "Eva", 1965, "blue")
fred = child(no-parent, no-parent, "Fred", 1966, "pink")

# Youngest Generation:
gustav = child(fred, eva, "Gustav", 1988, "brown")

# Question 1
fun len-tree(ft :: Family-Tree) -> Number:
  doc: "returns the length of a tree"
  cases (Family-Tree) ft:
    | no-parent => 0
    | child(father, mother, name, year, eyes) =>
      1 + len-tree(father) + len-tree(mother)
  end
where:
  len-tree(no-parent) is 0
  len-tree(carl) is 1
  len-tree(eva) is 3
end 

fun sum-age-tree(ft :: Family-Tree) -> Number:
  doc: "returns the sum of ages of family members in a tree"
  cases (Family-Tree) ft:
    | no-parent => 0
    | child(father, mother, name, year, eyes) =>
      (2017 - year) + sum-age-tree(father) + sum-age-tree(mother)
  end 
where:
  sum-age-tree(no-parent) is 0
  sum-age-tree(eva) is 234
  sum-age-tree(carl) is 91
end

fun average-age-tree(ft :: Family-Tree) -> Number:
  doc: "returns the average age of all members in a tree"
  sum-age-tree(ft) / len-tree(ft)
where:
  average-age-tree(no-parent) raises ""
  average-age-tree(gustav) is 62.8
  average-age-tree(carl) is 91
  average-age-tree(eva) is 78
end 

# Question 2
fun depth-tree(ft :: Family-Tree) -> Number:
  doc: "returns the length of the longest branch starting from the tree node"
  cases (Family-Tree) ft:
    | no-parent => 0
    | child(father, mother, name, year, eyes) => 
      if (depth-tree(father) > depth-tree(mother)):
        1 + depth-tree(father)
      else:
        1 + depth-tree(mother)
      end
  end
where:
  depth-tree(no-parent) is 0
  depth-tree(adam) is 2
  depth-tree(carl) is 1
  depth-tree(eva) is 2
  depth-tree(gustav) is 3
end

# Question 3
fun len-forest(ff :: List<Family-Tree>) -> Number:
  doc: "returns the sum of tree lengths in a family forest"
  cases (List) ff:
    | empty => 0
    | link(first, rest) =>
      len-tree(first) + len-forest(rest)
  end
where:
  len-forest(empty) is 0
  len-forest([list: carl, bettina]) is 2
  len-forest([list: eva, adam]) is 6
end

fun sum-age-forest(ff :: List<Family-Tree>) -> Number:
  doc: "returns the sum of ages of all family members in a family forest"
  cases (List) ff:
    | empty => 0
    | link(first, rest) =>
      sum-age-tree(first) + sum-age-forest(rest)
  end
where:
  sum-age-forest(empty) is 0
  sum-age-forest([list: carl, bettina]) is 182
  sum-age-forest([list: carl, eva]) is 325
end

fun average-age-forest(ff :: List<Family-Tree>) -> Number:
  doc: "returns the average age of all members in a family forest"
      sum-age-forest(ff) / len-forest(ff)
where:
  average-age-forest(empty) raises ""
  average-age-forest([list: carl, bettina]) is 91
  average-age-forest([list: eva, adam]) is 80.5
end 

# Question 4
fun depth-forest(ff :: List<Family-Tree>) -> Number:
  doc: "returns the length of the longest branch starting from any tree in a forest"
  cases (List) ff:
    | empty => raise("")
    | link(first, rest) =>
      cases (List) rest:
        | empty => depth-tree(first)
        | else => num-max(depth-tree(first), depth-forest(rest))
      end            
  end   
where:
  depth-forest(empty) raises ""
  depth-forest([list: carl, gustav]) is 3
  depth-forest([list: adam, dave]) is 2
  depth-forest([list: dave, gustav]) is 3
end

# S-Expression
data S-Exp:
  | s-num(n :: Number)
  | s-str(s :: String)
  | s-sym(s :: String)
  | s-list(exps :: List<S-Exp>)
end

# Question 5
fun depth-sexp(sexp :: S-Exp) -> Number:
  doc: "returns the depth of an s-expression"
  cases (S-Exp) sexp:
    | s-num(n :: Number) => 0
    | s-str(s :: String) => 0
    | s-sym(s :: String) => 0
    | s-list(exps :: List<S-Exp>) =>
      cases (List<S-Exp>) exps:
        | empty => 0
        | link(first :: S-Exp, rest :: List<S-Exp>) => num-max(1 + depth-sexp(first), depth-sexp(s-list(rest)))
      end
  end
where:
  depth-sexp(s-num(5)) is 0
  depth-sexp(s-list(empty)) is 0
  depth-sexp(s-str("a")) is 0
  depth-sexp(s-sym("a")) is 0
  depth-sexp(s-list([list: s-sym("a"), s-list([list: s-str("b"), s-sym("c")])])) is 2
  depth-sexp(s-list([list: s-num(6), s-list([list: s-sym("9"), s-list([list: s-str("ab")])])])) is 3 
  depth-sexp(s-list([list: s-num(3), s-num(4), s-list([list: s-num(2), s-num(6), s-list([list: s-list([list: s-str("a")])])])])) is 4
end

# Question 6
fun replace-sym-in-sl(exps :: List<S-Exp>, sym1 :: String, sym2 :: String):
  doc: "returns s-list obtained by replacing all occurrences of the first symbol in the input s-list by the second symbol"
  cases (List<S-Exp>) exps:
    | empty => empty
    | link(first, rest) => 
      link(replace-sym-in-exp(first, sym1, sym2), replace-sym-in-sl(rest, sym1, sym2))
  end
where:
  replace-sym-in-sl(empty, "b", "A") is empty
  replace-sym-in-sl([list: s-sym("b"), s-num(6), s-list([list: s-sym("b")])], "b", "c") is [list: s-sym("c"), s-num(6), s-list([list: s-sym("c")])]
end

fun replace-sym-in-exp(sexp :: S-Exp, sym1 :: String, sym2 :: String) -> S-Exp:
  doc: "returns the s-expression obtained by replacing all occurrences of the first symbol in the input s-expression by the second symbol"
  cases (S-Exp) sexp:
    | s-num(n :: Number) => s-num(n)
    | s-str(s :: String) => s-str(s)
    | s-sym(s :: String) => 
      if (s == sym1):
        s-sym(sym2)
      else:
        s-sym(s)
      end
    | s-list(exps :: List<S-Exp>) => s-list(replace-sym-in-sl(exps, sym1, sym2))      
  end
where:
  replace-sym-in-exp(s-str("\"a\""), "a", "b") is s-str("\"a\"")
  replace-sym-in-exp(s-num(6), "6", "4") is s-num(6)
  replace-sym-in-exp(s-list(empty), "a", "b") is s-list(empty)
  replace-sym-in-exp(s-sym("b"), "b", "a") is s-sym("a")
  replace-sym-in-exp(s-list([list: s-sym("a"), s-num(3), s-sym("t"), s-sym("b")]), "t", "b") is s-list([list: s-sym("a"), s-num(3), s-sym("b"), s-sym("b")])
  replace-sym-in-exp(s-list([list: s-sym("a"), s-list([list: s-sym("a"), s-sym("b"), s-sym("c")])]), "a", "c") is s-list([list: s-sym("c"), s-list([list: s-sym("c"), s-sym("b"), s-sym("c")])])
end

# Binary Tree
data BinTree:
  | leaf # should be "null" imo
  | node(value :: Number, left :: BinTree, right :: BinTree)
end
t1 = node(2, leaf, leaf)
t2 = node(9, leaf, leaf)
t3 = node(7, leaf, leaf)
t4 = node(15, leaf, leaf)
t5 = node(8, t1, t2)
t6 = node(14, t3, t4)
t7 = node(12, t5, t6)

# Question 7
fun check-num-bin(tree :: BinTree, num :: Number) -> Boolean:
  doc: "checks whether a number is in a binary tree"
  cases (BinTree) tree:
    | leaf => false
    | node(value, left, right) =>
      (value == num) or check-num-bin(left, num) or check-num-bin(right, num)
  end
where:
  check-num-bin(leaf, 8) is false
  check-num-bin(t3, 6) is false
  check-num-bin(t7, 14) is true
  check-num-bin(t6, 7) is true
end

# Question 8
fun insert-num-bin(tree :: BinTree, num :: Number) -> BinTree:
  doc: "returns a BinTree containing the elements of the input BinTree and the Number"
  cases (BinTree) tree:
    | leaf => node(num, leaf, leaf)
    | node(value, left, right) =>
      if (value == num):
        tree
      else if (value > num):
        node(value, insert-num-bin(left, num), right)
      else:
        node(value, left, insert-num-bin(right, num))
      end
  end
where:
  insert-num-bin(leaf, 3) is node(3, leaf, leaf)
  insert-num-bin(t1, 2) is node(2, leaf, leaf)
  insert-num-bin(t1, 8) is node(2, leaf, node(8, leaf, leaf))
  insert-num-bin(t2, 11) is node(9, leaf, node(11, leaf, leaf))
  insert-num-bin(t5, 6) is node(8, node(2, leaf, node(6, leaf, leaf)), t2)
end
  
# Question 9
fun make-tree(l :: List<Number>) -> BinTree: 
  doc: "returns a tree from the given list of numbers"
  cases (List) l:
    | empty => leaf
    | link(first, rest) =>
      insert-num-bin(make-tree(rest), first)      
  end 
where:
  make-tree(empty) is leaf
  make-tree([list: 1, 2, 3]) is node(3, node(2, node(1, leaf, leaf), leaf), leaf)
  make-tree([list: 2, 1, 3, 5]) is node(5, node(3, node(1, leaf, node(2, leaf, leaf)), leaf), leaf)
  make-tree([list: 9, 5, 3, 4]) is node(4, node(3, leaf, leaf), node(5, leaf, node(9, leaf, leaf)))
  make-tree([list: 8, 4, 9, 6, 7]) is node(7, node(6, node(4, leaf, leaf), leaf), node(9, node(8, leaf, leaf), leaf))
end

# Pairs
data Pair:
  | p(x :: Number, y :: Number)
end

#| map :: (
  f :: (a -> b)
  l :: List<a>
  )
   -> List<b> |#

# Question 10
fun list-of-pairs(l1 :: List<Number>, l2 :: List<Number>) -> List<Pair>:
  doc: "returns all possible pairs between 2 lists of nums; 1st elem from 1st list and 2nd elem from 2nd list"
  cases (List) l2:
    | empty => empty
    | link(first, rest) =>
      map(lam(x): p(x, first) end, l1) + list-of-pairs(l1, rest) 
  end
where:
  list-of-pairs(empty, empty) is empty
  list-of-pairs([list: 1, 3], empty) is empty    
  list-of-pairs(empty, [list: 2, 1]) is empty      
  list-of-pairs([list: 1, 3], [list: 5]) is [list: p(1, 5), p(3, 5)]
  list-of-pairs([list: 2, 3, 4], [list: 1, 0, 6]) is [list: p(2, 1), p(3, 1), p(4, 1), p(2, 0), p(3, 0), p(4, 0), p(2, 6), p(3, 6), p(4, 6)]
  list-of-pairs([list: 2], [list: 1, 2]) is [list: p(2, 1), p(2,2)]
  list-of-pairs([list: 1, 2, 3], [list: 4, 5]) is [list: p(1, 4), p(2, 4), p(3, 4), p(1, 5), p(2, 5), p(3, 5)]
end
      
