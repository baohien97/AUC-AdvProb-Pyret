fun my-sum(l :: List<Number>) -> Number:
  doc: "return sum of all n0"
  cases(List) l:
    | empty => 0
    | link(f, r) => f + my-sum(r)
  end
where:
  my-sum(empty) is 0
  my-sum([list: 1, 2, 3]) is 6
end

fun my-len(l :: List<Any>) -> Number:
  doc: "return length of list"
  cases(List) l:
    | empty => 0
    | link(f, r) => 1 + my-len(r)
  end
where:
  my-len([list: ]) is 0
  my-len([list: 1, 2, 3]) is 3
end

fun my-max(l :: List<Number>) -> Number:
  doc: "Return the biggest number"
  cases (List) l:
    | empty      => raise("")
    | link(f, r) => 
      cases (List) r:
        | empty => f
        | else => num-max(f, my-max(r))
      end
  end
where: 
      my-max([list: 2, 9, 1]) is 9
      my-max(empty) raises ""
      my-max([list: 19]) is 19
end 
  
fun my-str-len(str :: List<String>) -> Number:
  doc: "convert each string to a number representing its length"
  cases (List) str:
    | empty => 0
    | link(f, r) => 1 + my-str-len(r)
  end    
where: 
  my-str-len([list: "fat", "cat"]) is 2
  my-str-len([list: "love", "to", "eat", "chicken"]) is 4
  my-str-len(empty) is 0
end 

fun my-pos-num(l :: List<Number>) -> List:
  doc: "Return the positive numbers in a list"
  cases (List) l:
    | empty => empty
    | link(f, r) =>
      if f > 0:
        link(f, my-pos-num(r))
      else:
        my-pos-num(r)
      end
  end      
where:
  my-pos-num([list: -1, 2, 5, 0]) is [list: 2, 5]
  my-pos-num([list: -4]) is empty
  my-pos-num(empty) is empty
end

fun my-alternating(l :: List<Any>) -> List<Any>:
  doc: "returns a list of everyother item"
  cases (List) l:
    | empty => empty
    | link(f, r) => 
      cases (List) r:
        | empty => [list: f]
        | link(f2, r2) => link(f, my-alternating(r2))
      end
  end
          
where:
  my-alternating(empty) is empty
  my-alternating([list: 9]) is [list: 9]
  my-alternating([list: 5, 9]) is [list: 5]
  my-alternating([list: 0, 1, 5, 6, 9]) is [list: 0, 5, 9]
  my-alternating([list: "cat", "dog", "pig", "fish"]) is [list: "cat", "pig"]
end 

```fun my-running-sum(l :: List<Number>) -> List<Number>:
  doc: "Given a list of numbers, replace every element with the running sum"
  cases (List) l:
where: 
  my-running-sum([list: 2, 4, 9]) is [list: 2, 6, 15]
  my-running-sum(empty) is empty
  my-running-sum([list: 2]) is [list: 2]
end``` 

fun my-avg(l :: List<Number>) -> Number:
  doc: "Return the average of all the n0 in a list"
  my-sum(l) / my-len(l)
where:
  my-avg([list: 9, 5, 6]) is 20/3
  my-avg([list: 5]) is 5
end 
    
  