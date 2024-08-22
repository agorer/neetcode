module Stack = struct
  type t = {
    min: int;
    stack: int list;
  }
  
  let empty = {
    min=Int.max_int;
    stack=[]
  }

  exception EmptyStack

  let push stack elt =
    {
      min=if elt < stack.min then elt else stack.min;
      stack=elt :: stack.stack
    }

  let pop stack =
    let aux' stack =
      match stack with
      | [] -> raise EmptyStack
      | elt :: tail -> elt, tail
    in
    let elt, tail = aux' stack.stack in
    { stack with stack=tail }, elt
                       
  let top stack =
    match stack.stack with
    | [] -> None
    | elt :: _ -> Some elt

  let min stack =
    stack.min
end

