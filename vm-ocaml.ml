open Core.Std

type registers = {
  mutable a: int;
  mutable b: int;
  mutable c: int;
  mutable d: int;
  mutable e: int;
  mutable f: int
}

type register =
  | A
  | B
  | C
  | D
  | E
  | F

type instruction =
  | F of int * instruction list
  | RUN of int
  | PSH of int
  | SET of register
  | GET of register
  | ADD
  | MUL
  | POP
  | HLT


let stack = Stack.create()
let registers = {a=0;b=0;c=0;d=0;e=0;f=0}
let functions = [|[];[];[];[];[];[]|] (* yay for empty functions *)

let setRegister x v =
  (* real ugly, hopefuly will learn a better way to do it soon *)
  match x with
  | A -> registers.a <- v
  | B -> registers.b <- v
  | C -> registers.c <- v
  | D -> registers.d <- v
  | E -> registers.e <- v
  | F -> registers.f <- v

let getRegister x =
  match x with
  | A -> registers.a
  | B -> registers.b
  | C -> registers.c
  | D -> registers.d
  | E -> registers.e
  | F -> registers.f

let set x =
  let opt = Stack.pop stack in
  match opt with
  | None -> assert false (* memory error should be handled *)
  | Some v -> setRegister x v

let get x =
  Stack.push stack (getRegister x)

let pop stack =
  let opt = Stack.pop stack in
  match opt with
  | None -> printf "nothing found\n"
  | Some x -> printf "popped %i\n" x

let rec reduceStack stack result f =
  let i = Stack.pop stack in
  match i with
  | None -> Stack.push stack result
  | Some x -> reduceStack stack (f result x) f

let saveFunction i l = functions.(i) <- l

let rec run prog =
  match prog with
  | [] -> printf "no more instruction\n"
  | HLT :: _ -> printf "execution stopped\n"
  | F (i, l) :: rest -> saveFunction i l; run rest
  | RUN i :: rest -> run functions.(i); run rest
  | PSH x :: rest -> Stack.push stack x; run rest
  | SET x :: rest -> set x; run rest
  | GET x :: rest -> get x; run rest
  | POP :: rest -> pop stack; run rest
  | ADD :: rest -> reduceStack stack 0 (+); run rest
  | MUL :: rest -> reduceStack stack 1 ( * ); run rest

(* here's the tiny program *)
let program = [
  F (0, [PSH 3; PSH 5]);
  RUN 0;
  RUN 0;
  PSH 3;
  PSH 5;
  MUL;
  SET A;
  PSH 6;
  PSH 4;
  ADD;
  SET B;
  PSH 3;
  GET A;
  GET B;
  ADD;
  POP;
  HLT
]

let () =
  run program