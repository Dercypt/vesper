type t =
  | Op_Const of int
  | Op_Add
  | Op_Sub
  | Op_Mul
  | Op_Div
  | Op_Mod
  | Op_Neg
  | Op_Not
  | Op_Eq
  | Op_Neq
  | Op_Lt
  | Op_Le
  | Op_Gt
  | Op_Ge
  | Op_GetLocal of int
  | Op_SetLocal of int
  | Op_GetGlobal of string
  | Op_DefGlobal of string
  | Op_SetGlobal of string
  | Op_Jump of int
  | Op_JumpIfFalse of int
  | Op_Call of int
  | Op_Return
  | Op_Halt
  | Op_Pop
  | Op_Unit
  | Op_Closure of int
  | Op_TieRec of string

let to_string = function
  | Op_Const idx -> Printf.sprintf "Op_Const %d" idx
  | Op_Add -> "Op_Add"
  | Op_Sub -> "Op_Sub"
  | Op_Mul -> "Op_Mul"
  | Op_Div -> "Op_Div"
  | Op_Mod -> "Op_Mod"
  | Op_Neg -> "Op_Neg"
  | Op_Not -> "Op_Not"
  | Op_Eq -> "Op_Eq"
  | Op_Neq -> "Op_Neq"
  | Op_Lt -> "Op_Lt"
  | Op_Le -> "Op_Le"
  | Op_Gt -> "Op_Gt"
  | Op_Ge -> "Op_Ge"
  | Op_GetLocal slot -> Printf.sprintf "Op_GetLocal %d" slot
  | Op_SetLocal slot -> Printf.sprintf "Op_SetLocal %d" slot
  | Op_GetGlobal name -> Printf.sprintf "Op_GetGlobal %s" name
  | Op_DefGlobal name -> Printf.sprintf "Op_DefGlobal %s" name
  | Op_SetGlobal name -> Printf.sprintf "Op_SetGlobal %s" name
  | Op_Jump target -> Printf.sprintf "Op_Jump %d" target
  | Op_JumpIfFalse target -> Printf.sprintf "Op_JumpIfFalse %d" target
  | Op_Call arity -> Printf.sprintf "Op_Call %d" arity
  | Op_Return -> "Op_Return"
  | Op_Halt -> "Op_Halt"
  | Op_Pop -> "Op_Pop"
  | Op_Unit -> "Op_Unit"
  | Op_Closure idx -> Printf.sprintf "Op_Closure %d" idx
  | Op_TieRec name -> Printf.sprintf "Op_TieRec %s" name

let pp fmt op = Format.pp_print_string fmt (to_string op)

let equal op1 op2 =
  match (op1, op2) with
  | Op_Const i1, Op_Const i2 -> i1 = i2
  | Op_Add, Op_Add
  | Op_Sub, Op_Sub
  | Op_Mul, Op_Mul
  | Op_Div, Op_Div
  | Op_Mod, Op_Mod
  | Op_Neg, Op_Neg
  | Op_Not, Op_Not
  | Op_Eq, Op_Eq
  | Op_Neq, Op_Neq
  | Op_Lt, Op_Lt
  | Op_Le, Op_Le
  | Op_Gt, Op_Gt
  | Op_Ge, Op_Ge ->
      true
  | Op_GetLocal s1, Op_GetLocal s2 -> s1 = s2
  | Op_SetLocal s1, Op_SetLocal s2 -> s1 = s2
  | Op_GetGlobal g1, Op_GetGlobal g2 -> String.equal g1 g2
  | Op_DefGlobal g1, Op_DefGlobal g2 -> String.equal g1 g2
  | Op_SetGlobal g1, Op_SetGlobal g2 -> String.equal g1 g2
  | Op_Jump t1, Op_Jump t2 -> t1 = t2
  | Op_JumpIfFalse t1, Op_JumpIfFalse t2 -> t1 = t2
  | Op_Call a1, Op_Call a2 -> a1 = a2
  | Op_Return, Op_Return | Op_Halt, Op_Halt | Op_Pop, Op_Pop | Op_Unit, Op_Unit -> true
  | Op_Closure c1, Op_Closure c2 -> c1 = c2
  | Op_TieRec n1, Op_TieRec n2 -> String.equal n1 n2
  | _ -> false
