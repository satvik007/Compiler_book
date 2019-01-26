type id = string;

datatype binop = Plus | Minus | Times | Div;

datatype stm = CompoundStm of stm * stm
            | AssignStm of id * exp
            | PrintStm of exp list
    and exp  = IdExp of id
            | NumExp of int
            | OpExp of exp * binop * exp
            | EseqExp of stm * exp

val prog = 
    CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)), 
    CompoundStm(AssignStm("b",
        EseqExp(PrintStm [IdExp "a", OpExp(IdExp "a", Minus,
                                                NumExp 1)],
            OpExp(NumExp 10, Times, IdExp "a"))),
    PrintStm [IdExp "b"]));

fun max (x : int, y : int) = if (x >= y) then x
                             else y;

fun maxargs x = 
    let fun stmmaxargs y = 
            case y of 
                CompoundStm(a, b) => max(stmmaxargs a, stmmaxargs b)
                | AssignStm(a, b) => expmaxargs b
                | PrintStm(x) => max(length x, openup x)
        and expmaxargs y = 
            case y of 
                EseqExp(a, b) => stmmaxargs(a)
                | _ => 0
        and openup y = 
            case y of 
                (t :: ts) => max (expmaxargs t, openup ts)
                | _ => 0
    in 
        stmmaxargs x
    end;

maxargs prog;

