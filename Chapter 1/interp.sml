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

fun interp x = 
    let fun interpStm y = 
        case y of 
            (CompoundStm(a, b), tab) => (let val utab = interpStm(a, tab) 
                                        in interpStm(b, utab) 
                                        end)
            | (AssignStm(a, b), tab) => (let val ret = interpExp(b, tab)
                                            in update (a, ret)
                                         end)
            | (PrintStm(x), tab) => printHelper (x, tab)

        and interpExp y =
        case y of 
            (IdExp (a), tab) => (eval (a, tab), tab)
            | (NumExp (a), tab) => (a, tab)
            | (OpExp (a, b, c), tab) => (case b of 
                Plus => (let val fv = interpExp (a, tab)
                         in (let val sv = interpExp (a, #2(fv))
                            in (#1(fv) + #1(sv), #2(sv))
                            end)
                        end)   
                | Minus => (let val fv = interpExp (a, tab)
                         in (let val sv = interpExp (a, #2(fv))
                            in (#1(fv) - #1(sv), #2(sv))
                            end)
                        end)  
                | Times => (let val fv = interpExp (a, tab)
                         in (let val sv = interpExp (a, #2(fv))
                            in (#1(fv) * #1(sv), #2(sv))
                            end)
                        end)
                | Div => (let val fv = interpExp (a, tab)
                         in (let val sv = interpExp (a, #2(fv))
                            in (#1(fv) div #1(sv), #2(sv))
                            end)
                        end) 
            )  
            | (EseqExp (a, b), tab) => (let val utab = interpStm (a, tab)
                                        in interpExp (b, utab)
                                        end)  
        and update (a, ret) = ((a, #1(ret)) :: #2(ret))

        and printHelper y = 
        case y of 
            ((x :: xs), tab) => (let val ret = interpExp (x, tab)
                                in
                                    (
                                        print (Int.toString (#1(ret)) ^ "\n");
                                        printHelper (xs, #2(ret))
                                    )
                                end)
            | (_, tab) => tab
        
        and eval y =
        case y of 
            (a, (x :: xs)) => if (#1(x) = a) then #2(x) else eval (a, xs)
            | (a, _) => 0
            
    in
        interpStm (x, [])
    end;

interp prog;

