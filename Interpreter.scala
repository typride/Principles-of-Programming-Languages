package edu.colorado.csci3155.project2

/*
   A Lettuce interpreter with evalExpr function that has missing cases to be handled. See TODOs below.
 */

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) => {
            val x = evalExpr(l, env)
            x match {
                case NumValue(x) => { val polygon = new Polygon( List( (0, 0 ) , ( x, 0 )))
                    val newc = new MyCanvas(List(polygon))
                    
                    FigValue(newc)
                }
            }
        }  //TODO: Handle a line object
        case EquiTriangle(sideLength) => {
            val x = evalExpr(sideLength, env)
            x match {
                case NumValue(x) => {
                    val polygon = new Polygon(List((0,0), (x,0), (x/2,math.sqrt(3)*x/2)))
                    val newc = new MyCanvas(List(polygon))
                    FigValue(newc)
                }

            }
        }// TODO: Handle Equilateral Triangle
        case Rectangle(sideLength) => {val x = evalExpr(sideLength, env)
            v match {case NumValue(x) => {
                    val rect = new Polygon(List((0,0), (0,x), (x,x), (x,0)))
                    val newRec = new MyCanvas(List(rect))
                    FigValue(newRec)
                }
            }
        } // TODO: Handle square given the side length
        case Circle(rad) => {val x = evalExpr(rad, env)
            x match {case NumValue(x) => {
                    val newcircle  = new MyCircle((x, x), x)
                    val newc = new MyCanvas(List(newcircle))
                    FigValue(newc)
                }
                case _ => {throw new IllegalArgumentException("not ok")}
            }
        } //TODO: Handle circle
        case Plus (e1, e2) => {
            val x = evalExpr(e1, env)
            x match {case FigValue(x) => {
                    val y = evalExpr(e2, env)
                    y match {case FigValue(y) => {
                            FigValue(x.overlap(y))
                        }
                    }
                }
            }
        } // TODO: Handle addition of numbers or figures
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)


        case Mult(e1, e2) => {
            val x = evalExpr(e1, env)
            x match { case FigValue(x) => {val y = evalExpr(e2, env)
                    y match {case FigValue(y) => { FigValue(v1.placeRight(y))}
                    }
                }

            }
        } // TODO: Handle multiplication of numbers or figures
        case Div(e1, e2) => {
            val x = evalExpr(e1, env)
            x match {case FigValue(x) => {val y = evalExpr(e2, env)
                    y match {
                        case FigValue(y) => {FigValue(y.placeTop(x))}
                        case NumValue(y) => {FigValue(x.rotate(y))}
                    }
                case _ => {throw new IllegalArgumentException("not okay")}
            }
        } // TODO: Handle division
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => {Closure(x,e,env)}
        case LetRec(f, x, e1, e2) => {val newEnv = ExtendREC(f,x,e1,env) evalExpr(e2,newEnv)}// TODO: Handle recursive functions -- look at Environment.scala
        case FunCall(fCallExpr, arg) => {
            val n = evalExpr(fCallExpr, env)
            val n2 = evalExpr(arg, env)
            n match { case Closure(x, closure_ex, closed_env) => { val new_env = Extend(x, n2, closed_env)
                    evalExpr(closure_ex, new_env)
                }
            }
        } 
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
