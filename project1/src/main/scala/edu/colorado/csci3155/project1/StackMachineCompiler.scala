package edu.colorado.csci3155.project1

object StackMachineCompiler {

    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
    	var stackL = List[StackMachineInstruction]()
    	e match {
	    	case Const(x) => PushI(x) :: stackL
	    	case Plus(e1, e2) => {
	    		stackL = AddI :: stackL
	    		stackL = compileToStackMachineCode(e2) ++ stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Minus(e1, e2) => {
	    		stackL = SubI :: stackL
	    		stackL = compileToStackMachineCode(e2) ++ stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Mult(e1, e2) => {
	    		stackL = MultI :: stackL
	    		stackL = compileToStackMachineCode(e2) ++ stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Div(e1, e2) => {
	    		stackL = DivI :: stackL
	    		stackL = compileToStackMachineCode(e2) ++ stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Exp(e1) => {
	    		stackL = ExpI :: stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Log(e1) => {
	    		stackL = LogI :: stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Sine(e1) => {
	    		stackL = SinI :: stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    	case Cosine(e1) => {
	    		stackL = CosI :: stackL
	    		compileToStackMachineCode(e1) ++ stackL
	    	}
	    }
    }
}

