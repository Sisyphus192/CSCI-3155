package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */

    def do_una_op(stackL: List[Double], una: (Double) => Double): List[Double] = stackL match {
      case x :: rest => una(x) :: rest
      case Nil => throw new IllegalArgumentException("Stack is Empty")
    }
    def do_bin_op(stackL: List[Double], bop: (Double, Double) => Double): List[Double] = stackL match {
      case x :: y :: rest => bop(y,x) :: rest
      case Nil => throw new IllegalArgumentException("Stack is Empty")
      case _ => throw new IllegalArgumentException("Insufficient number of elements in stack for operation")
    }

    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] =
        ins match {
          case PushI(d) => d :: stack
          case PopI => stack.init
          case AddI => do_bin_op(stack, (y,x) => y + x)
          case SubI => do_bin_op(stack, (y,x) => y - x)
          case MultI => do_bin_op(stack, (y,x) => y * x)
          case DivI => do_bin_op(stack, (y,x) => {
              if (x == 0){
                throw new IllegalArgumentException("Division by 0")
              }
              else {
                y / x
              }
            }
          )
          case LogI => do_una_op(stack, (x) => {
              if (x > 0){
                scala.math.log(x)
              }
              else {
                throw new IllegalArgumentException("Cannot take log of num <= 0")
              }
            }
          )
          case ExpI => do_una_op(stack, (x) => scala.math.exp(x))
          case SinI => do_una_op(stack, (x) => scala.math.sin(x))
          case CosI => do_una_op(stack, (x) => scala.math.cos(x))
        }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double =
        instructionList.foldLeft(Nil: List[Double])(emulateSingleInstruction)(0) 
}
