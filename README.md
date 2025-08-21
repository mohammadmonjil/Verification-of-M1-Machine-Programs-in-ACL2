# Verification of M1-Machine Programs in ACL2

## Overview
This project verifies programs written for the **M1-machine** using the ACL2 theorem prover.  
The **main goal** was to write and verify a multiplication program. Two additional stretch goals were also attempted: division and greatest common divisor (GCD).

---

## Programs Verified

- **Multiplication (Main Goal)**  
  Implemented by repeated addition. The multiplicand is used as a loop counter, and the multiplier is added to an accumulator until the counter reaches zero.  
  - Input: (n0, n1)  
  - Output: (0, n1, n0*n1)  

- **Division (Stretch Goal 1)**  
  Implemented by repeated subtraction until the dividend is less than the divisor. The remainder and quotient are stored in separate variables.  
  - Input: (n0, n1)  
  - Output: (n0 mod n1, n1, floor(n0/n1))  

- **GCD (Stretch Goal 2)**  
  Implemented with nested loops where the inner loop calculates modulus and the outer loop performs the Euclidean algorithm.  
  - Input: (n0, n1)  
  - Output: (gcd(n0,n1), 0, 0)  

---

## Verification Approach

- Modeled the **M1-machine state** with program counter, local variables, and stack.  
- Defined loop clocks (`lp-clk`) to capture the number of steps per iteration.  
- Proved **lemmas for symbolic computation** of the M1-machine.  
- Verified **loop correctness** using induction on the loop counter(s).  
- Proved **final theorems** that the machine state after execution matches the expected final state.

---

## Running the Project

- Tested in **ACL2 8.6**.  
- Source files are provided in Lisp:  
  - `project_main_goal_multiplication.lisp`  
  - `project_strech_goal_division.lisp`  
  - `project_strech_goal_gcdx.lisp`  
- Load these files in ACL2 to explore the programs and their proofs.
---
