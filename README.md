# Floating-Point-ALU
SystemVerilog Implementation of an IEEE 754 floating point unit.

The floating point unit has modules for addition/subtraction, multiplication, fused multiply-add, division, square root, reciprocal, inverse square root, max/min, and comparison. 

It utilizes single precision (32-bit) floating point arithmetic and all modules are pipelined to allow for usage in data processing pipelines. The modules are optimized to achieve xxx MHz for an Artix 7 FPGA on a Zybo Z7 7020. Details on the specifics of the modules' features and pipeline stages are to follow. 

## Addition/Subtraction $in_1 + in_2$

Addition and subtraction share a pipeline that decides on the operation based on the signs of the inputs. [Kogge-Stone Adders](https://github.com/dantonklein/Advanced-Adders-and-Multipliers) were utilized for addition/subtraction to optimize the design for speed. It takes 5 cycles for completion, with those stages being:
<ol>
  <li>Input Handling</li>
  <li>Alignment</li>
  <li>Addition/Subtraction</li>
  <li>Normalization</li>
  <li>Rounding</li>
</ol> 

## Multiplication $in_1 * in_2$

Multiplication uses a [24-bit Dadda Multiplier](https://github.com/dantonklein/Advanced-Adders-and-Multipliers) for the mantissa multiplication. It takes 4 cycles for completion, with thoses stages being:

<ol>
  <li>Input Handling</li>
  <li>Multiplication 1 and Exponent Handling</li>
  <li>Multiplication 2</li>
  <li>Normalization and Rounding</li>
</ol> 

## Fused Multiply-Add $(in_1 * in_2) + in_3$

Fused multiply-add combines functionality of both addition/subtraction and multiplication into a pipeline that only requires 1 input handling stage and 1 rounding stage. It takes 7 cycles for completion, with those stages being:

<ol>
  <li>Input Handling</li>
  <li>Multiplication 1 and Exponent Handling</li>
  <li>Multiplication 2</li>
  <li>Normalization and Alignment</li>
  <li>Addition/Subtraction</li>
  <li>Normalization</li>
  <li>Rounding</li>
</ol> 

## Reciprocal $\frac{1}{in}$

While division would be the next logical operation to list, I elected to mention Reciprocal next since I utilize its mantissa calculation in my division module. The mantissa for the reciprocal is calculated with two iterations of Newton's Method, with a 256-entry look up table providing the initial guess. The formula for the Newton's Method iteration is $x_{n+1} = x_{n}(2-ax_{n})$, where a is the input and $x_n$ is the guess/previous iteration. It takes 13 cycles for completion, with those stages being:

<ol>
  <li>Input Handling</li>
  <li>Look-Up Table Read and Exponent Handling 1</li>
  <li>Newton's Iteration 1 and Exponent Handling 2</li>
  <li>Newton's Iteration 1</li>
  <li>Newton's Iteration 1</li>
  <li>Newton's Iteration 1</li>
  <li>Newton's Iteration 1</li>
  <li>Newton's Iteration 2</li>
  <li>Newton's Iteration 2</li>
  <li>Newton's Iteration 2</li>
  <li>Newton's Iteration 2</li>
  <li>Newton's Iteration 2</li>
  <li>Rounding</li>
</ol> 

## Divison $\frac{in_1}{in_2}$

Division utilizes the module that calculates the reciprocal mantissa for calculating $\frac{1}{in2}$ followed by multiplication with in1. It takes 15 cycles for completion.

## Inverse Square Root $\frac{1}{\sqrt{in}}$

Similar to reciprocal, inverse square root is calculated utilizing a look up table and two Newton's Method iterations. It is also later used in the Square Root module. The reason why inverse square root is the calculation done with Newton's Method rather than Square Root is that it features no division in its formula, which is $x_{n+1} = \frac{1}{2}x_{n}(3-(a{x_n}^2))$. It takes 17 cycles for completion.

## Square Root $\sqrt{in}$

Square root utilizies the inverse square root module for calculating the reciprocal followed by a multiplication with the input since $\frac{in}{\sqrt{in}} = \sqrt{in}$. It takes 19 cycles for completion.

## Max/Min

Max/Min calculate the maximum/minimum of two numbers and returns the corresponding number. The user also has an option to specify if you want to compare magnitudes or do regular signed comparison with a paramater. Takes 1 cycle to compute.

##Comparison

Comparison can calculate equal to, not equal to, less than, greater than, less than or equal to, or greater than or equal to. The operation is decided by a parameter Takes 1 cycle to compute.
