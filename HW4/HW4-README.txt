CS 320 Principles of Programming Languages
Homework 4
Due Wednesday 3/14 before class

This homework comes with a Haskell implementation of the applied call-by-value
lambda calculus that's covered in the week 7 and 8 lecture slides.

  - Lambda.lhs is the abstract syntax definition; you won't have to modify
    this, but you should look over it to see the AST types for lambda calculus
    types and expressions.

  - LambdaLex.hs implements a lexer and LambdaParse.hs implements a parser for
    the syntax used in the lecture slides; we're done focusing on syntax in
    this class, so you don't have to look at these unless you want to. The
    important thing to know is that backslashes can be used as lambdas, but
    since the backslash is also the escape character in Haskell strings, you
    have to use two backslashes every time (e.g. "(\\x. x + x) 1").

  - LambdaSmallStep.lhs implements the small-step semantics from the week 7
    lecture slides. The "readEval" function can be used to evaluate strings
    expressions written as strings.

  - LambdaBigStep.lhs implements the big-step semantics from the week 8 lecture
    slides. Like in the small step file, there's a "readEval" function that can
    be used to evaluate expressions written as strings; see the bottom of the
    file for example usage.

  - LambdaTypecheck.lhs implements the typechecking rules from the week 8
    lecture slides. There's a "readTypecheck" function that can be used to
    typecheck expressions written as strings; see the bottom of the file for
    example usage.

1. (8 points)

Normalize the following untyped applied lambda calculus expressions as much as
possible using the call-by-value rules, showing all steps. You can either do
this by hand or extend LambdaSmallStep.hs with a function to do it for you; if
you do it with a function, make sure to include the function in your
submission. (Hint: the final result of each normalization can be checked with
the provided "readEval" function in LambdaSmallStep.hs.)

a) (λx. x * x) 1
b) (λx. x + 4) ((λy. y + 5) 3)
c) (λf g x. g (f x)) (λa. a * a) (λb. b + 1 + 2) 3
d) (λf x. f (f (f x))) (λb. if b then false else true) true


2. (8 points)

For each of the following simply-typed applied lambda calculus expressions,
give a typing derivation or show that the expression is ill-typed by showing
that there can't exist a typing derivation for it. (Hint: the final result of
typechecking each expression can be checked with the provided "readTypecheck"
function in LambdaTypecheck.hs.)

a) if (if true then false else true) then false else true
b) λ(x: num). x + x * 3
c) λ(a: bool). if a then 1 else a
d) (λ(f: num -> bool) (a: num). if f a then a + 1 else a + 2) (λ(x: num). false)


3. (20 points)

At each of the five commented lines in the following procedural program:
  - Write out the contents of the type environment during typechecking.

  - Write out the contents of the evaluation environment (the stack) during
    evaluation for each time the line is hit during execution, assuming
    execution starts with main().

num distance(num x, num y) {
  bool b = x < y;
  num d = 0;
  // 1

  if b {
    num z = y - x;
    d = z;
    // 2
  } else {
    num w = x - y;
    d = w;
    // 3
  }

  // 4
  return d;
}

unit main() {
  num x = distance(1, 2);
  num y = distance(4, 3);
  // 5
}


4. (4 points)

***********PLEASE SEE LambdaTypecheck.lhs*********

Add a case for the numeric less-than operator (the Less constructor) to the
"step" function in LambdaSmallStep.lhs, the "eval" function in
LambdaBigStep.lhs, or the "typecheck" function in LambdaTypecheck.lhs. (The
parsing code for the operator is already implemented.)

You're only required to add the case in one file of your choice: there are 12
points available in this question (4 for each file) but the question is graded
out of 4 points, so if you add it correctly in more than one file it'll count
as extra credit.

Here are the inference rules for the operator in each semantics:


Small step:

         n1 < n2                   n1 >= n2
    ------------------        -------------------
     (n1 < n2) ⇒ true          (n1 < n2) ⇒ false
 
 
         e1 ⇒ e1'                   e2 ⇒ e2'
 ------------------------   ------------------------
  (e1 < e2) ⇒ (e1' < e2)     (v1 < e2) ⇒ (v1 < e2')



Big step:

  e1 ⇓ n1   e2 ⇓ n2   n1 < n2     e1 ⇓ n1   e2 ⇓ n2   n1 >= n2
 -----------------------------   ------------------------------
     <ρ, (e1 < e2)> ⇓ true           <ρ, (e1 < e2)> ⇓ false



Typechecking:

  Γ ⊢ e1 : num   Γ ⊢ e2 : num
 -----------------------------
      Γ ⊢ (e1 < e2) : bool
