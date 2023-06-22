module Problem3 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2021 Problem of the Week, Week 3
=============================================

This week's problem focuses on defining data types and recursive functions over
them, by representing and manipulating simple mathematical formulae.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Part (a): A type!
-----------------

For this part, you should define a datatype to represent logical formulae
(F or G).  Your datatype should (somehow) incorporate the following cases:

    Syntax   |  Meaning
    ---------+------------------------------------------------
    P, Q, R  |  Propositional variable (any string)
    ¬ F      |  Negation of F (where F is any formula)
    F ∧ G    |  Conjunction of F and G
    F ∨ G    |  Disjunction of F and G

-------------------------------------------------------------------------------}

data Formula = Negate Formula
             | Var String
             | Conj_Label Formula Formula
             | Disj_Label Formula Formula
  deriving (Eq, Show)

-- Constructor functions

variable :: String -> Formula
variable = Var

neg :: Formula -> Formula
neg = Negate

conj, disj :: Formula -> Formula -> Formula
conj = Conj_Label
disj = Disj_Label

-- A few examples.  

form1, form2, form3 :: Formula
form1 = neg (variable "X")
form2 = (neg (variable "X") `conj` variable "Y") `disj` variable "Z"
form3 = (variable "X" `disj` variable "Y") `conj` variable "Z"

{-------------------------------------------------------------------------------

Part (b): A function!
---------------------

For this part, you should write a function that converts an arbitrary formula
into negation normal form.  A formula is in negation normal form (NNF) if the
negation operator is ONLY applied to propositional variables, not to any other
type of formula.  For example:

    Formula      |  Negation normal form
    -------------+---------------------
    ¬(P ∧ ¬Q)    |   ¬P ∨ Q
    ¬(¬P ∨ ¬Q)   |   P ∧ Q

How should you convert a formula into NNF?  You'll need three logical
equivalences:

    ¬¬P        ≡   P
    ¬(F ∨ G)   ≡   ¬F ∧ ¬G
    ¬(F ∧ G)   ≡   ¬F ∨ ¬G

Keep in mind you may have to apply these rules multiple times.  For example,
given the formula

    ¬(P ∧ ¬P)

you could apply the third rule (one of de Morgan's laws) to get

    ¬P ∨ ¬(¬P)

but this isn't yet in NNF!  You then have to apply the first rule
(double-negation elimination) to get

    ¬P ∨ P

-------------------------------------------------------------------------------}

nnf :: Formula -> Formula
nnf (Var p) = Var p
nnf (Negate(Negate p)) = nnf p
nnf (Conj_Label p q) = Conj_Label(nnf p)(nnf q)
nnf (Disj_Label p q) = Disj_Label(nnf p)(nnf q)
nnf (Negate(Conj_Label p q)) = Disj_Label(nnf $ Negate p)(nnf $ Negate q)
nnf (Negate(Disj_Label p q)) = Conj_Label(nnf $ Negate p)(nnf $ Negate q)
nnf (Negate p) = Negate $ nnf p