# 1º Trabalho Prático de PFL

- 2LEIC06 - Grupo 2
    - António Ferreira - up202004735
    - Guilherme Almeida - up202006137

## New Data Types

**Vars**
*type Vars = [( Char, Int )]*

A new type synonym that represents the variable and respective degree of a Monomial. To associate a variable (a **Char**) with a degree (an **Int**), it was chosen a tuple. However, due to the fact that monomials can have more than one variable (2*x²*y, for example), we decided to use a list of tuples.

For example:

- **x²** = [('x', 2)]
- **x * y** = [('x', 1),('y', 1)]

**Moni**
*data Moni = Moni { coef :: Int, vars :: Vars} deriving (Ord, Eq)*

A new data type that has two parameters: *coef*, an **Int** which represents the coefficient of the monomial, and *vars*, a **Vars** which represents the variables and degrees of the monomial. This new type was created since there isn't any default data type that allows us to easily represent a monomial. On the other hand, by using 'data', we can represent, in a user-friendly way, a monomial, by instantiating our Show function. It needs to be noted that, whenever a monomial is only a number, like '4' or '45', its vars will be a list with only one element - ('_', 0). Also, if a monomial doesn't have a coefficient (like 'x' or 'x⁵ * y'), it means that the value of coef is 1.

For example:

- **23 * x³** = Moni 23 [('x', 3)]
- **3 * x * y⁴** = Moni 3 [('x', 1),('y', 4)]
- **4** = Moni 4 [('_', 0)]

**Poli**
*type Poli = [Moni]*

A new type synonym that is used to represent a Polynomial. Since a polynomial can be described as the combination of various elements, which are monomials, we decided to represent a polynomial as a list of **Moni**'s.

For example:

- **3 * x⁵ + 5 * y - 6** = [Moni 3 [('x', 5)], Moni 5 [('y', 1)], Moni -6 [('_', 0)]]

## Functionalities Implemented
### Normalize Polynomials
Given a polynomial, we can use the function **normalizePoli** to normalize it. In other words, this function takes a polynomial and orders the elements in decreasing order (degree >  variable > coefficient), does the internal sum of the polynomial by summing elements that have the same Vars and filters elements that have coefficient 0. When calling the function in question, for each member of the list, we also call the function **normalizeMoni**, which converts the vars of a monomial that has the coefficient 0 to [('_', 0)], and, if needed, for each element of the Vars of the monomial we call **normalizeVars**, responsible for normalizing the members of the Vars (transforms elements that have degree 0 and joins Vars that have the same variable by adding their degree).
### Sum Polynomials
Given two polynomials, we can sum them by using the function **sumPoli**.  In this function, we merge the two lists of monomials and do the internal sum of the elements by calling the **internalSum** function, which will sort the elements of the merged list and recursively sum two elements of the list until there's only one element or none on the list. For summing two elements we call the **sumMoni** function which sums two monomials if, and only if, they have the same Vars.
### Multiplicate Polynomials
To obtain the product of two polynomials we can use the **prodPoli** function, that takes two polynomials and, using list comprehensions, applies the function **prodMoni** to each two elements of the list. On the other hand, prodMoni is responsible for the product of two monomials, which means the product of the coefficients and the handling of the Vars, which is done by the **prodVars** function, whose main objetive is to merge the two lists of tuples and join the variables that have the same degree. It also should be noted that the function **normalizePoli** is also called to normalize the result.
### Derive Polynomials
In order to derive a polynomial we can use the **derivPoli** function, which will derive the given polynomial according to the variable chosen.  This task is done by list compreehension, where for each member of the polynomial the function **derivMoni** is called, responsible for handling the coefficient and invoking the **derivVars**, which be applied to every members of Vars and is essential treating the degrees during the derivation process. It should be noted that, if a monomial doesn't have the variable chosen to derive, it returns 0. Also, the function **normalizePoli** is called to normalize the final result.

### Parse String to Polynomial
With the objetive to parse a String to Poli, the **parsePoli** function was implemented. This function converts the given string into a list of tokens, each representing a different type of char. Then, integrates these tokens into a binary tree where each node will be a member of the given list. Finally, by calling an evaluator, this binary tree is converted into a Poli.
### Parse Polynomial to String
To convert a Poli into a String, in order to offer the user a more pleasing and easy way of viewing the results, we can use the **tellPoli** function.

## Examples
- tellPoli (normalizePoli (parsePoli "4 * x + 6 * x * y + 0 * x ^ 5 + 0 + 4 + 0 * x * y + 8 * x ^ 0"))
- sumPoli (parsePoli "2 * x ^ 2 + 3") (parsePoli "3 * y + 2 * x ^ 2 + 8")
- prodPoli (parsePoli "2 * x ^ 2 * y + 3") (parsePoli "3 * y + 2 *x ^ 2 + 8")
- derivPoli 'z' (parsePoli "3 * y + 2 * x ^ 2 + 8")
- derivPoli 'y' (parsePoli "3 * y + 2 * x ^ 2 + 8 * x * y")
- tellPoli (derivPoli 'y' (prodPoli (parsePoli "2* x ^ 3 - 5 * x ^ 2") (sumPoli (parsePoli "2 * x * y ^ 2 + 3 * x") (parsePoli "0 * x + y ^ 2 + 5 * x"))))