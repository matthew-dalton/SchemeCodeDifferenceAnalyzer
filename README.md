# SchemeCodeDifferenceAnalyzer

This Scheme program is purely functional, that is, it produces no side effects.

The purpose of this program is Scheme procedure called 'expr-compare' that takes two Scheme expressions as inputs, x and y, and produces a difference summary of where the two expressions are the same and where they differ.

When executed in an evironment where the Scheme variable '%' is true, the summary should have the same behavior as input x; otherwise, it should behave as input y would.

This procedure outputs a qualitative summary, that is, duly notes but largely disregards inconsequential differences such as variable renaming and using the shorthand 'λ' in place of 'lambda' for lambda expressions.

This procedure does not, however, account for the every feature that Scheme has to offer. Rather, it is limited to the subset of Scheme that includes the following:

  * constant literals
  * variable references
  * procedure calls
  * the quote form
  * the lambda form
  * the conditional form


Following are a few conventions taken for the output expression:

  * Wherever one expression uses the symbol 'λ' and the other uses either 'λ' or 'lambda', the output should use 'λ'
  * Wherever bound variable names agree, the summary should use that name
  * Wherever bound variable names disagree, the summary should combine the two such that if expression x used 'X' and expression y used 'Y', the summary would use 'X!Y'
  * '%' is used to represent a subexpression that is #t in expression x and #f in expression y
