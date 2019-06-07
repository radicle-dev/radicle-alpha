- namespaces should be mutable; machines evolve!
- they will have lots of updates if a machine is long lived.
- Even deleting definitions!
- Principle: it should be "statically" determinable what a variable is referring
  to, within a namespace.
- But this means unqualified importing of another namespace is illegal!
- But _can_ import specific variables unqualified.
- Under all these assumptions it was decided that most performant impl. was adding the concept of "module bindngs" to Envs.

