module rec Problang.Parser

type Observation = RuleContent * (SideEffect list)
type RuleContent = string
type SideEffect = Nil
