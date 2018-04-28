package encrywm.frontend.semantics.error

class SemanticError(m: String, codeExample: String) extends Error(m.concat(s" In '$codeExample'"))
