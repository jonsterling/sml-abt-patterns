signature PATTERN_UTIL =
sig
  include PATTERN

  (* Any term may be turned into a pattern, such that its free variables shall become metavariables *)
  val const : Abt.t -> pattern

  type solution

  exception InvalidMatch

  (* Compute a first-order substitution by matching a pattern against a term. *)
  val match : pattern * Abt.t -> solution

  (* Apply a substitution to a term *)
  val apply : solution * Abt.t -> Abt.t
end
