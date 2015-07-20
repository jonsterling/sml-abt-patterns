signature PATTERN =
sig
  (* The term language *)
  structure Abt : ABT

  (* A metavariable is a free variable which we shall consider "open to
     negotiation" during unification. Other free variables occuring in a pattern
     are intended to match exactly. *)
  type metavar = Abt.Variable.t

  (* We augment the structure of abts by adding nodes for metavariables. *)
  datatype 'a view =
      METAVAR of metavar
    | SUBTERM of 'a Abt.view

  type pattern
  val into : pattern view -> pattern
  val out : pattern -> pattern view
end
