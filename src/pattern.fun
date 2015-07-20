functor Pattern (Abt : ABT) : PATTERN =
struct
  structure Abt = Abt
  type metavar = Abt.Variable.t

  datatype 'a view =
      METAVAR of metavar
    | SUBTERM of 'a Abt.view

  datatype pattern = IN of pattern view

  val into = IN
  fun out (IN t) = t
end
