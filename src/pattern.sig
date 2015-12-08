signature PATTERN =
sig
  type operator
  type metavariable
  type metacontext
  type 'a spine

  type pattern

  datatype 'a argument =
      MVAR of metavariable
    | PAT of pattern

  datatype 'a view = @ of operator * 'a argument spine

  structure Error :
  sig
    datatype t =
        NON_LINEAR
      | OTHER
  end

  exception InvalidPattern of Error.t

  (* Construct a valid linear abt pattern *)
  val into : pattern view -> pattern

  (* Inspect a linear abt pattern and its metavariable context *)
  val out : pattern -> pattern view * metacontext
end
