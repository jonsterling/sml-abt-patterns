functor PatternUtil
          (structure Pattern : PATTERN
           structure Dict : DICT where type key = Pattern.metavar) : PATTERN_UTIL =
struct
  open Pattern

  exception InvalidMatch

  structure AbtUtil : ABT_UTIL = AbtUtil(Abt)
  structure V = Abt.Variable

  structure Solution =
  struct
    type t = Abt.t Dict.dict

    val empty = Dict.empty
    fun extend S (x, M) =
      case Dict.find S x of
          NONE => Dict.insert S x M
        | SOME N => if Abt.eq (M, N) then S else raise InvalidMatch

    fun concat (S, S') =
      Dict.foldl (fn (x,M,S'') => extend S'' (x,M)) S S'

    fun compile S =
      Dict.foldl (fn (x,M,f) => AbtUtil.subst M x o f) (fn x => x) S
  end

  local
    fun go H (Abt.$ (operator, subterms)) =
      let
        val patterns = Vector.map (go H o Abt.out) subterms
      in
        into (SUBTERM (Abt.$ (operator, patterns)))
      end
      | go H (Abt.` x) =
        if Dict.member H x then
          into (SUBTERM (Abt.` x))
        else
          into (METAVAR x)
      | go H (Abt.\ (x, E)) =
        go (Dict.insert H x ()) (Abt.out E)
  in
    val const = go Dict.empty o Abt.out
  end

  type solution = Solution.t

  local
    fun bound H (x, y) =
      V.eq (Dict.lookup H x, y)
      handle _ => V.eq (Dict.lookup H y, x)
      handle _ => false

    fun go H (METAVAR x, M) S = Solution.extend S (x, M)
      | go H (SUBTERM tm, M) S =
        case (tm, Abt.out M) of
            (Abt.$ (operator, pats), Abt.$ (operator', subterms)) =>
            if Abt.Operator.eq (operator, operator') then
              Vector.foldl
                (fn ((pat,tm), S') => Solution.concat (S', go H (out pat, tm) S'))
                S
                (VectorPair.zip (pats, subterms))
            else
              raise InvalidMatch
          | (Abt.` x, Abt.` y) =>
            if bound H (x, y) then
              S
            else
              raise InvalidMatch
          | (Abt.\ (x, E), Abt.\ (y, E')) => go (Dict.insert H x y) (out E, E') S
          | _ => raise InvalidMatch
  in
    fun match (pat, term) = go Dict.empty (out pat, term) Solution.empty
  end

  fun apply (soln, term) = Solution.compile soln term
end
