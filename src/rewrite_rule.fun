signature ABT_PATTERN =
sig
  structure Abt : LIST_ABT
  structure Pattern : PATTERN
    where type operator = Abt.operator
    where type metavariable = Abt.metavariable
    where type metacontext = Abt.metacontext
    where type 'a spine = 'a Abt.spine
end

functor RewriteRule (P : ABT_PATTERN) : REWRITE_RULE =
struct
  open P
  open Pattern

  structure MetaEnv =
    SplayDict
      (structure Key =
       struct
         open Abt.Metavariable Abt.Metavariable.Eq
       end)

  type term = Abt.abt
  type env = Abt.btm MetaEnv.dict

  (* When [pat] is a pattern with metacontext [Θ] and [m] is a closed term in
   * metacontext [Θ], then [p ~> m] is a rewrite rule. *)
  datatype view = ~> of pattern * term

  (* When [pat] is a pattern and [m] is a closed term without metavariables,
   * then [pat <*> m] is the judgment that [m] unifies with the pattern [pat] *)
  datatype match = <*> of pattern * term
  infix ~> $@ <*>

  datatype rule = RULE of view

  exception InvalidRule

  (* a rewrite rule is valid in case the definiens is well-formed under
   * metavariable context induced by the definiendum *)
  fun into (p ~> m) =
    let
      val (_, Theta) = Pattern.out p
      val fvs = Abt.freeVariables m
    in
      case fvs of
           [] => RULE (p ~> Abt.check Theta (Abt.infer m))
         | _ => raise InvalidRule
    end

  fun out (RULE r) = r

  exception RuleInapplicable

  local
    open Abt
    infix $ $# \
    structure Spine = Operator.Arity.Valence.Spine
    structure Sort = Operator.Arity.Sort

    fun matchOperator (ptheta, theta) =
      (* compare if they are the "same" operator modulo parameters *)
      if Operator.eq (fn _ => true) (ptheta, theta) then
        let
          (* therefore, the operators should have compatible supports *)
          val us = map #1 (Operator.support ptheta)
          val vs = map #1 (Operator.support theta)
        in
          ListPair.zipEq (us, vs)
        end
      else
        raise RuleInapplicable

    fun matchSort (sigma, tau) =
      if Sort.Eq.eq (sigma, tau) then
        ()
      else
        raise RuleInapplicable

    fun extendEnv rho (mv, e) =
      MetaEnv.insertMerge rho mv e (fn _ => raise RuleInapplicable)
    fun concatEnv (rho, rho') =
      MetaEnv.union rho rho' (fn _ => raise RuleInapplicable)

    fun unify (pat <*> m) =
      let
        val (ptheta $@ pargs, Theta) = Pattern.out pat
        val (theta $ es, tau) = Abt.infer m
        fun go [] [] (rho, env) = (rho, env)
          | go (MVAR mv :: pargs) (e :: es) (rho, env) = go pargs es (rho, extendEnv env (mv, e))
          | go (PAT pat :: pargs) ((([], []) \ m) :: es) (rho, env) =
              let
                val (rho', env') = unify (pat <*> m)
              in
                go pargs es (rho @ rho', concatEnv (env, env'))
              end
          | go _ _ _ = raise RuleInapplicable
        val (rho, env) = go pargs es ([], MetaEnv.empty)
      in
        (matchOperator (ptheta, theta) @ rho, env)
      end


    (* we recursively wring out all the metavariables by looking them up in the
     * environment. Another option would be to replace the environment by a
     * tree of metavariable substitutions, and apply them from the leaves down
     * in order. *)
    fun applyEnv env m =
      let
        val Theta = Abt.metacontext m
      in
        if Metacontext.isEmpty Theta then
          m
        else
          foldl (substMetavar Theta env) m (Metacontext.toList Theta)
      end
    and substMetavar Theta env ((mv, vl), m) =
      let
        val (xs, us) \ m' = MetaEnv.lookup env mv
        val e = Abt.checkb Theta ((xs, us) \ applyEnv env m', vl)
      in
        Abt.metasubst (e, mv) m
      end

    fun applyRen rho m =
      foldl (fn (r, m') => Abt.rename r m') m rho

  in
    fun compile (RULE (pat ~> m)) n =
      let
        val (rho, env) = unify (pat <*> n)
      in
        applyRen rho (applyEnv env m)
      end
  end
end

