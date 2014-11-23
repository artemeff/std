-module(std_fn).
-export([ compose/1
        , partial/2
        ]).

-spec compose([fun((any()) -> any())])
   -> fun((any()) -> any()).
compose(Fns) -> 
    lists:foldl(fun(G, F) ->
        fun(X) ->
            G(F(X))
        end
    end, fun(X) ->
        X
    end, Fns).

-spec partial(fun((...) -> any()), [any()])
   -> fun((any()) -> any()).
partial(F, FixedArgs) ->
    {arity, Arity} = erlang:fun_info(F, arity),
    case length(FixedArgs) of
        L when L < Arity ->
            Args = [{var, 1, N} ||
                N <- lists:seq(1, Arity - L)],
            FArgs = [case is_function(A) of
                false -> erl_parse:abstract(A);
                true  -> {var, 1, erlang:fun_to_list(A)}
            end || A <- FixedArgs],
            Parsed = [{'fun', 1,
                        {clauses,
                            [{clause, 1, Args, [],
                                [{call, 1,
                                    {var, 1, 'F'},
                                        FArgs ++ Args}]}]}}],
            Binds = [{erlang:fun_to_list(A), A} ||
                A <- FixedArgs, is_function(A)],
            {_, R, _} = erl_eval:exprs(Parsed, [{'F', F}] ++ Binds),
            R
    end.
