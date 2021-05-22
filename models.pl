
:- dynamic model/1.
:- use_module(library(error)).

solve_models(equiv, success) :-
    shell('cat empty > models-theory_appended.lp', _),
    solve_models('theory_appended.lp', 1, [_|_]).

solve_models(equiv, fail) :-
    shell('cat empty > models-theory_appended.lp', _),
    solve_models('theory_appended.lp', 1, []).

solve_models(File, All, Models) :-
     ((All = all) -> Option = '0 ' ; Option = ' '),
     string_concat('clingo --outf=3 ', Option, Pre),
     string_concat(Pre, File, BaseCmd),
     string_concat('models-',File, FileModels),
     string_concat('cat empty > ', FileModels, EmptyModelsCmd),
     shell(EmptyModelsCmd, _),
     string_concat(BaseCmd, ' post-process.py', PostProcess),
     string_concat(' >> ', FileModels, Redirect),
     string_concat(PostProcess, Redirect, Cmd),
     shell(Cmd, _),
     load_models(FileModels, Models).

load_models(File, Models) :-
     load_models(File),
     findall(X, model(X), Models).

load_models(File) :-
        retractall(model(_)),
        open(File, read, Stream),
        call_cleanup(load_terms(Stream),
                     close(Stream)).

load_terms(Stream) :-
        read(Stream, T0),
        load_terms(T0, Stream).

load_terms(end_of_file, _) :- !.
load_terms(model(X), Stream) :- !,
        assert(model(X)),
        read(Stream, T2),
        load_terms(T2, Stream).
load_terms(Term, Stream) :-
        type_error(model, Term).