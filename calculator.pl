:- initialization(main).

main :-
    writeln('Enter your arithmetic expression (Press q to quit)'),
    calculator_loop.

calculator_loop :-
    write('> '),
    read_line_to_string(user_input, Input),
    ( Input == "q" -> 
        writeln('End of the program'), halt  % halt komutu programı sonlandırır
    ; 
        process_input(Input),
        calculator_loop
    ).

process_input(Input) :-
    catch(
        ( term_string(Expr, Input),
          Result is Expr,
          format('Result: ~w~n', [Result])
        ),
        Error,
        handle_error(Error)
    ).

handle_error(error(E,_)) :-
    format('Error: ~w~n', [E]).
handle_error(_) :-
    writeln('Unknown error').

