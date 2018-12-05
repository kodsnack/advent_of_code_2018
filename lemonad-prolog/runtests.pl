use_module(library(plunit)).

main :-
    [december01],
    load_test_files([]),
    run_tests,
    test_report(fixme).
