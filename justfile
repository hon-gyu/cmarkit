default:
    just --list

format-dir dir="src/":
    find {{dir}} -name "*.ml" -o -name "*.mli" | xargs ocp-indent --inplace

format file:
    ocp-indent {{file}} --inplace
