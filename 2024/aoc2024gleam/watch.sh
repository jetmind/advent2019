time gleam run $@
fswatch -o src/*.gleam | xargs -n1 -I{} time gleam run $@
