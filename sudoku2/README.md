# Sudoku Solver v2

> My attempt at a more efficient sudoku solver.

### Ideas

- Have a manager function that will track improvements through rounds of filling obvious choices, finding only possible values, and guessing.  That way there will always be an authority for where to go next.  For example, I only want to move onto guessing if both filling automatic values and finding only options fails.