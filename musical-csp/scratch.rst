  * Rendered lines are created bar-by-bar, in which each pass of the solver is given a starting note and chord and target
    note and chord, and returns five notes
  * An entire bass line is created by chaining output passes, reusing the target (final) note of each bar
    as the starting note of the next.

