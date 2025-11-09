import itertools


def minmax(xs):
    """Given an iterable, returns a (min, max) tuple."""
    min_val = None
    max_val = None
    for x in xs:
        if min_val is None or x < min_val:
            min_val = x
        if max_val is None or x > max_val:
            max_val = x
    return (min_val, max_val)


def flatten(list_of_lists):
    """Flattens one level."""
    return itertools.chain.from_iterable(list_of_lists)


pause_continue = False


def pause():
    """Writes a message and waits for input.

    Does nothing if the global `pause_continue` is True."""
    global pause_continue
    if pause_continue:
        return
    line = input(
        "Paused. 'q' to quit, 'c' to continue without pausing, anything else to step."
    )
    if line:
        if line[0] == "q":
            exit(0)
        if line[0] == "c":
            pause_continue = True
