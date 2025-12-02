from types import GeneratorType


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


def chunks(xs: list, batch_size: int = 1000):
    """Yields `xs` in batches of `batch_size`."""
    for i in range(0, len(xs), batch_size):
        yield xs[i : i + batch_size]


def flatten(xs, acc=[]):
    """Flattens a list."""
    if isinstance(xs, list) or isinstance(xs, GeneratorType):
        for x in xs:
            flatten(x, acc)
    else:
        acc.append(xs)
    return acc


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
