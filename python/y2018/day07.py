# The Sum of Its Parts

import collections

from utils import *


class Step:
    work_duration_min = 60
    steps = {}

    @classmethod
    def find_or_create(cls, name):
        step = cls.steps.get(name)
        if step is None:
            step = cls(name)
            cls.steps[name] = step
        return step

    @classmethod
    def has_step_ready(cls):
        return any([s for s in cls.steps.values() if s.is_ready()])

    def __init__(self, name):
        self.name = name
        self.befores = []
        self.afters = []
        self.started = False
        self.done = False
        self.work_duration_seconds = ord(name) - ord("A") + Step.work_duration_min + 1

    def __str__(self):
        b_names = [b.name for b in self.befores]
        a_names = [a.name for a in self.afters]
        return f"Step {self.name} befores {b_names} afters {a_names}"

    def is_ready(self):
        return not self.started and not self.befores

    def start(self):
        self.started = True

    def finish(self):
        self.done = True
        for a in self.afters:
            if self in a.befores:
                a.befores.remove(self)


class Worker:
    def __init__(self, id):
        self.id = id
        self.working_on = None
        self.time_left = 0

    def start_work(self, step):
        self.working_on = step
        self.working_on.start()
        self.time_left = step.work_duration_seconds

    def do_work(self):
        if self.working_on:
            self.time_left -= 1
            if self.time_left == 0:
                self.working_on.finish()
                self.working_on = None

    def available_for_work(self):
        return self.time_left == 0

    def __str__(self):
        job = self.working_on and self.working_on.name or "."
        return f"Worker {self.id} working on {job} time left {self.time_left}"


def part1(testing=False):
    _read_steps(testing, 1)
    for step in _ordered_steps_generator():
        step.start()
        step.finish()
        print(step.name, end="")
    print("")


def part2(testing=False):
    num_workers = 5
    if testing:
        Step.work_duration_min = 0
        num_workers = 2
    _read_steps(testing, 1)
    workers = [Worker(i) for i in range(num_workers)]
    time_spent = _timed_parallel_work(workers)
    print(time_spent)


def _read_steps(testing, part_num):
    for line in data_file_lines(7, testing, part_num):
        before = Step.find_or_create(line[5])
        after = Step.find_or_create(line[36])
        before.afters.append(after)
        after.befores.append(before)


def _ordered_steps_generator():
    ready_func = lambda: [s for s in Step.steps.values() if s.is_ready()]
    ready_for_work = ready_func()
    while ready_for_work:
        earliest_ready = sorted(ready_for_work, key=lambda n: n.name)[0]
        yield earliest_ready
        ready_for_work = ready_func()


def _timed_parallel_work(workers):
    t = 0
    work_gen = _ordered_steps_generator()
    while True:
        _assign_work(workers, work_gen)
        busy_workers = [w for w in workers if not w.available_for_work()]
        if len(busy_workers) == 0 and t > 0:
            return t
        for w in busy_workers:
            w.do_work()
        t += 1


def _assign_work(workers, ordered_steps_generator):
    for w in workers:
        if w.available_for_work() and Step.has_step_ready():
            try:
                job = next(ordered_steps_generator)
                w.start_work(job)
            except StopIteration:
                break
