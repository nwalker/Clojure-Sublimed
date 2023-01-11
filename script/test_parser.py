#! /usr/bin/env python3
import os, sys, re

def print_table(titles, cols):
    cols = [col.split('\n') for col in cols]
    widths = [max([len(line) for line in col] + [len(title)]) for col, title in zip(cols, titles)]
    height = max(len(col) for col in cols)
    
    print("┌", end = "")
    for i, width in enumerate(widths):
        print("".join(['─'] * (width + 2)), end = "┬" if i < len(widths) - 1 else "┐")
    print()

    print("│", end = "")
    for title, width in zip(titles, widths):
        print(" " + title.ljust(width) + " ", end = "│")
    print()

    print("├", end = "")
    for i, width in enumerate(widths):
        print("".join(['─'] * (width + 2)), end = "┼" if i < len(widths) - 1 else "┤")
    print()

    for row in range(height):
        print("│", end = "")
        for col in range(len(cols)):
            str = cols[col][row] if row < len(cols[col]) else ""
            print(" " + str.ljust(widths[col]) + " ", end = "│")
        print()

    print("└", end = "")
    for i, width in enumerate(widths):
        print("".join(['─'] * (width + 2)), end = "┴" if i < len(widths) - 1 else "┘")
    print("\n")

if __name__ == '__main__':
  cwd = os.path.dirname(__file__)
  os.chdir(cwd + "/../src")
  sys.path.append(os.getcwd())
  import clojure_parser

  if len(sys.argv) > 1:
    expr = sys.argv[1]
    print(clojure_parser.parse(expr).serialize(expr))
  else:
    dir = cwd + "/../test_parser/"
    files = [file for file in os.listdir(dir) if file.endswith(".txt")]
    width = max(len(file) for file in files)
    for file in files:
        with open(dir + file) as f:
            content = f.read()
        print(file.ljust(width), "[", end = "")
        failures = []
        for match in re.finditer("={80}\n(.+)\n={80}\n\n((?:.+\n)+)\n-{80}\n\n((?:.+\n)+)", content):
            name = match.group(1)
            expr = match.group(2).strip('\n')
            expected = match.group(3).strip('\n')
            expected = "\n".join(line.rstrip(' ') for line in expected.split('\n'))
            actual = clojure_parser.parse(expr).serialize(expr)
            if actual != expected:
                print("F", end = "")
                failures.append((name, expr, expected, actual))
            else:
                print(".", end = "")
        print("]")
        for (name, expr, expected, actual) in failures:
            print_table([name, "Expected", "Actual"], [expr, expected, actual])