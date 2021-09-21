#!/usr/bin/env python3

from pathlib import Path
import sys
import networkx as nx


def parse(contents):
    result = []
    token = ""
    pos = 0
    in_string = False
    while pos < len(contents):
        char = contents[pos]
        if char == "(" and not in_string:
            if token != "":
                result.append(token)
                token = ""
            parsed, parsed_count = parse(contents[pos + 1:])
            pos += parsed_count + 1
            result.append(parsed)
        elif char == ")" and not in_string:
            if token != "":
                result.append(token)
            return result, pos
        elif char in " \n\t\r\v" and not in_string:
            if token != "":
                result.append(token)
                token = ""
        elif char == '"':
            token += char
            in_string = not in_string
        elif char in "'`":
            pass
        else:
            token += char

        pos += 1
    return result, pos


def pprint(contents, spacing=""):
    for entry in contents:
        if isinstance(entry, str):
            print(spacing, entry)
        else:
            pprint(entry, spacing=spacing + "  ")


def find(tree, query):
    if len(query) == 0:
        if len(tree) > 0:
            yield tree
        return

    if isinstance(query, str):
        queries = query.split()
        query = queries[0]
    else:
        queries = query
        query = query[0]
    from_root = query[0] == "^"
    if from_root:
        query = query[1:]
    pos = 0

    for pos in range(len(tree)):
        entry = tree[pos]
        if query != ">":
            if isinstance(entry, str):
                if entry == query or query == "*" or query == f"@{pos}":
                    yield from find(tree[pos + 1:], queries[1:])
            elif not from_root:
                if isinstance(entry, int):
                    print("found integer entry", entry)
                yield from find(entry, queries)
        else:
            yield from find(entry, queries[1:])


def removeprefix(self, prefix):
    return self[len(prefix):] if self.startswith(prefix) else self


def parse_packages(parsed):
    for package in find(parsed, 'define-public'):
        name = package[0]
        package = next(find(package, 'package'))

        inputs = []
        for package_type in ["inputs", "native-inputs", "propagated-inputs"]:
            # for input in find(package, f'> inputs > > @0'):
            #     print(input)
            inputs += [removeprefix(input[0], ",")
                       for input in find(package, f'> {package_type} > > @0')]
        yield (name, inputs)


contents = Path(sys.argv[1]).read_text()

G = nx.DiGraph()
for name, deps in parse_packages(parse(contents)[0]):
    for dep in deps:
        G.add_edge(name, dep)

for cycle in nx.simple_cycles(G):
    print("cycle:", " -> ".join(cycle))
