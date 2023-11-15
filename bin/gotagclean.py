#!/usr/bin/env python3

import sys

file = sys.argv[1]

with open(file) as f:
    for line in f.readlines():
        if line.startswith("!"):
            continue
        first, file, li, *rest = line.split()
        if rest[0] == "i":
            continue
        newrest = []
        for i in range(1, len(rest)):
            r = rest[i]
            if r.startswith("line"):
                continue
            r = r.removeprefix("access:")
            r = r.removeprefix("signature:")
            newrest.append(r)
        print(f"{first:<12}" + " " + f"{file}:"+li[:-2] + "\t" + "\t".join(newrest))
