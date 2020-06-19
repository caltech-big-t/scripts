import csv
import sys
import os

def mappics(dirname, formats=["png", "jpg", "jpeg"]):
    """returns dict images and conflicts
    images maps image names to filenames
    conflicts map image names to list of filenames
    """
    picmap = {}
    conflicts = {}

    def add(name, ext, path):
        if name in picmap:
            if name not in conflicts:
                conflicts[name] = [picmap[name]]
            conflicts[name].append(path)
        picmap[name] = path

    for root, dirs, files in os.walk(dirname):
        for path in files:
            name, ext = path.split(".", 1)
            if ext in formats:
                add(name, ext, path)
    return picmap, conflicts

def readbalfour(fname):
    """returns list of lists representing balfour csv"""
    with open(fname, "r") as f:
        reader = csv.reader(f, delimiter="\t", quotechar="\"")
        return list(reader)

def writebalfour(peeps, fname):
    """writes list of lists in balfour csv format"""
    with open(fname, "w") as f:
        writer = csv.writer(f, delimiter="\t", quotechar="\"", quoting=csv.QUOTE_NONNUMERIC)
        writer.writerows(peeps)

def fixextensions(peeps, picmap, basedir="."):
    """replaces image names with ones that actually exist in picmap"""
    fixed = [peeps[0].copy()]
    missing = []
    for i in range(1, len(peeps)):
        name, ext = peeps[i][2].split(".", 1)
        if (name in picmap):
            fixed.append(peeps[i].copy())
            fixed[i][2] = picmap[name]
        else:
            missing.append(i)
    return fixed, missing

def main(inname, outname):
    peeps = readbalfour(inname)
    basedir = os.path.dirname(inname)
    picdir = os.path.join(basedir, peeps[1][1])
    picmap, conflicts = mappics(picdir)

    print(f"{len(peeps)}\t peeps")
    print(f"{len(picmap)}\t pics")
    print(f"{len(conflicts)}\t conflicts")

    fixed, missing = fixextensions(peeps, picmap, basedir)
    print(f"{len(missing)}\t missing")
    writebalfour(fixed, outname)

if __name__ == "__main__":
    if (len(sys.argv) != 3):
        print("Fixes incorrect file extensions for images in balfour csvs")
        print(f"Usage:\n{sys.argv[0]} inputfile outputfile")
        exit(1)
    else:
        main(sys.argv[1], sys.argv[2])
