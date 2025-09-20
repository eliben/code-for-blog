from pathlib import Path
import argparse
import matplotlib.pyplot as plt
import sys
import seaborn


def read_pairs(path):
    """Read name count pairs from a text file.

    Lines starting with '#' or empty lines are ignored. Counts are parsed as
    integers (or floats if needed). Returns dict mapping name->count.
    """
    d = {}
    with open(path, "r", encoding="utf-8") as f:
        for i, line in enumerate(f, start=1):
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            parts = line.split()
            if len(parts) < 2:
                print(f"Skipping malformed line {i}: '{line}'", file=sys.stderr)
                continue
            name = parts[0]
            count = int(parts[1])
            d[name] = count
    return d


def plot_bar(data, out_path=None, show=False):
    width_px = 600
    height_px = 400
    dpi = 100
    width_inches = width_px / dpi
    height_inches = height_px / dpi

    keys = list(sorted(data.keys()))
    values = [data[k] for k in keys]

    fig, ax = plt.subplots(figsize=[width_inches, height_inches])
    ax.bar(keys, values, color="C0", width=0.6)
    ax.set_xticks(range(len(keys)))
    ax.set_xticklabels(keys, rotation=45, ha="right")
    ax.set_ylabel("count")
    plt.tight_layout()

    if out_path:
        plt.savefig(out_path, dpi=dpi)
        print(f"Saved plot to {out_path}")
    if show:
        plt.show()
    plt.close(fig)


def main():
    seaborn.set()
    parser = argparse.ArgumentParser(description="Plot name count pairs as a bar chart")
    parser.add_argument("input", help="Input text file with 'name count' per line")
    parser.add_argument(
        "--out",
        "-o",
        help="Output image path (PNG). If omitted, saves to output.png",
        default="output.png",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Show the plot interactively after creating it",
    )
    args = parser.parse_args()

    path = Path(args.input)
    if not path.exists():
        print(f"Input file does not exist: {path}", file=sys.stderr)
        sys.exit(2)

    data = read_pairs(path)
    plot_bar(data, out_path=args.out, show=args.show)


if __name__ == "__main__":
    main()
