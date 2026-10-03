"""Simulate retirement balances in today's dollars.

Run with `uv run retire-monte-carlo.py`; add --show to open the saved plot.
Rates are decimals: 0.04 means 4%, and 0.18 means 18 percentage points of
spread.

Each year, draw a return and an inflation rate from normal distributions and
clamp both to at least -99%. Apply the real growth factor
(1 + return) / (1 + inflation), then take out $30,000.
A depleted portfolio stays at zero.

The normal distributions have mean annual return of 4% and inflation of 2%, with
standard deviations of 18 and 1.5 percentage points. These are illustrative
assumptions for a mostly stock portfolio. All draws are independent.
"""

import argparse
from pathlib import Path

import numpy as np
from numpy.typing import NDArray

INITIAL_BALANCE = 1_000_000
ANNUAL_WITHDRAWAL = 30_000

# Annual rate settings, used directly by NumPy's normal distribution.
RETURN_MEAN = 0.04
RETURN_STD = 0.18
INFLATION_MEAN = 0.02
INFLATION_STD = 0.015

# Keep both growth factors positive, including the inflation denominator.
MIN_RATE = -0.99


def simulate(
    *,
    trials: int = 10_000,
    years: int = 100,
    return_mean: float = RETURN_MEAN,
    return_std: float = RETURN_STD,
    inflation_mean: float = INFLATION_MEAN,
    inflation_std: float = INFLATION_STD,
    seed: int = 42,
) -> NDArray[np.float64]:
    """Run independent retirement scenarios and return their yearly balances.

    Each trial is one possible future, lasting up to `years` years. The seed
    makes repeated runs reproducible. Each mean/std describes an annual rate
    before clamping. Zero std removes randomness.

    The result has one row per year and one column per trial. Row 0 contains
    the starting balance; later rows contain balances after withdrawing.
    All balances are in today's dollars, and zero means the money ran out.
    """
    if trials <= 0 or years <= 0:
        raise ValueError("Trials and years must be positive.")
    rng = np.random.default_rng(seed)
    balances = np.zeros((years + 1, trials))
    balances[0] = INITIAL_BALANCE

    for year in range(1, years + 1):
        # Each trial gets its own rates. Clamp extreme losses/deflation at -99%.
        returns = np.maximum(rng.normal(return_mean, return_std, size=trials), MIN_RATE)
        inflation = np.maximum(
            rng.normal(inflation_mean, inflation_std, size=trials), MIN_RATE
        )
        # Adjust growth for inflation, then withdraw a fixed real-dollar amount.
        # Clamping to zero keeps depleted portfolios depleted in later years.
        balances[year] = np.maximum(
            0,
            balances[year - 1] * (1 + returns) / (1 + inflation) - ANNUAL_WITHDRAWAL,
        )

    return balances


def main() -> None:
    """Read command-line settings, report depletion risk, and save the plot."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--trials", type=int, default=10_000)
    parser.add_argument("--years", type=int, default=100)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument(
        "--return-mean",
        type=float,
        default=RETURN_MEAN,
        help="Normal return mean before clamping (0.04 means 4%%)",
    )
    parser.add_argument(
        "--return-std",
        type=float,
        default=RETURN_STD,
        help="Annual return standard deviation (0.18 means 18 percentage points)",
    )
    parser.add_argument(
        "--inflation-mean",
        type=float,
        default=INFLATION_MEAN,
        help="Normal inflation mean before clamping (0.02 means 2%%)",
    )
    parser.add_argument(
        "--inflation-std",
        type=float,
        default=INFLATION_STD,
        help="Annual inflation standard deviation (0.015 means 1.5 percentage points)",
    )
    parser.add_argument(
        "--output", type=Path, default=Path("/tmp/retirement-monte-carlo.png")
    )
    parser.add_argument("--show", action="store_true", help="Open the plot window")
    args = parser.parse_args()

    balances = simulate(
        trials=args.trials,
        years=args.years,
        return_mean=args.return_mean,
        return_std=args.return_std,
        inflation_mean=args.inflation_mean,
        inflation_std=args.inflation_std,
        seed=args.seed,
    )

    # At each year, summarize all trials, including those that ran out of money.
    # For example, the 10th percentile is the balance below which 10% of trials lie.
    percentiles = np.percentile(balances, [10, 25, 50, 75, 90], axis=1)
    depleted_fraction = np.mean(balances == 0, axis=1)
    # The first year at which at least half the trials have depleted their money.
    median_crossing = np.flatnonzero(depleted_fraction >= 0.5)

    print(f"{args.trials:,} trials over {args.years} years; seed {args.seed}.")
    print(
        f"Starting balance: ${INITIAL_BALANCE:,.0f}; "
        f"withdrawal: ${ANNUAL_WITHDRAWAL:,.0f}/year."
    )
    print("All amounts are in today's dollars.")
    print("Independent normal rates; illustrative variability assumptions.")
    print(f"Both rates are clamped to at least {MIN_RATE:.0%}.")
    print("Normal distribution settings before clamping:")
    print(
        f"Return: mean {args.return_mean:.1%}, "
        f"SD {100 * args.return_std:g} percentage points."
    )
    print(
        f"Inflation: mean {args.inflation_mean:.1%}, "
        f"SD {100 * args.inflation_std:g} percentage points."
    )
    for year in sorted({30, 40, 50, 60, args.years}):
        if year <= args.years:
            print(f"Depleted by year {year}: {depleted_fraction[year]:.1%}")
    if median_crossing.size:
        print(f"Median depletion year: {median_crossing[0]}")
    else:
        print(f"Median depletion year: beyond the {args.years}-year horizon.")
    print(f"Still funded at year {args.years}: {1 - depleted_fraction[-1]:.1%}")

    import matplotlib

    if not args.show:
        matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import MultipleLocator, PercentFormatter, StrMethodFormatter

    years = np.arange(args.years + 1)
    # Compare with fixed annual rates equal to the normal distribution means.
    constant_balances = simulate(
        trials=1,
        years=args.years,
        return_mean=args.return_mean,
        return_std=0,
        inflation_mean=args.inflation_mean,
        inflation_std=0,
    )[:, 0]

    fig, (balance_ax, risk_ax) = plt.subplots(
        2, 1, figsize=(10, 9), dpi=80, sharex=True, layout="constrained"
    )
    balance_ax.fill_between(
        years,
        percentiles[0],
        percentiles[4],
        alpha=0.15,
        color="tab:blue",
        label="10th–90th percentiles",
    )
    balance_ax.fill_between(
        years,
        percentiles[1],
        percentiles[3],
        alpha=0.25,
        color="tab:blue",
        label="25th–75th percentiles",
    )

    balance_ax.plot(
        years,
        constant_balances,
        "--",
        color="black",
        label="Constant return and inflation",
    )
    balance_ax.set(ylabel="Balance (today's dollars)", ylim=(0, None))
    balance_ax.yaxis.set_major_formatter(StrMethodFormatter("${x:,.0f}"))
    balance_ax.legend(loc="upper right", fontsize=14)

    risk_ax.step(years, depleted_fraction, where="post", color="tab:red")
    risk_ax.set(
        xlabel="Years since retirement (after year-end withdrawal)",
        ylabel="Fraction of trials depleted",
        xlim=(0, args.years),
        ylim=(0, 1),
    )
    risk_ax.xaxis.set_major_locator(MultipleLocator(5))
    risk_ax.yaxis.set_major_formatter(PercentFormatter(1))
    for ax in (balance_ax, risk_ax):
        ax.xaxis.label.set_fontsize(14)
        ax.yaxis.label.set_fontsize(14)
        ax.grid(alpha=0.25)
    fig.savefig(args.output, dpi=80)
    print(f"Plot saved to {args.output}")
    if args.show:
        plt.show()
    plt.close(fig)


if __name__ == "__main__":
    main()
