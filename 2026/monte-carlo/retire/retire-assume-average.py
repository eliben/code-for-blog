"""Plot a retirement portfolio entirely in today's dollars.

Run with `uv run retire-assume-average.py` using the shared project dependencies.
Add `--show` to open the plot as well as saving it to /tmp/retirement.png.

At the end of each year, apply the inflation-adjusted return to the portfolio,
then withdraw a fixed $30,000 in today's dollars. The recurrence is:

    balance[n] = balance[n - 1] * (1.04 / 1.02) - 30_000

Balances on the plot are measured immediately after each withdrawal.
"""

import argparse
from pathlib import Path


INITIAL_BALANCE = 1_000_000
ANNUAL_WITHDRAWAL = 30_000
ANNUAL_RETURN = 0.04
ANNUAL_INFLATION = 0.02
MAX_YEARS = 200


def simulate(
    initial_balance=INITIAL_BALANCE,
    annual_withdrawal=ANNUAL_WITHDRAWAL,
    annual_return=ANNUAL_RETURN,
    annual_inflation=ANNUAL_INFLATION,
    max_years=MAX_YEARS,
):
    """Return yearly real balances (including year 0) and the depletion year.

    A depletion year of None means the portfolio outlasted max_years.
    Stop at zero when the remaining balance cannot fund the full withdrawal.
    """
    balance = initial_balance
    real_return_factor = (1 + annual_return) / (1 + annual_inflation)
    balances = [balance]

    for year in range(1, max_years + 1):
        balance *= real_return_factor
        balance -= annual_withdrawal
        balances.append(max(0, balance))
        if balance <= 0:
            return balances, year

    return balances, None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=Path("/tmp/retirement.png"))
    parser.add_argument("--show", action="store_true", help="Open the plot window")
    args = parser.parse_args()

    import matplotlib

    if not args.show:
        matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import StrMethodFormatter

    balances, depletion_year = simulate()
    years = range(len(balances))
    real_return = (1 + ANNUAL_RETURN) / (1 + ANNUAL_INFLATION) - 1

    print("All amounts are in today's dollars.")
    print(f"Starting balance: ${INITIAL_BALANCE:,.2f}")
    print(f"Annual return: {ANNUAL_RETURN:.1%}; inflation: {ANNUAL_INFLATION:.1%}")
    print(f"Real annual return: {real_return:.4%}")
    print(f"Fixed year-end withdrawal: ${ANNUAL_WITHDRAWAL:,.2f}")
    if depletion_year is not None:
        print(f"Money runs out at the end of year {depletion_year}.")
        print(f"Fully funded annual withdrawals: {depletion_year - 1}")
    else:
        print(f"Money does not run out within {MAX_YEARS} years.")

    fig, ax = plt.subplots(figsize=(10, 6), layout="constrained")
    ax.plot(years, balances, linewidth=2)
    if depletion_year is not None:
        ax.axvline(depletion_year, color="tab:red", linestyle=":")
        ax.annotate(
            f"Runs out: year {depletion_year}",
            xy=(depletion_year, 0),
            xytext=(-15, 35),
            textcoords="offset points",
            ha="right",
            arrowprops={"arrowstyle": "->", "color": "tab:red"},
        )
    ax.set(
        title=(
            f"Retirement: ${INITIAL_BALANCE:,.0f} starting portfolio\n"
            f"${ANNUAL_WITHDRAWAL:,.0f}/year in today's dollars; "
            f"{real_return:.2%} real annual return"
        ),
        xlabel="Years since retirement (balance after year-end withdrawal)",
        ylabel="Remaining money (today's dollars)",
        xlim=(0, len(balances) - 1),
        ylim=(0, None),
    )
    ax.yaxis.set_major_formatter(StrMethodFormatter("${x:,.0f}"))
    ax.grid(alpha=0.25)
    fig.savefig(args.output, dpi=160)
    print(f"Plot saved to {args.output}")
    if args.show:
        plt.show()
    plt.close(fig)


if __name__ == "__main__":
    main()
