#!/usr/bin/env python3
"""Figure 1: study area and sampling locations, Plum Island Sound estuary.

Basemaps are public-domain U.S. Geological Survey products from The National Map
(https://basemap.nationalmap.gov): USGSImageryOnly (NAIP orthoimagery) for the main map
and USGSTopo for the inset. Both were retrieved with the ArcGIS REST `export` request
recorded in fig1_basemaps.json and are stored beside this script, so the figure is
drawn without network access.

Usage:  python make_fig1_map.py [output.png]
Needs:  Python 3 with numpy, matplotlib and Pillow.
"""
import csv
import json
import logging
import math
import os
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import patheffects as pe
from matplotlib.lines import Line2D
from matplotlib.patches import Rectangle
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "..", "..", "figures", "Fig1_study_area_map.png")
R = 6378137.0


def mx(lon):
    return R * math.radians(lon)


def my(lat):
    return R * math.log(math.tan(math.pi / 4 + math.radians(lat) / 2))


with open(os.path.join(HERE, "fig1_basemaps.json")) as fh:
    cfg = json.load(fh)
main, inset = cfg["main"], cfg["inset"]

sites = []
with open(os.path.join(HERE, "PIE_sampling_locations.csv"), newline="") as fh:
    for row in csv.DictReader(fh):
        sites.append((row["Sample Type"].strip(), float(row["Lat"]), float(row["Lon"])))

logging.getLogger("matplotlib.font_manager").setLevel(logging.ERROR)
plt.rcParams["font.family"] = ["Arial", "Liberation Sans", "DejaVu Sans"]
halo = [pe.withStroke(linewidth=3, foreground="black")]

x0, y0, x1, y1 = main["bbox3857"]
fig_w = 11.0
ax_w, left, top = 0.90, 0.075, 0.055
fig_h = fig_w * ax_w * (y1 - y0) / (x1 - x0) / (1 - top - 0.02)
fig = plt.figure(figsize=(fig_w, fig_h), dpi=300)
ax = fig.add_axes([left, 0.02, ax_w, 1 - top - 0.02])
ax.imshow(Image.open(os.path.join(HERE, main["file"])), extent=[x0, x1, y0, y1], interpolation="lanczos")
ax.set_xlim(x0, x1)
ax.set_ylim(y0, y1)

# graticule ticks, labelled on the top and left edges
lon_ticks = [-70.92, -70.90, -70.88, -70.86, -70.84, -70.82, -70.80, -70.78, -70.76]
lat_ticks = [42.68, 42.70, 42.72, 42.74, 42.76, 42.78]
ax.set_xticks([mx(v) for v in lon_ticks])
ax.set_xticklabels([f"{v:.2f}°" for v in lon_ticks], fontsize=12, fontweight="bold")
ax.set_yticks([my(v) for v in lat_ticks])
ax.set_yticklabels([f"{v:.2f}°" for v in lat_ticks], fontsize=12, fontweight="bold", rotation=90, va="center")
ax.tick_params(top=True, bottom=True, left=True, right=True, labeltop=True, labelbottom=False,
               direction="out", length=6, width=1)
for s in ax.spines.values():
    s.set_linewidth(1.2)

# sampling locations
style = {
    "Food Web": dict(marker="o", ms=22, mfc="none", mec="#e3120b", mew=4.5, zorder=5),
    "Water Qualit": dict(marker="s", ms=11, mfc="#2ca02c", mec="black", mew=0.8, zorder=6),
    "Fiddler Transect": dict(marker="o", ms=11, mfc="#74b3f7", mec="white", mew=0.8, zorder=6),
}
for kind, lat, lon in sites:
    ax.plot(mx(lon), my(lat), linestyle="none", **style[kind])

# place names
ax.text(mx(-70.812), my(42.7215), "Plum Island Sound", color="white", fontsize=15, fontweight="bold",
        ha="center", va="center", path_effects=halo)
ax.text(mx(-70.7745), my(42.7375), "Gulf of Maine", color="white", fontsize=15, fontweight="bold",
        ha="center", va="center", path_effects=halo)

# scale bar: Web Mercator distances are stretched by 1 / cos(latitude)
lat_mid = 0.5 * (main["lat"][0] + main["lat"][1])
bar = 2000.0 / math.cos(math.radians(lat_mid))
bx, by = mx(-70.9305), my(42.6785)
ax.add_patch(Rectangle((bx - 250, by - 420), bar + 2300, 1050, fc="white", ec="black", lw=0.8, alpha=0.92, zorder=7))
ax.add_patch(Rectangle((bx, by), bar, 200, fc="#1f6fd6", ec="black", lw=0.8, zorder=8))
ax.text(bx, by + 290, "0", fontsize=11, fontweight="bold", ha="center", va="bottom", zorder=8)
ax.text(bx + bar, by + 290, "2", fontsize=11, fontweight="bold", ha="center", va="bottom", zorder=8)
ax.text(bx + bar + 220, by + 100, "Kilometers", fontsize=12, fontweight="bold", ha="left", va="center", zorder=8)

# legend
handles = [
    Line2D([], [], linestyle="none", label="Fiddler Crab Transects", **{**style["Fiddler Transect"], "mec": "black"}),
    Line2D([], [], linestyle="none", label="Food Web", **{**style["Food Web"], "ms": 16, "mew": 3.5}),
    Line2D([], [], linestyle="none", label="Water Quality", **style["Water Qualit"]),
]
leg = ax.legend(handles=handles, title="Sample Type", loc="lower left", bbox_to_anchor=(0.012, 0.085),
                frameon=True, framealpha=0.95, facecolor="white", edgecolor="black", fontsize=12.5,
                title_fontsize=13.5, labelspacing=0.9, borderpad=0.8, handletextpad=0.8)
leg.get_title().set_fontweight("bold")
leg.set_zorder(9)

# north arrow
ax.annotate("N", xy=(0.965, 0.115), xytext=(0.965, 0.045), xycoords="axes fraction", ha="center", va="center",
            fontsize=15, fontweight="bold", color="white", path_effects=halo,
            arrowprops=dict(arrowstyle="-|>", color="white", lw=2.2, mutation_scale=22,
                            path_effects=[pe.withStroke(linewidth=4, foreground="black")]))

# inset locator map, placed over open water so that it hides no sampling location
ix0, iy0, ix1, iy1 = inset["bbox3857"]
iw = 0.25
ih = iw * (iy1 - iy0) / (ix1 - ix0) * (x1 - x0) / (y1 - y0)
axi = ax.inset_axes([1 - iw, 1 - ih, iw, ih])
axi.imshow(Image.open(os.path.join(HERE, inset["file"])), extent=[ix0, ix1, iy0, iy1], interpolation="lanczos")
axi.set_xlim(ix0, ix1)
axi.set_ylim(iy0, iy1)
axi.set_xticks([])
axi.set_yticks([])
for s in axi.spines.values():
    s.set_linewidth(1.5)
pad_x, pad_y = 0.55 * (x1 - x0), 0.55 * (y1 - y0)   # box drawn larger than the map extent so it is visible
axi.add_patch(Rectangle((x0 - pad_x, y0 - pad_y), (x1 - x0) + 2 * pad_x, (y1 - y0) + 2 * pad_y,
                        fc="none", ec="#e3120b", lw=2.2, zorder=5))

os.makedirs(os.path.dirname(os.path.abspath(OUT)), exist_ok=True)
fig.savefig(OUT, dpi=300)
print("wrote", OUT, "size (in):", round(fig_w, 2), "x", round(fig_h, 2))
