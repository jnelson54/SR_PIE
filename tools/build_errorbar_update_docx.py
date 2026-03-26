# This script builds a docx with updated error bar language and figures for review.
from pathlib import Path
import shutil
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt

REPO_ROOT = Path(r"c:\Users\jn03823\University of Georgia\Nelson Lab UGA Marine Science-Jimmy Nelson - Jimmy Nelson\Manuscripts\NCC_PIE\NatureComms_Analysis_Repository_2026-03-08_v2")
SOURCE_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_submission_ready_with_map_2026-03-25.txt"
DOC_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_errorbar_update.docx"

FIGURE_MAP = {
    "Figure 1:": REPO_ROOT / "figures" / "fig_study_area_map.png",
    "Figure 2:": REPO_ROOT / "figures" / "pie_anomalies_panel.png",
    "Figure 3:": REPO_ROOT / "figures" / "fig_burrow_panel.png",
    "Figure 4:": REPO_ROOT / "figures" / "fig_plankton_trends_nyt_with_fiddler.png",
}

SUPPLEMENTARY_FIGURE_MAP = {
    "Supplementary Figure 1": REPO_ROOT / "figures" / "Supplementary_Figure1_parker_discharge.png",
}

# Use the same logic as the main docx builder, but output to a new file
import sys
sys.path.append(str(REPO_ROOT / "tools"))
from build_submission_docx_with_map import build_docx

if __name__ == "__main__":
    build_docx(
        source_path=SOURCE_PATH,
        doc_path=DOC_PATH,
        figure_map=FIGURE_MAP,
        supplementary_figure_map=SUPPLEMENTARY_FIGURE_MAP,
        top_level_headings=None,
        backup_path=None
    )
    print(f"Wrote updated docx with error bar language and figures: {DOC_PATH}")
