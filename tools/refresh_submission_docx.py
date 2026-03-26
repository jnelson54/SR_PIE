from pathlib import Path
import shutil

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt


REPO_ROOT = Path(r"c:\Users\jn03823\University of Georgia\Nelson Lab UGA Marine Science-Jimmy Nelson - Jimmy Nelson\Manuscripts\NCC_PIE\NatureComms_Analysis_Repository_2026-03-08_v2")
SOURCE_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_submission_ready_2026-03-24.txt"
DOC_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_submission_ready.docx"
BACKUP_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_submission_ready.pre_refresh_backup.docx"

FIGURE_MAP = {
    "Figure 1:": REPO_ROOT / "figures" / "pie_anomalies_panel.png",
    "Figure 2:": REPO_ROOT / "figures" / "fig_burrow_panel.png",
    "Figure 3:": REPO_ROOT / "figures" / "fig_plankton_trends_nyt_with_fiddler.png",
}

SUPPLEMENTARY_FIGURE_MAP = {
    "Supplementary Figure 1": REPO_ROOT / "figures" / "Supplementary_Figure1_parker_discharge.png",
}

TOP_LEVEL_HEADINGS = {
    "Abstract",
    "Introduction",
    "Results",
    "Discussion",
    "Methods",
    "References",
    "Data availability",
    "Code availability",
    "Author contributions",
    "Competing interests",
    "Supplementary materials",
    "Figures",
}

SUBHEADINGS = {
    "Changes in estuarine physical parameters",
    "Freshwater discharge",
    "Fiddler crab burrow counts",
    "Increasing marine plankton support of PIE food webs",
    "Plum Island Ecosystems (PIE) LTER, Massachusetts, USA.",
    "Estuary hydrography",
    "Trend estimation",
    "Crab burrow density",
    "Food web analysis",
}


def add_line_numbering(section):
    sect_pr = section._sectPr
    existing = sect_pr.find(qn("w:lnNumType"))
    if existing is not None:
        sect_pr.remove(existing)
    line_numbers = OxmlElement("w:lnNumType")
    line_numbers.set(qn("w:countBy"), "1")
    line_numbers.set(qn("w:distance"), "240")
    line_numbers.set(qn("w:restart"), "continuous")
    sect_pr.append(line_numbers)


def set_run_font(run, size, bold=False):
    run.font.name = "Times New Roman"
    run._element.rPr.rFonts.set(qn("w:eastAsia"), "Times New Roman")
    run.font.size = Pt(size)
    run.bold = bold


def add_paragraph(document, text, *, align, size, bold=False, space_after=0, blank_after=False):
    paragraph = document.add_paragraph()
    paragraph.alignment = align
    paragraph.paragraph_format.space_after = Pt(space_after)
    paragraph.paragraph_format.line_spacing = 2
    run = paragraph.add_run(text)
    set_run_font(run, size, bold=bold)
    if blank_after:
        document.add_paragraph()
    return paragraph


def build_document():
    lines = SOURCE_PATH.read_text(encoding="utf-8").splitlines()

    if DOC_PATH.exists():
        shutil.copy2(DOC_PATH, BACKUP_PATH)

    document = Document()
    section = document.sections[0]
    section.top_margin = Inches(1)
    section.bottom_margin = Inches(1)
    section.left_margin = Inches(1)
    section.right_margin = Inches(1)
    add_line_numbering(section)

    normal_style = document.styles["Normal"]
    normal_style.font.name = "Times New Roman"
    normal_style._element.rPr.rFonts.set(qn("w:eastAsia"), "Times New Roman")
    normal_style.font.size = Pt(12)

    in_authors = False
    in_affiliations = False

    for index, raw_line in enumerate(lines):
        trimmed = raw_line.strip()

        if not trimmed:
            document.add_paragraph()
            continue

        if index == 0:
            add_paragraph(
                document,
                trimmed,
                align=WD_ALIGN_PARAGRAPH.CENTER,
                size=16,
                bold=True,
                space_after=10,
                blank_after=True,
            )
            continue

        if trimmed == "Authors:":
            in_authors = True
            in_affiliations = False
            continue

        if trimmed == "Affiliations:":
            in_authors = False
            in_affiliations = True
            continue

        if trimmed in TOP_LEVEL_HEADINGS:
            in_authors = False
            in_affiliations = False
            add_paragraph(
                document,
                trimmed,
                align=WD_ALIGN_PARAGRAPH.LEFT,
                size=13,
                bold=True,
                space_after=6,
            )
            continue

        if trimmed in SUBHEADINGS:
            add_paragraph(
                document,
                trimmed,
                align=WD_ALIGN_PARAGRAPH.LEFT,
                size=12,
                bold=True,
                space_after=4,
            )
            continue

        if trimmed.startswith("Figure ") and ":" in trimmed:
            add_paragraph(
                document,
                trimmed,
                align=WD_ALIGN_PARAGRAPH.LEFT,
                size=11,
                bold=False,
                space_after=6,
            )
            figure_key = trimmed[:9]
            figure_path = FIGURE_MAP.get(figure_key)
            if figure_path and figure_path.exists():
                figure_paragraph = document.add_paragraph()
                figure_paragraph.alignment = WD_ALIGN_PARAGRAPH.CENTER
                figure_paragraph.add_run().add_picture(str(figure_path), width=Inches(6.2))
                document.add_paragraph()
            continue

        if trimmed.startswith("Supplementary Figure "):
            add_paragraph(
                document,
                trimmed,
                align=WD_ALIGN_PARAGRAPH.LEFT,
                size=11,
                bold=False,
                space_after=6,
            )
            figure_key = trimmed.split(" reports", 1)[0]
            figure_path = SUPPLEMENTARY_FIGURE_MAP.get(figure_key)
            if figure_path and figure_path.exists():
                figure_paragraph = document.add_paragraph()
                figure_paragraph.alignment = WD_ALIGN_PARAGRAPH.CENTER
                figure_paragraph.add_run().add_picture(str(figure_path), width=Inches(6.2))
                document.add_paragraph()
            continue

        if in_authors or in_affiliations:
            add_paragraph(
                document,
                trimmed,
                align=WD_ALIGN_PARAGRAPH.CENTER,
                size=11,
                bold=False,
                space_after=0,
            )
            continue

        add_paragraph(
            document,
            trimmed,
            align=WD_ALIGN_PARAGRAPH.LEFT,
            size=12,
            bold=False,
            space_after=0,
        )

    document.save(DOC_PATH)


if __name__ == "__main__":
    build_document()
    print("DOCX_REBUILT")