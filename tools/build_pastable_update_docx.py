from pathlib import Path

from docx import Document
from docx.enum.section import WD_SECTION_START
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt


REPO_ROOT = Path(__file__).resolve().parents[1]
SOURCE_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_submission_ready_with_map_2026-03-25.txt"
DOC_PATH = REPO_ROOT / "manuscript" / "NCC_Fiddler_Niche_pastable_updates_2026-03-26.docx"

FIGURE_MAP = {
    "Figure 3:": REPO_ROOT / "figures" / "fig_burrow_panel.png",
    "Figure 4:": REPO_ROOT / "figures" / "fig_plankton_trends_nyt_with_fiddler.png",
}

REPLACEMENT_NOTES = [
    "Burrow panel: Figure 3 in the with-map manuscript, Figure 2 in the no-map manuscript.",
    "Plankton panel: Figure 4 in the with-map manuscript, Figure 3 in the no-map manuscript.",
    "Use this document as a paste-in packet only; it does not modify the main submission manuscript.",
]


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


def add_paragraph(document, text, *, align, size, bold=False, space_after=0):
    paragraph = document.add_paragraph()
    paragraph.alignment = align
    paragraph.paragraph_format.space_after = Pt(space_after)
    paragraph.paragraph_format.line_spacing = 2
    run = paragraph.add_run(text)
    set_run_font(run, size, bold=bold)
    return paragraph


def normalize_lines(path: Path):
    return [line.rstrip() for line in path.read_text(encoding="utf-8").splitlines()]


def find_index(lines, heading, *, prefix=False):
    for index, line in enumerate(lines):
        candidate = line.strip()
        if prefix and candidate.startswith(heading):
            return index
        if candidate == heading:
            return index
    raise ValueError(f"Heading not found: {heading}")


def slice_block(lines, start_heading, end_heading):
    start = find_index(lines, start_heading)
    end = find_index(lines, end_heading)
    return [line for line in lines[start:end] if line.strip()]


def slice_figure_caption(lines, figure_heading, next_heading):
    start = find_index(lines, figure_heading, prefix=True)
    end = find_index(lines, next_heading, prefix=True)
    return [line for line in lines[start:end] if line.strip()]


def add_image(document, path):
    if not path.exists():
        return
    paragraph = document.add_paragraph()
    paragraph.alignment = WD_ALIGN_PARAGRAPH.CENTER
    paragraph.add_run().add_picture(str(path), width=Inches(6.2))


def add_bulleted_note(document, text):
    paragraph = document.add_paragraph(style="List Bullet")
    paragraph.paragraph_format.line_spacing = 2
    run = paragraph.add_run(text)
    set_run_font(run, 11, bold=False)


def add_block(document, title, lines):
    add_paragraph(document, title, align=WD_ALIGN_PARAGRAPH.LEFT, size=12, bold=True, space_after=4)
    for index, line in enumerate(lines):
        add_paragraph(
            document,
            line.strip(),
            align=WD_ALIGN_PARAGRAPH.LEFT,
            size=12,
            bold=(index == 0 and not line.startswith("     ")),
            space_after=0,
        )
    document.add_paragraph()


def build_document():
    lines = normalize_lines(SOURCE_PATH)

    results_blocks = []
    results_blocks.append(slice_block(lines, "Fiddler crab burrow counts", "Increasing marine plankton support of PIE food webs"))
    results_blocks.append(slice_block(lines, "Increasing marine plankton support of PIE food webs", "Discussion"))
    discussion_block = slice_block(lines, "Discussion", "Methods")
    methods_blocks = []
    methods_blocks.append(slice_block(lines, "Crab burrow density", "Food web analysis"))
    methods_blocks.append(slice_block(lines, "Food web analysis", "References"))
    figure_3_caption = slice_figure_caption(lines, "Figure 3:", "Figure 4:")
    figure_4_caption = slice_figure_caption(lines, "Figure 4:", "Supplementary Figures")

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

    add_paragraph(
        document,
        "NCC Fiddler Manuscript Paste-In Updates",
        align=WD_ALIGN_PARAGRAPH.CENTER,
        size=16,
        bold=True,
        space_after=10,
    )
    add_paragraph(
        document,
        "Source: manuscript/NCC_Fiddler_Niche_submission_ready_with_map_2026-03-25.txt.",
        align=WD_ALIGN_PARAGRAPH.LEFT,
        size=11,
        bold=False,
        space_after=8,
    )
    for note in REPLACEMENT_NOTES:
        add_bulleted_note(document, note)

    document.add_paragraph()

    add_paragraph(document, "Updated Figures", align=WD_ALIGN_PARAGRAPH.LEFT, size=13, bold=True, space_after=6)
    add_paragraph(document, "Replacement Figure: Burrow panel", align=WD_ALIGN_PARAGRAPH.LEFT, size=12, bold=True, space_after=4)
    for line in figure_3_caption:
        add_paragraph(document, line.strip(), align=WD_ALIGN_PARAGRAPH.LEFT, size=11, bold=False, space_after=2)
    add_image(document, FIGURE_MAP["Figure 3:"])
    document.add_paragraph()
    add_paragraph(document, "Replacement Figure: Plankton panel", align=WD_ALIGN_PARAGRAPH.LEFT, size=12, bold=True, space_after=4)
    for line in figure_4_caption:
        add_paragraph(document, line.strip(), align=WD_ALIGN_PARAGRAPH.LEFT, size=11, bold=False, space_after=2)
    add_image(document, FIGURE_MAP["Figure 4:"])

    document.add_section(WD_SECTION_START.NEW_PAGE)
    add_line_numbering(document.sections[-1])

    add_paragraph(document, "Updated Results", align=WD_ALIGN_PARAGRAPH.LEFT, size=13, bold=True, space_after=6)
    add_block(document, "Replace Results subsection 1", results_blocks[0])
    add_block(document, "Replace Results subsection 2", results_blocks[1])

    add_paragraph(document, "Updated Discussion", align=WD_ALIGN_PARAGRAPH.LEFT, size=13, bold=True, space_after=6)
    add_block(document, "Replace Discussion section", discussion_block)

    document.add_section(WD_SECTION_START.NEW_PAGE)
    add_line_numbering(document.sections[-1])

    add_paragraph(document, "Updated Methods", align=WD_ALIGN_PARAGRAPH.LEFT, size=13, bold=True, space_after=6)
    add_block(document, "Replace Methods subsection 1", methods_blocks[0])
    add_block(document, "Replace Methods subsection 2", methods_blocks[1])

    document.save(DOC_PATH)
    print(DOC_PATH)


if __name__ == "__main__":
    build_document()