$ErrorActionPreference = 'Stop'

$repoRoot = 'c:\Users\jn03823\University of Georgia\Nelson Lab UGA Marine Science-Jimmy Nelson - Jimmy Nelson\Manuscripts\NCC_PIE\NatureComms_Analysis_Repository_2026-03-08_v2'
$sourcePath = Join-Path $repoRoot 'manuscript\NCC_Fiddler_Niche_submission_ready_2026-03-24.txt'
$docPath = Join-Path $repoRoot 'manuscript\NCC_Fiddler_Niche_submission_ready.docx'
$backupPath = Join-Path $repoRoot 'manuscript\NCC_Fiddler_Niche_submission_ready.pre_refresh_backup.docx'
$tempDocPath = Join-Path $repoRoot 'manuscript\NCC_Fiddler_Niche_submission_ready.rebuilt.docx'
$figDir = Join-Path $repoRoot 'figures'

$wdAlignParagraphLeft = 0
$wdAlignParagraphCenter = 1
$wdLineSpaceDouble = 2
$wdLineNumberingContinuous = 0
$wdFormatXMLDocument = 12

$figureMap = @{
    'Figure 1:' = Join-Path $figDir 'pie_anomalies_panel.png'
    'Figure 2:' = Join-Path $figDir 'fig_burrow_density_trend.png'
    'Figure 3:' = Join-Path $figDir 'fig_plankton_trends_nyt_with_fiddler.png'
}

$topLevelHeadings = @(
    'Abstract',
    'Introduction',
    'Results',
    'Discussion',
    'Methods',
    'References',
    'Data availability',
    'Code availability',
    'Figures'
)

$subHeadings = @(
    'Changes in estuarine physical parameters',
    'Freshwater discharge',
    'Fiddler crab burrow counts',
    'Increasing marine plankton support of PIE food webs',
    'Plum Island Ecosystems (PIE) LTER, Massachusetts, USA.',
    'Estuary hydrography',
    'Trend estimation',
    'Crab burrow density',
    'Food web analysis'
)

function Add-Paragraph {
    param(
        [Parameter(Mandatory = $true)][object]$Selection,
        [Parameter(Mandatory = $true)][string]$Text,
        [int]$Alignment = $wdAlignParagraphLeft,
        [bool]$Bold = $false,
        [int]$FontSize = 12,
        [int]$SpaceAfter = 6,
        [int]$LeftIndent = 0,
        [int]$FirstLineIndent = 0,
        [bool]$AddBlankLineAfter = $false
    )

    $Selection.Font.Name = 'Times New Roman'
    $Selection.Font.Size = $FontSize
    $Selection.Font.Bold = if ($Bold) { 1 } else { 0 }
    $Selection.ParagraphFormat.Alignment = $Alignment
    $Selection.ParagraphFormat.LineSpacingRule = $wdLineSpaceDouble
    $Selection.ParagraphFormat.SpaceAfter = $SpaceAfter
    $Selection.ParagraphFormat.LeftIndent = $LeftIndent
    $Selection.ParagraphFormat.FirstLineIndent = $FirstLineIndent
    $Selection.TypeText($Text)
    $Selection.TypeParagraph()
    if ($AddBlankLineAfter) {
        $Selection.TypeParagraph()
    }
}

Copy-Item $docPath $backupPath -Force

$text = Get-Content -LiteralPath $sourcePath -Raw -Encoding UTF8
$lines = $text -split "`r?`n"

$word = $null
$doc = $null

try {
    $word = New-Object -ComObject Word.Application
    $word.Visible = $false
    $word.DisplayAlerts = 0
    $doc = $word.Documents.Add()
    $selection = $word.Selection

    $doc.PageSetup.TopMargin = $word.InchesToPoints(1)
    $doc.PageSetup.BottomMargin = $word.InchesToPoints(1)
    $doc.PageSetup.LeftMargin = $word.InchesToPoints(1)
    $doc.PageSetup.RightMargin = $word.InchesToPoints(1)
    $doc.PageSetup.LineNumbering.Active = $true
    $doc.PageSetup.LineNumbering.CountBy = 1
    $doc.PageSetup.LineNumbering.RestartMode = $wdLineNumberingContinuous
    $doc.PageSetup.LineNumbering.DistanceFromText = $word.InchesToPoints(0.2)

    $inAuthors = $false
    $inAffiliations = $false

    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i].TrimEnd()
        $trimmed = $line.Trim()

        if ([string]::IsNullOrWhiteSpace($trimmed)) {
            $selection.TypeParagraph()
            continue
        }

        if ($trimmed -eq 'Supplementary Figures:') {
            continue
        }

        if ($i -eq 0) {
            Add-Paragraph -Selection $selection -Text $trimmed -Alignment $wdAlignParagraphCenter -Bold $true -FontSize 16 -SpaceAfter 10 -AddBlankLineAfter $true
            continue
        }

        if ($trimmed -eq 'Authors:') {
            $inAuthors = $true
            $inAffiliations = $false
            continue
        }

        if ($trimmed -eq 'Affiliations:') {
            $inAuthors = $false
            $inAffiliations = $true
            continue
        }

        if ($topLevelHeadings -contains $trimmed) {
            $inAuthors = $false
            $inAffiliations = $false
            Add-Paragraph -Selection $selection -Text $trimmed -Alignment $wdAlignParagraphLeft -Bold $true -FontSize 13 -SpaceAfter 6
            continue
        }

        if ($subHeadings -contains $trimmed) {
            Add-Paragraph -Selection $selection -Text $trimmed -Alignment $wdAlignParagraphLeft -Bold $true -FontSize 12 -SpaceAfter 4
            continue
        }

        if ($trimmed -match '^Figure\s+[0-9]+:') {
            Add-Paragraph -Selection $selection -Text $trimmed -Alignment $wdAlignParagraphLeft -Bold $false -FontSize 11 -SpaceAfter 6
            $captionKey = $trimmed.Substring(0, 9)
            if ($figureMap.ContainsKey($captionKey) -and (Test-Path -LiteralPath $figureMap[$captionKey])) {
                $selection.ParagraphFormat.Alignment = $wdAlignParagraphCenter
                $shape = $selection.InlineShapes.AddPicture($figureMap[$captionKey])
                if ($shape.Width -gt $word.InchesToPoints(6.2)) {
                    $shape.LockAspectRatio = $true
                    $shape.Width = $word.InchesToPoints(6.2)
                }
                $selection.TypeParagraph()
                $selection.TypeParagraph()
            }
            continue
        }

        if ($inAuthors -or $inAffiliations) {
            Add-Paragraph -Selection $selection -Text $trimmed -Alignment $wdAlignParagraphCenter -Bold $false -FontSize 11 -SpaceAfter 0
            continue
        }

        Add-Paragraph -Selection $selection -Text $trimmed -Alignment $wdAlignParagraphLeft -Bold $false -FontSize 12 -SpaceAfter 0
    }

    $doc.SaveAs2($tempDocPath, $wdFormatXMLDocument)
    Write-Output 'DOCX_REBUILT'
}
finally {
    if ($doc -ne $null) {
        $doc.Close($false)
    }
    if ($word -ne $null) {
        $word.Quit()
    }
}

[System.IO.File]::Copy($tempDocPath, $docPath, $true)
