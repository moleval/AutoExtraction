[CmdletBinding()]
param(
    [switch]$AutoCAD,
    [switch]$Quiet
)

$ErrorActionPreference = "Stop"

$Root    = Split-Path -Parent $PSScriptRoot
$checker = Join-Path $Root "tests\static_check.py"

if (-not $Quiet) {
    Write-Host "============================================="
    Write-Host " AutoExtraction test suite"
    Write-Host "============================================="
}

# ------------------------------------------------------------
# Определение Python
# ------------------------------------------------------------
$python     = $null
$pythonArgs = @()

# 1) Обычный python, но НЕ алиас WindowsApps
$cmd = Get-Command python -CommandType Application -ErrorAction SilentlyContinue |
       Where-Object { $_.Source -notmatch "WindowsApps" } |
       Select-Object -First 1
if ($cmd) {
    $python = $cmd.Source
}

# 2) py launcher с -3
if (-not $python) {
    $cmd = Get-Command py -CommandType Application -ErrorAction SilentlyContinue |
           Select-Object -First 1
    if ($cmd) {
        $python     = $cmd.Source
        $pythonArgs = @("-3")
    }
}

if (-not $python) {
    Write-Host "FAIL: Python interpreter not found (tried 'python' and 'py -3')"
    exit 1
}

if (-not $Quiet) {
    Write-Host "Python: $python $($pythonArgs -join ' ')"
}

# ------------------------------------------------------------
# Статическая проверка
# ------------------------------------------------------------
if (-not (Test-Path $checker)) {
    Write-Host "FAIL: checker not found: $checker"
    exit 1
}

& $python @pythonArgs $checker
$rc = $LASTEXITCODE

if ($rc -ne 0) {
    Write-Host ""
    Write-Host "RESULT: FAIL (static checks)"
    exit $rc
}

if (-not $Quiet) {
    Write-Host ""
    Write-Host "Static checks: PASS"
}

# ------------------------------------------------------------
# AutoCAD (опционально)
# ------------------------------------------------------------
if ($AutoCAD) {
    $scr     = Join-Path $Root "tests\run-tests.scr"
    $example = Join-Path $Root "tests\run-tests.scr.example"

    if (-not (Test-Path $scr)) {
        if (-not $Quiet) {
            Write-Host ""
            Write-Host "AutoCAD: $scr not found"
            Write-Host "  Copy the example:"
            Write-Host "    Copy-Item `"$example`" `"$scr`""
            Write-Host "RESULT: PASS (static only)"
        }
        exit 0
    }

    $acad    = $null
    $cands = @(
        "C:\Program Files\Autodesk\AutoCAD 2016\acad.exe",
        "C:\Program Files\Autodesk\AutoCAD 2020\acad.exe",
        "C:\Program Files\Autodesk\AutoCAD 2023\acad.exe",
        "C:\Program Files\Autodesk\AutoCAD 2024\acad.exe"
    )
    foreach ($c in $cands) {
        if (Test-Path $c) { $acad = $c; break }
    }

    if (-not $acad) {
        if (-not $Quiet) {
            Write-Host ""
            Write-Host "AutoCAD: not found in default paths"
            Write-Host "RESULT: PASS (static only)"
        }
        exit 0
    }

    if (-not $Quiet) {
        Write-Host ""
        Write-Host "AutoCAD: $acad"
        Write-Host "  SCR:   $scr"
        Write-Host "  NOTE:  AutoCAD runner is a placeholder for now."
        Write-Host "RESULT: PASS (static only)"
    }
    exit 0
}

if (-not $Quiet) {
    Write-Host ""
    Write-Host "RESULT: PASS"
}
exit 0