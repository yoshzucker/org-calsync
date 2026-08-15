<#
.SYNOPSIS
  Open an Outlook compose window with a file attached (does NOT send).

.DESCRIPTION
  Uses Outlook COM (the sanctioned mailer) to create a new mail with ATTACHMENT
  and Display() it, so the user fills in the recipient and sends manually.  A
  plain mailto: URL cannot attach a file, hence COM.  Attaches to the running
  Outlook and never calls Quit().  Windows + Outlook required.

.PARAMETER Attachment
  Path to the file to attach (required).

.PARAMETER Subject
  Mail subject (optional).

.PARAMETER Body
  Mail body text (optional).

.EXAMPLE
  .\outlook-compose.ps1 -Attachment C:\Temp\busy.ics -Subject 'Busy'
#>
param(
    [Parameter(Mandatory = $true)][string]$Attachment,
    [string]$Subject = '',
    [string]$Body = ''
)

if ($env:OS -ne 'Windows_NT') {
    Write-Error 'outlook-compose requires Windows with Outlook installed.'
    exit 1
}
if (-not (Test-Path $Attachment)) {
    Write-Error "Attachment not found: $Attachment"
    exit 1
}
$Attachment = (Resolve-Path $Attachment).Path

$outlook = New-Object -ComObject Outlook.Application
try {
    $mail = $outlook.CreateItem(0)   # olMailItem
    $mail.Subject = $Subject
    $mail.Body = $Body
    [void]$mail.Attachments.Add($Attachment)
    $mail.Display()   # open the compose window; do NOT send
    Write-Output "Composed with $Attachment"
} finally {
    # Do NOT Quit(): $outlook is the user's running Outlook.
    if ($mail) {
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($mail) | Out-Null
    }
    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($outlook) | Out-Null
    [System.GC]::Collect()
}
