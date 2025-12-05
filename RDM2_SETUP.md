# RDM 2.0 Setup Guide

## CCC Dashboard Modernization

This app has been updated to work with the new **RDM 2.0** REDCap system.

---

## Prerequisites

### 1. Install gmed Package

The app now uses the `gmed` package for RDM 2.0 data loading:

```r
# In R or Positron console:
remotes::install_github('fbuckhold3/gmed')
```

### 2. Update Environment Variables

Add your **RDM 2.0 token** to `.Renviron.Renviron`:

```bash
# Edit .Renviron.Renviron
RDM_TOKEN=YOUR_ACTUAL_RDM2_TOKEN_HERE
```

Replace `YOUR_ACTUAL_RDM2_TOKEN_HERE` with your actual RDM 2.0 API token.

---

## What Changed?

### ✅ Data Loading
- **Old:** Used `imres` package with custom `forms_api_pull()`
- **New:** Uses `gmed::load_rdm_complete()` for RDM 2.0

### ✅ REDCap URL
- **Old:** `https://redcapsurvey.slu.edu/api/`
- **New:** `https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/`

### ✅ Field Updates
Only **3 fields** changed (minimal impact!):
- `archived` → `res_archive` *(automatic in gmed)*
- ❌ `ccc_mile_concerns` - Removed (doesn't exist in RDM 2.0)
- ❌ `ccc_concern_notes` - Removed (doesn't exist in RDM 2.0)

### ✅ Period Detection
- New helper functions use `gmed::get_current_period()`
- Auto-detects academic review periods

---

## Testing Checklist

Before deploying, test these key functions:

- [ ] App loads without errors
- [ ] Resident data loads correctly
- [ ] CCC review form displays
- [ ] Milestone data displays
- [ ] Can submit scheduled CCC reviews (instances 1-7)
- [ ] Can submit interim CCC reviews (instances 8+)

---

## Deployment to Posit Cloud

1. **Install gmed on Posit Cloud:**
   ```r
   remotes::install_github('fbuckhold3/gmed')
   ```

2. **Set RDM_TOKEN as environment variable** in Posit Cloud settings

3. **Deploy the app**

---

## Troubleshooting

### "RDM_TOKEN not set" error
➜ Make sure `.Renviron.Renviron` has `RDM_TOKEN=your_token` (not the placeholder)

### "Error loading RDM data"
➜ Check:
- RDM_TOKEN is correct
- Network connection to REDCap server
- gmed package is installed

### "Package 'gmed' not found"
➜ Install from GitHub:
```r
remotes::install_github('fbuckhold3/gmed')
```

---

## Files Modified

- `R/global.R` - Completely rewritten for RDM 2.0
- `R/server.R` - Removed references to deleted fields
- `R/ui.R` - Removed UI elements for deleted fields
- `.Renviron.Renviron` - Added RDM_TOKEN placeholder

## Backup

The old `global.R` is backed up as `R/global_old_backup.R` for reference.

---

**Questions?** Check the full modernization guide or review the git commit history.
