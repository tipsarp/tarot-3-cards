# Simple Digital Tarot Reading Platform

A minimal, self-contained R Shiny web application for pulling three-card tarot readings. Designed to run completely out-of-the-box with zero external UI packages or styling frameworks.

## Features

- **Three-Card Spread:** Randomly shuffles and draws three tarot cards from the Major and Minor Arcana.
- **Card Reversals:** Optional toggle to include reversed cards (with a 50% random chance per card) complete with inverted meanings and rotated display visuals.
- **Copy to Clipboard:** One-click button to easily copy your text reading format.
- **Rich Reading Details:** Comprehensive breakdown at the bottom covering numerology, elemental associations, astrological signs/planets, and specific upright or reversed interpretations.

---

## Project Structure

Your working directory should look like this:
```text
├── app.R
└── cards
        └── ... (Tarot card images)
```

---

## Prerequisites & Installation

To run this application locally, you need [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/) installed on your machine.

**Install Shiny:**
   Open your R console and run the following command to install the required package:
   ```R
   install.packages("shiny")
   ```

## Running the App

1. Open `app.R` in RStudio.
2. Click the **Run App** button in the top right of the source editor, or execute the following snippet in your R console:
   ```R
   shiny::runApp("app.R")
   ```
