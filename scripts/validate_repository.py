#!/usr/bin/env python3
"""Validate the LMS Analytics repository without altering data or running models."""

from __future__ import annotations

import sys
from pathlib import Path

REQUIRED_PATHS = (
    "README.md",
    "requirements.txt",
    "data/README.md",
    "docs/README.md",
    "docs/data_catalog.md",
    "docs/methodology_ru.md",
    "docs/literature_review_ru.md",
    "docs/sources_manifest.md",
    "docs/github_project_plan.md",
    "development/README.md",
    "report/README.md",
    "scripts/profile_lms_workbooks.py",
)


def main() -> int:
    root = Path(__file__).resolve().parents[1]
    errors: list[str] = []
    for relative_path in REQUIRED_PATHS:
        if not (root / relative_path).is_file():
            errors.append(f"Missing required file: {relative_path}")

    corrected_workbooks = sorted((root / "data").glob("*_correct.xlsx"))
    if not corrected_workbooks:
        errors.append("No *_correct.xlsx workbooks found in data/")

    if errors:
        print("Validation failed:")
        print("\n".join(f"- {error}" for error in errors))
        return 1

    print("Repository validation passed.")
    print(f"Corrected workbooks found: {len(corrected_workbooks)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
