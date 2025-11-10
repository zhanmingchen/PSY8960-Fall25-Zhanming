This is a template directory structure to use as a starting point for projects.

It consists of the minimum set of folders and files for any project. Specific projects will likely require additional folders and files.

This README file is meant to be "outward facing," largely for external people to know how to navigate the directory.

The directory structure is as follows (with descriptions of what might go in each)

{Project_Name}\ |--- Admin/ (Project administration, correspondence, participant logs)\       |--- Ethics/ (IRB application and approval, staff training certificates)\       |--- Admin_Notes.txt\ |--- Archives/ (any non active materials kept for posterity)\       |--- Archives_Notes.txt\ |--- Code/ (code/scripts for cleaning, wrangling, analysis, etc.)\       |--- Code_Notes.txt\ |--- Data/ (all project data)\       |--- Raw_Data/ (best to keep raw data separate, possibly read-only)\       |--- Data_Notes.txt\ |--- Procedure/ (study-related materials, protocols, measures, manuals)\       |--- Procedure_Notes.txt\ |--- Reports/ (manuscripts for publication, interim summaries, conference presentations)\       |--- Preregistration/ (final version of posted prereg and related materials)\       |--- Reports_Notes.txt\ Notes_to_File.txt\ README.md\

The "Notes to File" and "Notes" files within each sub-directory are meant to be "inward-facing," i.e., notes for the project team. Accordingly, you may want to add these to .gitignore so that they don't appear in the public repo.