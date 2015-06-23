# topic_model_article
Repository for R scripts related to P-grains bibliometric/topic modeling article.

# Instructions

Within this repository, there are three **analysis scripts that each take the global bibliography divide it differently according to objective then clean the resulting corpora, analyze them for topic models, generate csv files with the most relevant topic terms, and generate graphs utilizing the metadata. These scripts call all other scripts (tags, graphs, custom_functions) to complete their tasks, meaning it's necessary to pull the full repository to replicate analyses. to expedite the editing of individual files such as graphs.R and custom_functions.R, workspaces have been produced for each individual analysis script - load that, then open the script to be edited. To see further description of each individual file, see the annotation at the header. 


# Searches

## Agricola

Notes: Boolean/Phrase for abstract available, all years, Journal Article type, using Zotero's import icon in the toolbar.

("long duration" OR "long-duration" OR perennial) N1 (cajanus OR "pigeon pea" OR pigeonpea OR "pigeon-pea")  
perennial N1 grain  
perennial N1 (oryza OR rice)  
perennial N1 (rye OR secale) NOT ("rye grass" OR ryegrass OR "rye-grass")  
perennial N1 sorghum  
perennial N1 (triticum OR wheat)  
ratoon* N5 (cajanus OR "pigeon pea" OR pigeonpea OR "pigeon-pea")  
ratoon* N5 (oryza OR rice)  
ratoon* N5 sorghum

## ScienceDirect

Notes: searched all fields, all years, refined to Journals using ScienceDirect's Export feature.

("long duration" or perennial) W/1 (cajanus OR "pigeon-pea" OR "pigeonpea" OR "pigeon pea")  
perennial W/1 grain  
perennial W/1 (oryza OR rice)  
perennial W/1 (rye OR secale) AND NOT ("rye grass" OR "rye-grass" OR ryegrass)  
perennial W/1 sorghum  
perennial W/1 (triticum OR wheat)  
ratoon* W/5 (cajanus OR "pigeon-pea" OR "pigeonpea" OR "pigeon pea")  
ratoon* W/5 (oryza OR rice)  
ratoon* W/5 sorghum

## Scopus

Notes: searched all fields, all years, refined to articles, exported as a RIS

ALL(("long duration" OR "long-duration" OR perennial) W/1 (cajanus OR "pigeon pea" OR "pigeon-pea" OR pigeonpea)) AND DOCTYPE(ar)  
ALL(perennial W/1 grain) AND DOCTYPE(ar)  
ALL(perennial W/1 (oryza OR rice)) AND DOCTYPE(ar)  
ALL(perennial W/1 (rye OR secale) AND NOT ("rye grass" OR "rye-grass" OR ryegrass)) AND DOCTYPE(ar)  
ALL(perennial W/1 sorghum) AND DOCTYPE (ar)  
ALL(perennial W/1 (triticum OR wheat)) AND DOCTYPE (ar)  
ALL(ratoon* W/5 (cajanus OR "pigeon pea" OR "pigeon-pea" OR pigeonpea)) AND DOCTYPE (ar)  
ALL(ratoon* W/5 (oryza OR rice)) AND DOCTYPE (ar)  
ALL(ratoon* W/5 sorghum) AND DOCTYPE(ar)

## Web of Science

Notes Selected Web of Sciences (All Databases), all years, refined to ARTICLE, and exported as RIS

TS=(("long duration" OR perennial) NEAR/1 (cajanus OR "pigeon$pea"))  
TS=(perennial NEAR/1 grain)  
TS=(perennial NEAR/1 (oryza OR rice))  
TS=(perennial NEAR/1 (rye OR secale) NOT "rye$grass")  
TS=(perennial NEAR/1 sorghum)  
TS=(perennial NEAR/1 (triticum OR wheat))  
TS=(ratoon* NEAR/5 (cajanus OR "pigeon$pea"))  
TS=(ratoon* NEAR/5 (oryza OR rice))  
TS=(ratoon* NEAR/5 sorghum)
