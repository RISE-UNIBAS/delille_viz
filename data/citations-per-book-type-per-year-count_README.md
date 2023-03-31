The data in `citations-per-book-type-per-year-count` serve to produce **two visualisations**:
- one for the main categories
	- Dictionnaires
	- Anthologies et manuels
	- Ouvrages littéraires
	- Science et vulgarisation
	- Autres
- one for the subcategories of the *Ouvrages littéraires*:
	- Textes en vers
	- Roman, récit fictionnel
	- Esthétique, poétique, histoire littéraire
	- Mémoires, biographies, histoire, etc.
	- Autres essais

The '**publicationYear**' is to be used for building the visualisation. The 'publicationPeriod' is only included for reference. 'publicationYear' is the starting year of 'publicationPeriod', which makes sense to build the cumulative visualisation.

The **name of the categories** can be reconstructed by replacing (in this order):
- `___` (3 underscores) with `, ` (comma and single space)
- `_` (single underscore) with ` `(single space)


