# frequencies

An open-source (GPL-3) R package to create frequency tables which display both counts and rates. 
All comments and ideas are welcome. Please submit any bugs to [Issues](https://github.com/DataInsightPartners/frequencies/issues)

### `freq_vect`
  - Function that takes one atomic vector and creates a frequency table with counts and rates of
  the element in the vector.
  - This is useful when exploring a new dataset and trying to get both counts of elements and 
  how much of the whole they represent.
  - The user has the ability to choose to add a summary line and whether to sort by count  of 
  elements or by element name.
  
### `freq_two_vects`
  - Function that takes a data frame and the names of two columns and returns a data frame
  containing frequency tables with counts and percentages of column 2 within column 1.
  - In our work in the education sector this is useful for investigating things like proficiency
  within schools, subgroups within programs, grades by ethnicity, etc.
  - The user has the ability to have a single data frame or a seperate data frame for each element
  of column 1.
