-- run duckdb -c ".read part1.sql"

with lines as (
    -- set file to `test.txt` or `input.txt`
    select regexp_split_to_table(content, '\n') as line from read_text('input.txt')
),
indexed_lines as (
    select row_number() OVER () - 1 as i, line from lines 
    where line != ''
),
exploded_rows as (
    select 
        i, 
        line, 
        len(line) as width,
        regexp_split_to_table(line, '') as cell
    from indexed_lines
),
indexed_cells as (
    select 
        i, 
        ((row_number() OVER ())-1) % width as j,
        cell
    from exploded_rows
),
frequencies as (
    select DISTINCT cell from indexed_cells
    where cell != '.'
),
interferring_pairs as (
    select A.i as iA, B.i as iB, A.cell as cell, A.j as jA, B.j as jB from indexed_cells as A
    full outer join indexed_cells as B on A.cell = B.cell
    where A.cell != '.' and (A.i != B.i or A.j != B.j)
),
antinodes as (
    select 
        iA - (iB - iA) as iAnti,
        jA - (jB - jA) as jAnti
    from interferring_pairs
    where iAnti >= 0
      and jAnti >= 0
      and iAnti < (select max(i)+1 from exploded_rows)
      and jAnti < ((select count(*) from indexed_cells) / (select max(i)+1 from exploded_rows))
),
distinct_antinodes as (
    select DISTINCT (iAnti, jAnti) from antinodes
)

select 
*
from distinct_antinodes



;

