# Ant Colony Network Analysis Report

## Network Statistics
* Number of ants: 67
* Number of interactions: 159
* Network density: 0.036
* Two-way interactions: 0.126
* Average steps between ants: 5.101
* Group formation: 0.148

## Most Connected Ants
### Top 5 by Total Connections:
    ant degree degree_in degree_out betweenness   closeness eigenvector clustering_coef
636 636     12         7          5    489.6357 0.004878049   0.4646298      0.05454545
389 389     11         4          7    719.5107 0.005405405   0.5283054      0.10909091
543 543     11         4          7    261.4179 0.004587156   0.5997972      0.14545455
136 136     10         2          8    491.8167 0.005464481   0.3680304      0.06666667
259 259     10         7          3    203.4226 0.003236246   0.4365513      0.19444444

### Top 5 by Network Position (Betweenness):
    ant degree degree_in degree_out betweenness   closeness eigenvector clustering_coef
166 166      7         4          3    722.2119 0.004424779   0.3087613       0.0000000
389 389     11         4          7    719.5107 0.005405405   0.5283054       0.1090909
67   67      8         1          7    677.2119 0.005649718   0.2951086       0.1428571
293 293      9         3          6    657.2012 0.005494505   0.3286907       0.1785714
15   15      7         3          4    586.8190 0.004587156   0.3141378       0.1904762

## Forager Analysis
* Number of Foragers: 11
* Number of Nurses: 56

### Forager Details:
Ants that go outside (with outside trip frequency):
# A tibble: 11 × 4
   Individual Inside Outside percent_outside
        <int>  <int>   <int>           <dbl>
 1        459      0      13          100   
 2        643      1      12           92.3 
 3        230     21      13           38.2 
 4        166     26      16           38.1 
 5        480     15       9           37.5 
 6        397     11       4           26.7 
 7        259     22       7           24.1 
 8        228     19       3           13.6 
 9        366     16       2           11.1 
10        236     30       1            3.23
11         67     31       1            3.12

### Food Sharing Patterns:
Overall sharing by role:
# A tibble: 2 × 4
  role    n_individuals total_sharing sharing_per_ant
  <chr>           <int>         <int>           <dbl>
1 Forager             8            15            1.88
2 Nurse              28            48            1.71

Sharing directions (who gives to who):
# A tibble: 4 × 3
  giver_role receiver_role count
  <chr>      <chr>         <int>
1 Forager    Forager           8
2 Forager    Nurse             7
3 Nurse      Forager          13
4 Nurse      Nurse            35

### Network Position Comparison:
# A tibble: 2 × 5
  role    n_ants avg_connections avg_betweenness avg_closeness
  <chr>    <int>           <dbl>           <dbl>         <dbl>
1 Forager     11            5.27            222.       0.00349
2 Nurse       56            4.64            132.     NaN      
