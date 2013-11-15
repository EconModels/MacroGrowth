#! /bin/bash
ssh node-01 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK -e Q -m cese-\(kl\)e -C -p > node-01.txt" &
ssh node-02 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c JP,CN -e Q -m cese-\(kl\)e -C -p > node-02.txt" &
ssh node-03 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZA,SA -e Q -m cese-\(kl\)e -C -p > node-03.txt" &
ssh node-04 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ -e Q -m cese-\(kl\)e -C -p > node-04.txt" &
ssh node-05 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZM -e Q -m cese-\(kl\)e -C -p > node-05.txt" &
ssh node-06 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK -e Q -m cese-\(le\)k -C -p > node-06.txt" &
ssh node-07 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c JP,CN -e Q -m cese-\(le\)k -C -p > node-07.txt" &
ssh node-09 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZA,SA -e Q -m cese-\(le\)k -C -p > node-09.txt" &
ssh node-10 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ -e Q -m cese-\(le\)k -C -p > node-10.txt" &
ssh node-11 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZM -e Q -m cese-\(le\)k -C -p > node-11.txt" &
ssh node-12 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK -e Q -m cese-\(ek\)l -C -p > node-12.txt" &
ssh node-13 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c JP,CN -e Q -m cese-\(ek\)l -C -p > node-13.txt" & 
ssh node-14 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZA,SA -e Q -m cese-\(ek\)l -C -p > node-14.txt" &
ssh node-15 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ -e Q -m cese-\(ek\)l -C -p > node-15.txt" &
ssh node-16 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZM -e Q -m cese-\(ek\)l -C -p > node-16.txt" &
