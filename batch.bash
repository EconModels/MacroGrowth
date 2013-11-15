#! /bin/bash
ssh node-01 "cd github/Econ-Summer2013; ./batchEcon.R -c US,UK -e Q -m cese-(kl)e -C"
ssh node-02 "cd github/Econ-Summer2013; ./batchEcon.R -c JP,CN -e Q -m cese-(kl)e -C"
ssh node-03 "cd github/Econ-Summer2013; ./batchEcon.R -c ZA,SA -e Q -m cese-(kl)e -C"
ssh node-04 "cd github/Econ-Summer2013; ./batchEcon.R -c IR,TZ -e Q -m cese-(kl)e -C"
ssh node-05 "cd github/Econ-Summer2013; ./batchEcon.R -c ZM -e Q -m cese-(kl)e -C"

ssh node-06 "cd github/Econ-Summer2013; ./batchEcon.R -c US,UK -e Q -m cese-(le)k -C"
ssh node-07 "cd github/Econ-Summer2013; ./batchEcon.R -c JP,CN -e Q -m cese-(le)k -C"
ssh node-09 "cd github/Econ-Summer2013; ./batchEcon.R -c ZA,SA -e Q -m cese-(le)k -C"
ssh node-10 "cd github/Econ-Summer2013; ./batchEcon.R -c IR,TZ -e Q -m cese-(le)k -C"
ssh node-11 "cd github/Econ-Summer2013; ./batchEcon.R -c ZM -e Q -m cese-(le)k -C"

ssh node-12 "cd github/Econ-Summer2013; ./batchEcon.R -c US,UK -e Q -m cese-(ek)l -C"
ssh node-13 "cd github/Econ-Summer2013; ./batchEcon.R -c JP,CN -e Q -m cese-(ek)l -C"
ssh node-14 "cd github/Econ-Summer2013; ./batchEcon.R -c ZA,SA -e Q -m cese-(ek)l -C"
ssh node-15 "cd github/Econ-Summer2013; ./batchEcon.R -c IR,TZ -e Q -m cese-(ek)l -C"
ssh node-16 "cd github/Econ-Summer2013; ./batchEcon.R -c ZM -e Q -m cese-(ek)l -C"
