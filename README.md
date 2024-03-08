# BTP-1

- Singular Spectral Analysis is the most conveniently implemented.
- Much faster than sifting mode decomposition methods and Wavelet decomposition
- Parameterless. Only required selecting no. of columns that'll be considered for overall Trend. Took same value(=3) for each dataset, did not require tedious parameter selection for each dataset, and ran faster in the end
- Captures overall shapes well, especially Ahmedabad Dengue, didn't capture details like EMD for instance.
- Variational Mode Decomposition performed mediocre even for individualized parameter
- EMD performed better than VMD but very time consuming run-time.
