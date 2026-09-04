eval "$(conda shell.bash hook)"

# double deactivate is intentional
conda deactivate
conda deactivate 

echo $( which Rscript )
