from snakemake.shell import shell

log = snakemake.log_fmt_shell(stdout=True, stderr=True)

shell(
    "(bwtool matrix "
    "{snakemake.params.extra} "
    "{snakemake.input.bed} "
    "{snakemake.input.bigwigs} "
    "{snakemake.output.matrix}) {log}"
)
