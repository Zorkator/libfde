
set @setup=(                                                                                                  ^
    %[fetchArtifact]% --unzip --project grs/gramovis/libfde --job wheel.windows --ref gitlab.grs.de/main ^&^& ^
    pip install --find-links dist.windows fde                                                                 ^
)

%[activateEnviron]% --dont-check-pkg -- ^
    --pip "pytest"
