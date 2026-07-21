
set @setup=(                                                                                                 ^
    %[fetchArtifact]% --unzip --project grs/gramovis/libfde --job core.windows --ref gitlab.grs.de/main ^&^& ^
    set "FDEPATH=%CD%\.build\package\Release\bin"                                                       ^&^& ^
    pip install -e .                                                                                         ^
)

%[activateEnviron]% --dont-check-pkg -- ^
    --pip "pytest"
