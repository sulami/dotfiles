#!/bin/sh

get_project_version()
{
    CABAL_FILE=$(find . -iname '*.cabal')
    awk '/^version:/ {print $2}' ${CABAL_FILE}
}

get_project_name()
{
    CABAL_FILE=$(find . -iname '*.cabal')
    awk '/^name:/ {print $2}' ${CABAL_FILE}
}

get_project_name_version()
{
    echo "$(get_project_name)-$(get_project_version)"
}

get_snapshot_version()
{
    awk '/resolver:/ {print $2}' stack.yaml
}

download_snapshot_db()
{
    if [ ! -e stackage.hoo ]; then
        SNAPSHOT=$(get_snapshot_version)
        echo "Downloading the ${SNAPSHOT} Hoogle DB from Stackage"
        curl --progress-bar --location -o stackage.hoo "http://stackage.org/${SNAPSHOT}/db.hoo"
    fi
}

generate_haddock()
{
    echo "Building Haddock documentation"
    stack haddock
}

generate_hoogle()
{
    echo "Building local Hoogle DB"
    ARCH=$(uname -m)
    SNAPSHOT=$(get_snapshot_version)
    GHC_VERSION=$(stack exec ghc -- --numeric-version)
    PROJ_NAME_VERSION=$(get_project_name_version)
    PROJ_NAME=$(get_project_name)
    DOC_PATH="${PWD}/.stack-work/install/${ARCH}-linux/${SNAPSHOT}/${GHC_VERSION}/doc/${PROJ_NAME_VERSION}"
    TXT_PATH=".stack-work/install/${ARCH}-linux/${SNAPSHOT}/${GHC_VERSION}/doc/${PROJ_NAME_VERSION}/${PROJ_NAME}.txt"
    hoogle convert --haddock -d ${DOC_PATH} ${TXT_PATH} local.hoo
    hoogle combine -o default.hoo stackage.hoo local.hoo
}

main()
{
    download_snapshot_db
    generate_haddock
    generate_hoogle
}

main

