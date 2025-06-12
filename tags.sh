#!/bin/bash

LANG_TAGS=".git/tags/lang"
DEPS_TAGS=".git/tags/deps"
PROJECT_TAGS=".git/tags/project"

echo "Generating project tags..."
ctags -e -R -f "$PROJECT_TAGS" --exclude=.git .

if [ ! -f "$EXTERNAL_TAGS" ]; then # only regenerate external if missing
    mkdir -p `dirname $LANG_TAGS`
    touch $LANG_TAGS
    
    echo "Generating external language tags..."
    if [ -f "go.mod" ]; then
        ctags -e -R -a -f "$LANG_TAGS" $(go env GOROOT)/src
    fi
    
    if [ -f "pom.xml" ] || [ -f "gradlew" ] || [ -f "project.clj" ]; then
        JDK_SRC="/usr/lib/jvm/java-17-openjdk/lib/src.zip"
        if [ -f "$JDK_SRC" ]; then
            ctags -e -R -a -f "$LANG_TAGS" "$JDK_SRC"  # -a to append
        fi
    fi
fi

# Only regenerate deps if missing or package files changed
# if [ ! -f "$DEPS_TAGS" ] || [ "go.mod" -nt "$DEPS_TAGS" ] || [ "pom.xml" -nt "$DEPS_TAGS" ]; then
    mkdir -p `dirname $DEPS_TAGS`
    rm $DEPS_TAGS # temporary while testing
    touch $DEPS_TAGS
    
    echo "Regenerating dependency tags..."
    if [ -f "go.mod" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" $(go env GOMODCACHE)
    fi
    
    if [ -f "pom.xml" ] || [ -f "project.clj" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" ~/.m2/repository
    fi

    if [ -f "build.gradle" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" ~/.gradle/caches/modules-2/files-2.1/
    fi
# fi

mkdir -p `dirname $PROJECT_TAGS`
ctags -e -R -f $PROJECT_TAGS --exclude=.git .

echo "Done! Use M-. to jump to definitions."
