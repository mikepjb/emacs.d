#!/bin/bash

mkdir -p .tags
LANG_TAGS=".tags/lang"
DEPS_TAGS=".tags/deps"
PROJECT_TAGS=".tags/proj"
SOURCES_DIR="$HOME/.local/src"

echo "Generating project tags..."
ctags -e -R -f "$PROJECT_TAGS" --exclude=.git --exclude=node_modules .

if [ ! -f "$LANG_TAGS" ]; then
    touch "$LANG_TAGS"
    echo "Generating external language tags..."
    
    # Go
    if [ -f "go.mod" ] && [ -d "$SOURCES_DIR/go" ]; then
        ctags -e -R -a -f "$LANG_TAGS" \
            --exclude="*_test.go" --exclude="testdata" \
            "$SOURCES_DIR/go/src"
    fi
    
    # Java
    if ([ -f "pom.xml" ] || [ -f "gradlew" ] || [ -f "project.clj" ]) && [ -d "$SOURCES_DIR/java" ]; then
        ctags -e -R -a -f "$LANG_TAGS" "$SOURCES_DIR/java/src"
    fi
    
    # Clojure
    if [ -f "project.clj" ] && [ -d "$SOURCES_DIR/clojure" ]; then
        ctags -e -R -a -f "$LANG_TAGS" "$SOURCES_DIR/clojure/src"
    fi
    
    # Node.js
    if [ -f "package.json" ] && [ -d "$SOURCES_DIR/node" ]; then
        ctags -e -R -a -f "$LANG_TAGS" \
            --exclude="test" --exclude="deps" \
            "$SOURCES_DIR/node/lib"
    fi
fi

if [ ! -f "$DEPS_TAGS" ]; then
    echo "Regenerating dependency tags..."
    touch "$DEPS_TAGS"
    
    if [ -f "go.mod" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" --exclude="*_test.go" $(go env GOMODCACHE)
    fi
    
    if [ -f "package.json" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" \
            --exclude="*.min.js" --exclude="dist" --exclude="build" \
            node_modules
    fi
    
    if [ -f "pom.xml" ] || [ -f "project.clj" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" ~/.m2/repository
    fi
    
    if [ -f "build.gradle" ]; then
        ctags -e -R -a -f "$DEPS_TAGS" ~/.gradle/caches/modules-2/files-2.1/
    fi
fi

echo "Done! Use M-. to jump to definitions. Add '.tags/' to your .gitignore"
echo ""
echo "TAGS file sizes:"
for f in .tags/*; do
    if [ -f "$f" ]; then
        echo "  $(basename "$f"): $(du -h "$f" | cut -f1)"
    fi
done
