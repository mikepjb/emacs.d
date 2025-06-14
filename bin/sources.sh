#!/bin/bash

# Download language sources for ctags
SOURCES_DIR="$HOME/.local/src"
mkdir -p "$SOURCES_DIR"

echo "Downloading language sources to $SOURCES_DIR..."

# Go standard library
if [ ! -d "$SOURCES_DIR/go" ]; then
    echo "Downloading Go sources..."
    cd "$SOURCES_DIR"
    # Download Go source from GitHub (much faster than full repo)
    curl -L https://github.com/golang/go/archive/refs/heads/master.tar.gz | tar -xz
    mv go-master go
fi

# Java OpenJDK sources
if [ ! -d "$SOURCES_DIR/java" ]; then
    echo "Downloading Java OpenJDK sources..."
    cd "$SOURCES_DIR"
    # Download OpenJDK source (just the src directory)
    curl -L https://github.com/openjdk/jdk/archive/refs/heads/master.tar.gz | tar -xz
    mv jdk-master java
fi

# Clojure sources
if [ ! -d "$SOURCES_DIR/clojure" ]; then
    echo "Downloading Clojure sources..."
    cd "$SOURCES_DIR"
    git clone --depth 1 https://github.com/clojure/clojure.git
fi

# Node.js sources
if [ ! -d "$SOURCES_DIR/node" ]; then
    echo "Downloading Node.js sources..."
    cd "$SOURCES_DIR"
    git clone --depth 1 https://github.com/nodejs/node.git
fi

echo "Done! Sources in $SOURCES_DIR"
echo "Total size:"
du -sh "$SOURCES_DIR"
