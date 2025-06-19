#!/bin/sh

set +e

user_tags_dir="$HOME/.tags"
user_src_dir="$HOME/src"
update="false" # update source code before generating tags

mkdir -p $user_tags_dir $user_src_dir

main() {
    project_root=$(find_project_root)
    echo "Generating tags in context: $project_root"
    
    case "${1:-all}" in
        "go")
            setup_go
            ;;
        "java")
            setup_java
            ;;
        "clojure")
            setup_clojure
            setup_java
            ;;
        "pull")
            update="true"
            setup_go
            setup_java
            setup_clojure
            ;;
        "all"|"")
            setup_go
            setup_java
            setup_clojure
            ;;
        *)
            echo "Usage: $0 [go|java|clojure|all]"
            exit 1
            ;;
    esac

    # generate local project file
    ctags -e -f $project_root/tags --exclude=node_modules -R .
}

find_project_root() {
    local dir="$PWD"
    local project_files=("Makefile" "go.mod" "package.json" "deps.edn" ".git")
    
    while [[ "$dir" != "/" ]]; do
        for file in "${project_files[@]}"; do
            if [[ -e "$dir/$file" ]]; then
                echo "$dir"
                return 0
            fi
        done
        dir="$(dirname "$dir")"
    done
    
    # Fallback to current directory
    echo "$PWD"
}

setup_go() {
    setup_language_tags \
	"go" "https://github.com/golang/go.git" "Go" "go.tags"
    setup_dependency_tags "go" "$HOME/go/pkg/mod" "Go" "go.dep.tags"
}

setup_java() {
    setup_language_tags \
	"java" "https://github.com/corretto/corretto-17.git" "Java" "java.tags"

    # Maven dependencies
    setup_dependency_tags "maven" "$HOME/.m2/repository" "Java" "maven.dep.tags"
    
    # Gradle dependencies (try multiple common locations)
    setup_dependency_tags "gradle" "$HOME/.gradle/caches/modules-2/files-2.1" "Java" "gradle.dep.tags"
    
    # Alternative Gradle location for newer versions
    if [[ ! -f "$user_tags_dir/gradle.dep.tags" ]]; then
        setup_dependency_tags "gradle-alt" "$HOME/.gradle/caches/jars-9" "Java" "gradle.dep.tags"
    fi
}

setup_clojure() {
    setup_language_tags \
	"clojure" \
	"https://github.com/clojure/clojure.git" "Clojure,Java" "clojure.tags"

    # Maven dependencies (Clojure uses Maven repos)
    setup_dependency_tags "clojure-maven" "$HOME/.m2/repository" "Clojure,Java" "clojure.maven.dep.tags"
}

setup_dependency_tags() {
    local name="$1"
    local path="$2"
    local ctags_languages="$3"
    local tag_file="$4"

    if [[ -d "$path" ]]; then
	ctags -e -f $user_tags_dir/$tag_file \
	      --languages="$ctags_languages" \
	      --exclude=*.class \
	      --exclude=*.jar \
	      --exclude=test \
	      --exclude=tests \
	      -R $path
    fi
}

setup_language_tags() {
    local name="$1"
    local git_url="$2"
    local ctags_languages="$3"
    local tag_file="$4"
    
    if command -v "$name" >/dev/null 2>&1; then
        local src_dir="$user_src_dir/$name"
        
        if [[ ! -d "$src_dir" ]]; then
            git clone --depth 1 "$git_url" "$src_dir"
        else
            if [[ "$update" = "true" ]]; then
                (cd "$src_dir" && git pull origin master)
            fi
        fi

	if [[ ! -f "$user_tags_dir/$tag_file" || "$update" = "true" ]]; then
	    echo "generating tags for $name"
            ctags -e --languages="$ctags_languages" \
		  -f "$user_tags_dir/$tag_file" -R "$src_dir"
	else
	    echo "language tags already generated for $name"
	fi
    fi
}

# -- Generate 3rd party/dep sources ----

# from places like.. .m2 for clojure, go has it's GOROOT I think?, node has
# node_modules locally and java has gradle area?

# we end up with tags per language again but for the user-level caches e.g .m2
# folder has ALL deps we use across all projects that I have downloaded

# -- Finally generate tags based on your code

# -- Result.. tags that can be applied for any of the languages I work with.

main "$@"
