# Courtesy of https://gist.github.com/camtheman256/028aa2f1ced68cd435093a2d4680cf88
function fish_jj_prompt --description 'Write out the jj prompt'
    # Is jj installed?
    if not command -sq jj
        return 1
    end

    # Are we in a jj repo?
    if not jj root --quiet &>/dev/null
        return 1
    end

    # Generate prompt
    jj log --ignore-working-copy --no-graph --color always -r @ -T '
        surround(
            " (",
            ")",
            separate(
                " ",
                bookmarks.join(", "),
                coalesce(
                    surround(
                        "\"",
                        "\"",
                        if(
                            description.first_line().substr(0, 24).starts_with(description.first_line()),
                            description.first_line().substr(0, 24),
                            description.first_line().substr(0, 23) ++ "…"
                        )
                    ),
                    label(if(empty, "empty"), description_placeholder)
                ),
                change_id.shortest(),
                commit_id.shortest(),
                if(conflict, label("conflict", "(conflict)")),
                if(empty, label("empty", "(empty)")),
                if(divergent, "(divergent)"),
                if(hidden, "(hidden)"),
            )
        )
    '
end
