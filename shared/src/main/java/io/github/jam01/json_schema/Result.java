package io.github.jam01.json_schema;

import java.util.List;

public record Result(boolean valid,
                     List<?> errors,
                     List<?> annotations) {
}
