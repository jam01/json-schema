# JSON Schema Validator for Scala 3

Validation of JSON-like structures with JSON Schemas through upickle's visitor framework.

## Usage
### Example with _ujson_
```scala 3
val sch: Schema = json_schema.from(ujson.Readable, ujson.Readable.fromString("""{"type": "string"}"""))
val validator: Visitor[?, OutputUnit] = json_schema.validator(sch)
val result: OutputUnit = ujson.Str("foo").transform(validator)
```

**Note:** _ujson_ is not a direct dependency of _json-schema_3_.

### Dependency
_sbt_
```scala 3
libraryDependencies += "io.github.jam01" % "json-schema_3" % "0.1.0"
```
_Mill_
```scala 3
ivy"io.github.jam01::json-schema_3::0.1.0"
```

_Maven_
```xml
<dependency>
    <groupId>io.github.jam01</groupId>
    <artifactId>json-schema_3</artifactId>
    <version>0.1.0</version>
</dependency>
```
